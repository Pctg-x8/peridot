use std::rc::Rc;

use windows::{
    UI::Composition::{CompositionEffectSourceParameter, Desktop::DesktopWindowTarget},
    Win32::{
        Foundation::{FALSE, HWND, LPARAM, LRESULT, POINT, WPARAM},
        Graphics::{
            Direct3D12::ID3D12CommandQueue,
            Dxgi::{
                Common::{
                    DXGI_ALPHA_MODE_PREMULTIPLIED, DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_SAMPLE_DESC,
                },
                DXGI_SCALING_STRETCH, DXGI_SWAP_CHAIN_DESC1, DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL,
                DXGI_USAGE_RENDER_TARGET_OUTPUT, IDXGIFactory2, IDXGISwapChain3,
            },
            Gdi::MapWindowPoints,
        },
        UI::{
            Controls::WM_MOUSELEAVE,
            HiDpi::GetDpiForWindow,
            Input::KeyboardAndMouse::{TME_LEAVE, TME_NONCLIENT, TRACKMOUSEEVENT, TrackMouseEvent},
            WindowsAndMessaging::{
                CreateWindowExW, DefWindowProcW, DestroyWindow, GetClientRect, GetCursorPos,
                GetWindowLongPtrW, HTBOTTOM, HTBOTTOMLEFT, HTBOTTOMRIGHT, HTCAPTION, HTCLIENT,
                HTCLOSE, HTLEFT, HTMAXBUTTON, HTMINBUTTON, HTNOWHERE, HTRIGHT, HTTOP, HTTOPLEFT,
                HTTOPRIGHT, HTTRANSPARENT, KillTimer, MA_NOACTIVATE, SW_SHOWNOACTIVATE, SetTimer,
                SetWindowLongPtrW, ShowWindow, WINDOW_LONG_PTR_INDEX, WM_LBUTTONDOWN, WM_LBUTTONUP,
                WM_MOUSEACTIVATE, WM_MOUSEMOVE, WM_NCHITTEST, WM_NCLBUTTONDOWN, WM_NCLBUTTONUP,
                WM_NCMOUSELEAVE, WM_NCMOUSEMOVE, WM_NCRBUTTONDOWN, WM_RBUTTONDOWN, WM_RBUTTONUP,
                WNDCLASSEXW, WS_EX_NOACTIVATE, WS_EX_NOREDIRECTIONBITMAP, WS_EX_TOPMOST, WS_POPUP,
                WindowFromPoint,
            },
        },
    },
};
use windows_core::{Interface, PCWSTR, h, w};
use windows_numerics::{Vector2, Vector3};

use crate::{
    Event, LogicFiberEventDispatcher, SystemLink, WindowHandle,
    bindgen::Microsoft::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    input::{
        KeyboardFocusGroupRef, KeyboardFocusTokenRegistry, PerWindowKeyboardFocusState,
        hittest::{HitTestTreeData, HitTestTreeManager, HitTestTreeRef, PointerButton},
    },
    platform::windows::ApplicationContext,
    rendering::{
        NewContextMenuData, RenderMessage,
        composite::{
            AnimatableColor, CompositeMode, CompositeRect, CompositeTree, CompositeTreeRef,
        },
    },
    uikit::{
        MenuBaseSurfaceEventHandler, MenuItemLayout, MenuItemSubMenuView, MenuItemView,
        MountTarget, ViewInitContext,
    },
    utils::{
        LogicalUnit, PixelsUnit, Point, Size,
        platform::windows::{WindowByClassIter, register_class},
    },
};

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Handle(HWND);
impl core::hash::Hash for Handle {
    #[inline(always)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.0.addr().hash(state)
    }
}
unsafe impl Sync for Handle {}
unsafe impl Send for Handle {}
impl MountTarget for Handle {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        state(self.0).composite_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        state(self.0).ht_root
    }
}
impl Handle {
    pub fn close<E>(
        self,
        syslink: &SystemLink,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        let (tx, rx) = std::sync::mpsc::channel::<()>();
        syslink
            .rt_sender
            .send(RenderMessage::DestroyContextMenu(self, tx))
            .expect("rt_sender.send");
        rx.recv().expect("rx.recv");

        take_state(self.0).done(composite_tree, ht_manager, keyboard_focus_registry);

        if let Err(e) = unsafe { DestroyWindow(self.0) } {
            tracing::error!(reason = %e, "DestroyWindow");
        }
    }

    #[inline(always)]
    pub fn pixels_size(&self) -> Size<PixelsUnit> {
        let mut r = core::mem::MaybeUninit::uninit();
        unsafe {
            GetClientRect(self.0, r.as_mut_ptr()).expect("GetClientRect");
        }
        let r = unsafe { r.assume_init_ref() };

        Size::new_pixels(
            r.right as u32 - (SHADOW_SIZE * 2.0).ceil() as u32,
            r.bottom as u32 - (SHADOW_SIZE * 2.0).ceil() as u32,
        )
    }

    #[inline(always)]
    pub fn logical_size(&self) -> Size<LogicalUnit> {
        self.pixels_size().to_logical(self.render_scale())
    }

    #[inline(always)]
    pub fn render_scale(&self) -> f32 {
        unsafe { GetDpiForWindow(self.0) as f32 / 96.0 }
    }

    #[inline(always)]
    pub fn ht_root(&self) -> HitTestTreeRef {
        state(self.0).ht_root
    }

    #[inline(always)]
    pub fn keyboard_focus_state(&self) -> &PerWindowKeyboardFocusState {
        &state(self.0).keyboard_focus_state
    }

    #[inline(always)]
    pub fn keyboard_focus_state_mut(&mut self) -> &mut PerWindowKeyboardFocusState {
        &mut state_mut(self.0).keyboard_focus_state
    }

    #[inline(always)]
    pub fn submenu_pop_position(&self, view: &MenuItemSubMenuView) -> Point<LogicalUnit> {
        let mut window_rect = core::mem::MaybeUninit::uninit();
        unsafe {
            GetClientRect(self.0, window_rect.as_mut_ptr()).expect("GetClientRect");
        }
        let window_rect = unsafe { window_rect.assume_init() };

        Point::new_pixels(
            window_rect.right - (SHADOW_SIZE * 2.0).round() as i32,
            window_rect.top + (view.placement_y * self.render_scale()).round() as i32,
        )
        .to_logical(unsafe { GetDpiForWindow(self.0) as f32 / 96.0 })
        .with_offset(state(self.0).spawned_surface_pos)
    }
}

pub struct InstanceState {
    composite_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    event_dispatcher: *const LogicFiberEventDispatcher,
    _c_target: DesktopWindowTarget,
    keyboard_focus_state: PerWindowKeyboardFocusState,
    kf_root_group: KeyboardFocusGroupRef,
    spawned_surface_pos: Point<LogicalUnit>,
    pointer_focus: bool,
}
impl InstanceState {
    fn done<E>(
        self,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        composite_tree.free_all(self.composite_root);
        ht_manager.free_all(self.ht_root);
        keyboard_focus_registry.release_group(self.kf_root_group);
    }

    #[inline(always)]
    fn dispatch_event(&self, event: Event) {
        unsafe { &*self.event_dispatcher }.dispatch(event);
    }
}

const WINDOW_PTR_STATE: WINDOW_LONG_PTR_INDEX = WINDOW_LONG_PTR_INDEX(0);
const SHADOW_SIZE: f32 = 16.0;
const SHADOW_OFFSET: Vector3 = Vector3 {
    X: 0.0,
    Y: 4.0,
    Z: 0.0,
};

#[inline(always)]
fn set_state(hwnd: HWND, state: Box<InstanceState>) {
    unsafe {
        SetWindowLongPtrW(
            hwnd,
            WINDOW_PTR_STATE,
            Box::into_raw(state).addr().cast_signed(),
        );
    }
}

fn take_state(hwnd: HWND) -> Box<InstanceState> {
    let r = unsafe {
        Box::from_raw(core::ptr::with_exposed_provenance_mut(
            GetWindowLongPtrW(hwnd, WINDOW_PTR_STATE).cast_unsigned(),
        ))
    };
    unsafe {
        SetWindowLongPtrW(hwnd, WINDOW_PTR_STATE, 0);
    }

    r
}

#[inline(always)]
fn state<'a>(hwnd: HWND) -> &'a InstanceState {
    unsafe {
        &*core::ptr::with_exposed_provenance(
            GetWindowLongPtrW(hwnd, WINDOW_PTR_STATE).cast_unsigned(),
        )
    }
}

#[inline(always)]
fn state_mut<'a>(hwnd: HWND) -> &'a mut InstanceState {
    unsafe {
        &mut *core::ptr::with_exposed_provenance_mut(
            GetWindowLongPtrW(hwnd, WINDOW_PTR_STATE).cast_unsigned(),
        )
    }
}

extern "system" fn wndproc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    #[inline(always)]
    const fn is_application_handled_hittest(ht: u32) -> bool {
        ht == HTCLOSE || ht == HTMAXBUTTON || ht == HTMINBUTTON
    }

    if msg == WM_MOUSEACTIVATE {
        return LRESULT(MA_NOACTIVATE as _);
    }

    if msg == WM_NCHITTEST {
        /*let Some(state) = state_maybe(hwnd) else {
            // 初期化完了前にきた
            return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
        };*/

        let mut p = [Point::new_pixels(
            (lparam.0 & 0xffff) as i16 as _,
            ((lparam.0 >> 16) & 0xffff) as i16 as _,
        )
        .to_win32()];
        unsafe {
            MapWindowPoints(None, Some(hwnd), &mut p);
        }
        let client_pos = Point::from_win32(p[0]);

        let mut client_size = core::mem::MaybeUninit::uninit();
        unsafe {
            GetClientRect(hwnd, client_size.as_mut_ptr()).expect("getclientsize");
        }
        let client_size = unsafe { client_size.assume_init() };

        if 0 > client_pos.x
            || client_pos.x > client_size.right
            || 0 > client_pos.y
            || client_pos.y > client_size.bottom
        {
            // ウィンドウ範囲外
            return LRESULT(HTNOWHERE as _);
        }

        if SHADOW_SIZE.ceil() as i32 > client_pos.x
            || client_pos.x > client_size.right - SHADOW_SIZE.ceil() as i32
            || SHADOW_SIZE.ceil() as i32 > client_pos.y
            || client_pos.y > client_size.bottom - SHADOW_SIZE.ceil() as i32
        {
            // 影エリア
            return LRESULT(HTTRANSPARENT as _);
        }

        // TODO: 一旦全部クライアントエリアとして判定する
        // あとでHitTestTreeのroleの対応が必要になったときに変える
        return LRESULT(HTCLIENT as _);
        /*let Some(result) = state.non_client_hittest(
            hwnd,
            Point::new_pixels(
                (lparam.0 & 0xffff) as i16 as _,
                ((lparam.0 >> 16) & 0xffff) as i16 as _,
            ),
        ) else {
            // よくわからん(アプリウィンドウ範囲外)のでデフォルトに任せる
            return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
        };

        return LRESULT(result as _);*/
    }

    if (msg == WM_NCMOUSEMOVE || msg == WM_NCLBUTTONDOWN || msg == WM_NCLBUTTONUP)
        && (wparam.0 == HTTOP as _
            || wparam.0 == HTBOTTOM as _
            || wparam.0 == HTLEFT as _
            || wparam.0 == HTRIGHT as _
            || wparam.0 == HTTOPLEFT as _
            || wparam.0 == HTTOPRIGHT as _
            || wparam.0 == HTBOTTOMLEFT as _
            || wparam.0 == HTBOTTOMRIGHT as _)
    {
        // リサイズ境界上の処理はシステムにおまかせ
        return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
    }

    if (msg == WM_NCLBUTTONDOWN || msg == WM_NCLBUTTONUP) && wparam.0 == HTCAPTION as _ {
        // TitleBarの挙動はシステムにおまかせ
        return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
    }

    if msg == WM_LBUTTONDOWN {
        // move then down
        state(hwnd).dispatch_event(Event::ContextMenuPointerMove {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            client_pos: Point::new_pixels(
                (lparam.0 & 0xffff) as i16 as _,
                ((lparam.0 >> 16) & 0xffff) as i16 as _,
            )
            .to_logical(Handle(hwnd).render_scale()),
        });
        state(hwnd).dispatch_event(Event::ContextMenuPointerDown {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            button: PointerButton::Primary,
        });

        return LRESULT(0);
    }

    if msg == WM_NCLBUTTONDOWN && is_application_handled_hittest(wparam.0 as _) {
        // アプリケーションでハンドリングするNonClientエリア
        // NonClientイベントはスクリーン座標で来る
        let mut p = [POINT {
            x: (lparam.0 & 0xffff) as i16 as _,
            y: ((lparam.0 >> 16) & 0xffff) as i16 as _,
        }];
        unsafe {
            MapWindowPoints(None, Some(hwnd), &mut p);
        }

        // move then down
        state(hwnd).dispatch_event(Event::ContextMenuPointerMove {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            client_pos: Point::new_pixels(p[0].x, p[0].y).to_logical(Handle(hwnd).render_scale()),
        });
        state(hwnd).dispatch_event(Event::ContextMenuPointerDown {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            button: PointerButton::Primary,
        });

        return LRESULT(0);
    }

    if msg == WM_LBUTTONUP
        || (msg == WM_NCLBUTTONUP && is_application_handled_hittest(wparam.0 as _))
    {
        state(hwnd).dispatch_event(Event::ContextMenuPointerUp {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            button: PointerButton::Primary,
        });
        return LRESULT(0);
    }

    if msg == WM_RBUTTONDOWN {
        // move then down
        state(hwnd).dispatch_event(Event::ContextMenuPointerMove {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            client_pos: Point::new_pixels(
                (lparam.0 & 0xffff) as i16 as _,
                ((lparam.0 >> 16) & 0xffff) as i16 as _,
            )
            .to_logical(Handle(hwnd).render_scale()),
        });
        state(hwnd).dispatch_event(Event::ContextMenuPointerDown {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            button: PointerButton::Secondary,
        });

        return LRESULT(0);
    }

    if msg == WM_NCRBUTTONDOWN && is_application_handled_hittest(wparam.0 as _) {
        // アプリケーションでハンドリングするNonClientエリア
        // NonClientイベントはスクリーン座標で来る
        let mut p = [POINT {
            x: (lparam.0 & 0xffff) as i16 as _,
            y: ((lparam.0 >> 16) & 0xffff) as i16 as _,
        }];
        unsafe {
            MapWindowPoints(None, Some(hwnd), &mut p);
        }

        // move then down
        state(hwnd).dispatch_event(Event::ContextMenuPointerMove {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            client_pos: Point::new_pixels(p[0].x, p[0].y).to_logical(Handle(hwnd).render_scale()),
        });
        state(hwnd).dispatch_event(Event::ContextMenuPointerDown {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            button: PointerButton::Secondary,
        });

        return LRESULT(0);
    }

    if msg == WM_RBUTTONUP
        || (msg == WM_NCLBUTTONUP && is_application_handled_hittest(wparam.0 as _))
    {
        state(hwnd).dispatch_event(Event::ContextMenuPointerUp {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            button: PointerButton::Secondary,
        });
        return LRESULT(0);
    }

    if msg == WM_MOUSEMOVE {
        unsafe {
            TrackMouseEvent(&mut TRACKMOUSEEVENT {
                cbSize: core::mem::size_of::<TRACKMOUSEEVENT>() as _,
                dwFlags: TME_LEAVE,
                hwndTrack: hwnd,
                dwHoverTime: 0,
            })
            .expect("TrackMouseEvent");
        }

        state_mut(hwnd).pointer_focus = true;

        state(hwnd).dispatch_event(Event::ContextMenuPointerMove {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            client_pos: Point::new_pixels(
                (lparam.0 & 0xffff) as i16 as _,
                ((lparam.0 >> 16) & 0xffff) as i16 as _,
            )
            .to_logical(Handle(hwnd).render_scale()),
        });

        return LRESULT(0);
    }

    if msg == WM_NCMOUSEMOVE {
        unsafe {
            TrackMouseEvent(&mut TRACKMOUSEEVENT {
                cbSize: core::mem::size_of::<TRACKMOUSEEVENT>() as _,
                dwFlags: TME_LEAVE | TME_NONCLIENT,
                hwndTrack: hwnd,
                dwHoverTime: 0,
            })
            .expect("TrackMouseEvent");
        }

        state_mut(hwnd).pointer_focus = true;

        // NonClientイベントはスクリーン座標で来る
        let mut p = [POINT {
            x: (lparam.0 & 0xffff) as i16 as _,
            y: ((lparam.0 >> 16) & 0xffff) as i16 as _,
        }];
        unsafe {
            MapWindowPoints(None, Some(hwnd), &mut p);
        }

        state(hwnd).dispatch_event(Event::ContextMenuPointerMove {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
            client_pos: Point::new_pixels(p[0].x, p[0].y).to_logical(Handle(hwnd).render_scale()),
        });
        // Note: NCMOUSEMOVEはデフォルト動作もさせる
    }

    if msg == WM_MOUSELEAVE || msg == WM_NCMOUSELEAVE {
        // let st = state(hwnd);

        // if st.destroying {
        //     // closing
        //     return LRESULT(0);
        // }

        if state(hwnd).pointer_focus {
            state_mut(hwnd).pointer_focus = false;
            // leaveしたのでdeselectも発行
            // state(hwnd)
            //     .event_dispatcher
            //     .dispatch(Event::ContextMenuDeselectItem {
            //         depth: state(hwnd).depth,
            //     });
        }

        state(hwnd).dispatch_event(Event::ContextMenuPointerLeave {
            target: Handle(hwnd),
            pointer_id: super::PointerID(),
        });

        return LRESULT(0);
    }

    unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
}

pub struct SharedState {
    window_class: u16,
    dxgi_factory: IDXGIFactory2,
    d3d12_cq: ID3D12CommandQueue,
    delayed_action_timer_id: *mut usize,
}
impl SharedState {
    pub(super) const CLASS_NAME: PCWSTR = w!("ContextMenu");

    pub fn new(
        app_context: &ApplicationContext,
        dx_context: &super::DxContext,
        delayed_action_timer_id: core::pin::Pin<&mut usize>,
    ) -> Self {
        let window_class = unsafe {
            register_class(&WNDCLASSEXW {
                cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
                cbWndExtra: core::mem::size_of::<usize>() as _,
                lpfnWndProc: Some(wndproc),
                hInstance: app_context.hinstance,
                lpszClassName: Self::CLASS_NAME,
                ..core::mem::MaybeUninit::zeroed().assume_init()
            })
            .expect("context_menu.register_class")
        };

        Self {
            window_class,
            dxgi_factory: dx_context.dxgi_factory.clone(),
            d3d12_cq: dx_context.d3d12_cq.clone(),
            delayed_action_timer_id: delayed_action_timer_id.get_mut(),
        }
    }

    #[inline(always)]
    const fn window_class(&self) -> PCWSTR {
        PCWSTR(self.window_class as _)
    }

    pub fn reserve_delayed_action(&self) {
        unsafe {
            *self.delayed_action_timer_id =
                SetTimer(None, *self.delayed_action_timer_id, 400, None);
        }
    }

    pub fn unreserve_delayed_action(&self) {
        let active_timer_id = unsafe { self.delayed_action_timer_id.replace(0) };
        if active_timer_id != 0 {
            unsafe { KillTimer(None, active_timer_id).expect("KillTimer") };
        }
    }
}

impl super::SystemLink<'_> {
    #[tracing::instrument(skip(self, parent, composite_tree, ht_manager, keyboard_focus_registry))]
    pub fn new_flyout_surface<E>(
        &self,
        parent: WindowHandle,
        pos: Point<LogicalUnit>,
        size: Size<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) -> Handle {
        let render_scale = parent.ui_scale_factor();
        let mut ps = [pos.to_pixels_round(render_scale).to_win32()];
        unsafe {
            MapWindowPoints(Some(parent.0), None, &mut ps);
        }
        let screen_pos = Point::from_win32(ps[0]);
        let pixels_size = size.to_pixels_ceil(render_scale);

        tracing::debug!(?screen_pos, ?pixels_size, "new_flyout_surface");

        let h = unsafe {
            // Note: 子ウィンドウにしちゃうとcropされちゃうので独立させる
            CreateWindowExW(
                WS_EX_NOACTIVATE | WS_EX_TOPMOST | WS_EX_NOREDIRECTIONBITMAP,
                self.flyout_surface_context.window_class(),
                w!(""),
                WS_POPUP,
                screen_pos.x - SHADOW_SIZE.ceil() as i32,
                screen_pos.y - SHADOW_SIZE.ceil() as i32,
                pixels_size.width as i32 + (SHADOW_SIZE * 2.0).ceil() as i32,
                pixels_size.height as i32 + (SHADOW_SIZE * 2.0).ceil() as i32,
                Some(parent.0),
                None,
                Some((&*self.app_context_ptr).hinstance),
                None,
            )
            .expect("context_menu.create_window")
        };
        let composite_root = composite_tree.create(CompositeRect {
            relative_size_adjustment: [1.0, 1.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                0.0, 0.0, 0.0, 0.375,
            ])),
            ..Default::default()
        });
        let ht_root = ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            // shadowの分を開ける
            // TODO: RenderScaleが変わる場合は追従する必要がある
            left: SHADOW_SIZE / Handle(h).render_scale(),
            top: SHADOW_SIZE / Handle(h).render_scale(),
            // width, heightはいじらなくていい（渡される基本サイズがすでに影を抜いた分になっている）
            ..Default::default()
        });
        let c_target = unsafe {
            (*self.app_context_ptr)
                .native_compositor_desktop_interop
                .CreateDesktopWindowTarget(h, true)
                .expect("compositor_desktop_interop.CreateDesktopWindowTarget")
        };
        let cv_root = unsafe { &*self.app_context_ptr }
            .native_compositor
            .CreateSpriteVisual()
            .expect("compositor.CreateSpriteVisual");
        c_target.SetRoot(&cv_root).expect("c_target.SetRoot");

        let swapchain = unsafe {
            self.flyout_surface_context
                .dxgi_factory
                .CreateSwapChainForComposition(
                    &self.flyout_surface_context.d3d12_cq,
                    &DXGI_SWAP_CHAIN_DESC1 {
                        Width: pixels_size.width as _,
                        Height: pixels_size.height as _,
                        Format: DXGI_FORMAT_B8G8R8A8_UNORM,
                        SampleDesc: DXGI_SAMPLE_DESC {
                            Count: 1,
                            Quality: 0,
                        },
                        BufferCount: 2,
                        BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
                        Stereo: FALSE,
                        Scaling: DXGI_SCALING_STRETCH,
                        SwapEffect: DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL,
                        AlphaMode: DXGI_ALPHA_MODE_PREMULTIPLIED,
                        Flags: 0,
                    },
                    None,
                )
                .expect("dxgi_factory.CreateSwapChainForComposition")
        };
        let swapchain: IDXGISwapChain3 = swapchain.cast().expect("swapchain.cast");
        let fx = GaussianBlurEffect::new().expect("drag.fx.create");
        fx.SetSource(
            &CompositionEffectSourceParameter::Create(h!("source"))
                .expect("compositioneffectsourceparameter.create"),
        )
        .expect("drag.fx.set_source");
        fx.SetBlurAmount(16.0).expect("drag.fx.set_blur_amount");
        fx.SetOptimization(EffectOptimization::Speed)
            .expect("drag.fx.set_optimization");
        let effect_factory = unsafe { &*self.app_context_ptr }
            .native_compositor
            .CreateEffectFactory(&fx)
            .expect("drag.fx.create_factory");
        let backdrop_brush = unsafe { &*self.app_context_ptr }
            .native_compositor
            .CreateBackdropBrush()
            .expect("drag.backdrop_brush.create");
        let blur_brush = effect_factory.CreateBrush().expect("drag.fx_brush.create");
        blur_brush
            .SetSourceParameter(h!("Source"), &backdrop_brush)
            .expect("drag.fx.set_blur_source");
        cv_root
            .SetSize(Vector2 {
                X: pixels_size.width as _,
                Y: pixels_size.height as _,
            })
            .expect("cv_root.SetSize");
        cv_root
            .SetCenterPoint(Vector3::new(0.5, 0.5, 0.5))
            .expect("drag.visual.blur.set_center_point");
        cv_root
            .SetAnchorPoint(Vector2::new(0.5, 0.5))
            .expect("drag.visual.blur.set_anchor_point");
        cv_root
            .SetRelativeOffsetAdjustment(Vector3::new(0.5, 0.5, 0.0))
            .expect("drag.visual.blur.set_relative_offset_adjustment");
        cv_root
            .SetBrush(&blur_brush)
            .expect("drag.visual.blur.set_brush");
        cv_root
            .SetShadow(&{
                let x = unsafe { &*self.app_context_ptr }
                    .native_compositor
                    .CreateDropShadow()
                    .expect("drag.visual.shadow.create");
                x.SetBlurRadius(SHADOW_SIZE)
                    .expect("context_menu.shadow.set_blur_radius");
                x.SetOffset(SHADOW_OFFSET)
                    .expect("context_menu.shadow.set_offset");
                x.SetOpacity(0.3).expect("context_menu.shadow.set_opacity");
                x
            })
            .expect("drag.visual.set_shadow");
        let cv_composited = unsafe { &*self.app_context_ptr }
            .native_compositor
            .CreateSpriteVisual()
            .expect("drag.visual.color_tint.create");
        cv_composited
            .SetBrush(
                &unsafe { &*self.app_context_ptr }
                    .native_compositor
                    .CreateSurfaceBrushWithSurface(&unsafe {
                        (*self.app_context_ptr)
                            .native_compositor_interop
                            .CreateCompositionSurfaceForSwapChain(&swapchain)
                            .expect("compositor_interop.CreateCompositionSurfaceForSwapChain")
                    })
                    .expect("compositor.CreateSurfaceBrushWithSurface"),
            )
            .expect("cv_root.SetBrush");
        cv_composited
            .SetRelativeOffsetAdjustment(Vector3::zero())
            .expect("drag.visual.color_tint.set_relative_offset_adjustment");
        cv_composited
            .SetRelativeSizeAdjustment(Vector2::one())
            .expect("drag.visual.color_tint.set_relative_size_adjustment");
        cv_root
            .Children()
            .expect("drag.visual.get_children")
            .InsertAtTop(&cv_composited)
            .expect("drag.visual.add_child");

        let root_kf_group = keyboard_focus_registry.acquire_group();
        set_state(
            h,
            Box::new(InstanceState {
                composite_root,
                ht_root,
                event_dispatcher: self.event_dispatcher,
                _c_target: c_target,
                keyboard_focus_state: PerWindowKeyboardFocusState::new(root_kf_group),
                kf_root_group: root_kf_group,
                spawned_surface_pos: pos,
                pointer_focus: false,
            }),
        );
        self.rt_sender
            .send(RenderMessage::NewContextMenu(NewContextMenuData {
                w: Handle(h),
                // composition_surface_handle: SendableCompositionSurfaceHandle(dcomp_surface_handle),
                swapchain,
                composite_root,
            }))
            .expect("rt_sender.send");

        let _ = unsafe { ShowWindow(h, SW_SHOWNOACTIVATE) };
        Handle(h)
    }

    pub fn pop_context_menu(
        &self,
        parent: WindowHandle,
        view_init_context: &mut ViewInitContext,
        depth: usize,
        surface_pos: Point<LogicalUnit>,
        layouted_items: impl FnOnce(f32) -> Vec<MenuItemLayout>,
        setup_contents: impl FnOnce(
            Vec<MenuItemLayout>,
            Handle,
            &mut ViewInitContext,
        ) -> Vec<MenuItemView>,
    ) -> (Handle, Rc<MenuBaseSurfaceEventHandler>, Vec<MenuItemView>) {
        let render_scale = parent.ui_scale_factor();
        let layouted_items = layouted_items(render_scale);
        let width = MenuItemLayout::min_width(layouted_items.iter());
        let height = MenuItemLayout::height(layouted_items.iter());

        let h = self.new_flyout_surface(
            parent,
            surface_pos,
            Size::new_logical(width.value(), height.value()),
            view_init_context.mount_context.composite_tree,
            view_init_context.mount_context.ht_manager,
            view_init_context.mount_context.keyboard_focus_registry,
        );

        let base_surface_event_handler = Rc::new(MenuBaseSurfaceEventHandler::new(depth));
        view_init_context
            .ht_manager
            .set_action_handler(h.ht_root(), &base_surface_event_handler);
        let views = setup_contents(layouted_items, h, view_init_context);

        (h, base_surface_event_handler, views)
    }

    pub fn any_pointer_on_context_menu(&self) -> bool {
        let mut p = core::mem::MaybeUninit::<POINT>::uninit();
        unsafe {
            GetCursorPos(p.as_mut_ptr()).expect("Failed to get cursor pos");
        }
        let p = unsafe { p.assume_init() };

        let w_pointing = unsafe { WindowFromPoint(p) };
        WindowByClassIter::new(self.flyout_surface_context.window_class())
            .any(|x| x.expect("FindWindowExW failed") == w_pointing)
    }

    pub fn any_pointer_on_dropdown_menu(&self) -> bool {
        // TODO: ContextMenuと区別できてない（でも区別する必要もないか？）
        let mut p = core::mem::MaybeUninit::<POINT>::uninit();
        unsafe {
            GetCursorPos(p.as_mut_ptr()).expect("Failed to get cursor pos");
        }
        let p = unsafe { p.assume_init() };

        let w_pointing = unsafe { WindowFromPoint(p) };
        WindowByClassIter::new(self.flyout_surface_context.window_class())
            .any(|x| x.expect("FindWindowExW failed") == w_pointing)
    }
}
