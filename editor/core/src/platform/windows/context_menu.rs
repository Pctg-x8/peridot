use windows::{
    UI::Composition::{
        CompositionEffectSourceParameter, Compositor, Desktop::DesktopWindowTarget, SpriteVisual,
    },
    Win32::{
        Foundation::{CloseHandle, FALSE, HANDLE, HINSTANCE, HWND, LPARAM, LRESULT, POINT, WPARAM},
        Graphics::{
            Direct3D::D3D_FEATURE_LEVEL_12_0,
            Direct3D12::{
                D3D12_COMMAND_LIST_TYPE_DIRECT, D3D12_COMMAND_QUEUE_DESC,
                D3D12_COMMAND_QUEUE_FLAG_NONE, D3D12_FENCE_FLAG_NONE, D3D12CreateDevice,
                D3D12GetDebugInterface, ID3D12CommandQueue, ID3D12Debug, ID3D12Device, ID3D12Fence,
            },
            Dxgi::{
                Common::{
                    DXGI_ALPHA_MODE_PREMULTIPLIED, DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_SAMPLE_DESC,
                },
                CreateDXGIFactory2, DXGI_CREATE_FACTORY_DEBUG, DXGI_SCALING_STRETCH,
                DXGI_SWAP_CHAIN_DESC1, DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL,
                DXGI_USAGE_RENDER_TARGET_OUTPUT, IDXGIFactory2, IDXGISwapChain3,
            },
            Gdi::PtInRect,
        },
        System::{
            Threading::{CreateEventW, GetCurrentThreadId},
            WinRT::Composition::{ICompositorDesktopInterop, ICompositorInterop},
        },
        UI::{
            HiDpi::GetDpiForWindow,
            WindowsAndMessaging::{
                CallNextHookEx, CreateWindowExW, DefWindowProcW, DestroyWindow, GetClientRect,
                GetCursorPos, GetWindowLongPtrW, GetWindowRect, HHOOK, SW_SHOWNOACTIVATE,
                SetWindowLongPtrW, SetWindowsHookExW, ShowWindow, UnhookWindowsHookEx, WH_MOUSE,
                WINDOW_LONG_PTR_INDEX, WM_LBUTTONDOWN, WM_MBUTTONDBLCLK, WNDCLASSEXW,
                WS_EX_NOACTIVATE, WS_EX_NOREDIRECTIONBITMAP, WS_EX_TOPMOST, WS_POPUP,
            },
        },
    },
};
use windows_core::{Interface, PCWSTR, h, w};
use windows_numerics::{Vector2, Vector3};

use crate::{
    Event, LogicFiberEventDispatcher, SyncEvent, SystemLink,
    bindgen::Microsoft::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    rendering::{
        NewContextMenuData, RenderMessage,
        composite::{
            AnimatableColor, AnimatableFloat, CompositeMode, CompositeRect, CompositeRectText,
            CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTree, CompositeTreeRef, Gradient,
            GradientRef,
        },
        text::FontID,
    },
    utils::{
        LogicalUnit, PixelsUnit, Point, Size,
        platform::windows::{WindowByClassIter, current_instance_handle},
    },
};

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Handle(HWND);
impl core::hash::Hash for Handle {
    #[inline(always)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.0.addr().hash(state)
    }
}
unsafe impl Sync for Handle {}
unsafe impl Send for Handle {}
impl Handle {
    #[deprecated]
    pub const fn internal(&self) -> HWND {
        self.0
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
}

pub struct InstanceState {
    composite_root: CompositeTreeRef,
    cv_root: SpriteVisual,
    c_target: DesktopWindowTarget,
}
impl InstanceState {
    fn done(self, composite_tree: &mut CompositeTree<SyncEvent>) {
        composite_tree.free_all(self.composite_root);
    }
}

const WINDOW_PTR_STATE: WINDOW_LONG_PTR_INDEX = WINDOW_LONG_PTR_INDEX(0);
const SHADOW_SIZE: f32 = 16.0;
const SHADOW_OFFSET: Vector3 = Vector3 {
    X: 0.0,
    Y: 4.0,
    Z: 0.0,
};

fn register_class(hinstance: HINSTANCE) -> u16 {
    unsafe {
        crate::utils::platform::windows::register_class(&WNDCLASSEXW {
            cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
            cbWndExtra: core::mem::size_of::<usize>() as _,
            lpfnWndProc: Some(wndproc),
            hInstance: hinstance,
            lpszClassName: w!("ContextMenu"),
            ..core::mem::MaybeUninit::zeroed().assume_init()
        })
        .expect("context_menu.register_class")
    }
}

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
    unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
}

pub struct SharedState {
    window_class: u16,
    installed_hook: HHOOK,
    rt_sender: std::sync::mpsc::Sender<RenderMessage>,
    event_dispatcher: LogicFiberEventDispatcher,
    pub dxgi_factory: IDXGIFactory2,
    pub d3d12_device: ID3D12Device,
    pub d3d12_cq: ID3D12CommandQueue,
    pub d3d12_present_fence: ID3D12Fence,
    pub d3d12_present_fence_event: HANDLE,
    compositor: Compositor,
    compositor_desktop_interop: ICompositorDesktopInterop,
    compositor_interop: ICompositorInterop,
    entry_light_grad: GradientRef,
}
impl Drop for SharedState {
    fn drop(&mut self) {
        if let Err(e) = unsafe { CloseHandle(self.d3d12_present_fence_event) } {
            tracing::error!(reason = %e, "CloseHandle");
        }

        if let Err(e) = unsafe { UnhookWindowsHookEx(self.installed_hook) } {
            tracing::error!(reason = %e, "UnhookWindowsHookEx");
        }
    }
}
impl SharedState {
    #[inline(always)]
    pub const fn get() -> &'static Self {
        unsafe { &*CONTEXT_MENU_SHARED_STATE }
    }

    // hiding by mouse hook: https://www.codeproject.com/Tips/751520/Custom-Context-Menu
    extern "system" fn mouse_hook(code: i32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
        if WM_LBUTTONDOWN as usize <= wparam.0 && wparam.0 <= WM_MBUTTONDBLCLK as usize {
            let mut p = core::mem::MaybeUninit::<POINT>::uninit();
            unsafe {
                GetCursorPos(p.as_mut_ptr()).expect("Failed to get cursor pos");
            }
            let p = unsafe { p.assume_init() };

            let has_pointing_menu = WindowByClassIter::new(PCWSTR(Self::get().window_class as _))
                .any(|x| {
                    let mut w = core::mem::MaybeUninit::uninit();
                    if let Err(e) = unsafe { GetWindowRect(x, w.as_mut_ptr()) } {
                        tracing::error!(reason = %e, "GetWindowRect");
                        return false;
                    }

                    unsafe { PtInRect(w.as_ptr(), p).as_bool() }
                });

            if !has_pointing_menu {
                Self::get()
                    .event_dispatcher
                    .dispatch(Event::ContextMenuCloseAll);
            }
        }

        unsafe { CallNextHookEx(None, code, wparam, lparam) }
    }
}

static mut CONTEXT_MENU_SHARED_STATE: *mut SharedState = core::ptr::null_mut();

pub fn initialize(
    app_context: &super::ApplicationContext,
    rt_sender: std::sync::mpsc::Sender<RenderMessage>,
) {
    let window_class = register_class(current_instance_handle());
    let installed_hook = unsafe {
        SetWindowsHookExW(
            WH_MOUSE,
            Some(SharedState::mouse_hook),
            None,
            GetCurrentThreadId(),
        )
        .expect("SetWindowsHookExW")
    };

    let mut d3d12_debug = core::mem::MaybeUninit::uninit();
    unsafe {
        D3D12GetDebugInterface(d3d12_debug.as_mut_ptr()).expect("D3D12GetDebugInterface");
    }
    let d3d12_debug: ID3D12Debug = unsafe {
        d3d12_debug
            .assume_init()
            .expect("D3D12GetDebugInterface.null")
    };
    unsafe {
        d3d12_debug.EnableDebugLayer();
    }

    let dxgi_factory: IDXGIFactory2 =
        unsafe { CreateDXGIFactory2(DXGI_CREATE_FACTORY_DEBUG).expect("CreateDXGIFactory2") };
    let adapter = unsafe {
        dxgi_factory
            .EnumAdapters1(0)
            .expect("dxgi_factory.EnumAdapters1")
    };
    let mut d3d12_device = core::mem::MaybeUninit::uninit();
    unsafe {
        D3D12CreateDevice(&adapter, D3D_FEATURE_LEVEL_12_0, d3d12_device.as_mut_ptr())
            .expect("D3D12CreateDevice")
    };
    let d3d12_device: ID3D12Device =
        unsafe { d3d12_device.assume_init().expect("D3D12CreateDevice.null") };
    let d3d12_cq: ID3D12CommandQueue = unsafe {
        d3d12_device
            .CreateCommandQueue(&D3D12_COMMAND_QUEUE_DESC {
                Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
                Priority: 0,
                Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
                NodeMask: 0,
            })
            .expect("d3d12_device.CreateCommandQueue")
    };
    unsafe {
        d3d12_cq
            .SetName(w!("D3D12 Main Command Queue"))
            .expect("d3d12_cq.SetName");
    }
    let d3d12_present_fence: ID3D12Fence = unsafe {
        d3d12_device
            .CreateFence(0, D3D12_FENCE_FLAG_NONE)
            .expect("d3d12_device.CreateFence")
    };
    let d3d12_present_fence_event =
        unsafe { CreateEventW(None, false, false, None).expect("CreateEvent") };

    unsafe {
        CONTEXT_MENU_SHARED_STATE = Box::into_raw(Box::new(SharedState {
            window_class,
            installed_hook,
            rt_sender,
            dxgi_factory,
            d3d12_device,
            d3d12_cq,
            d3d12_present_fence,
            d3d12_present_fence_event,
            compositor: app_context.native_compositor.clone(),
            compositor_desktop_interop: app_context
                .native_compositor
                .cast()
                .expect("native_compositor.cast"),
            compositor_interop: app_context
                .native_compositor
                .cast()
                .expect("native_compositor.cast"),
            // あとで初期化するので一旦適当に埋める
            #[allow(invalid_value)]
            event_dispatcher: core::mem::MaybeUninit::uninit().assume_init(),
            #[allow(invalid_value)]
            entry_light_grad: core::mem::MaybeUninit::uninit().assume_init(),
        }));
    }
}

pub fn initialize_composite_resources(composite_tree: &mut CompositeTree<SyncEvent>) {
    unsafe {
        (*CONTEXT_MENU_SHARED_STATE).entry_light_grad =
            composite_tree.create_gradient(Gradient::Radial {
                start_color: [0.75, 1.0, 1.5, 1.0],
                end_color: [0.25, 0.5, 1.0, 0.0],
                center_relative: [0.5, 0.9],
                radius: [0.5, 0.1],
            });
    }
}

pub fn post_initialize(event_dispatcher: LogicFiberEventDispatcher) {
    unsafe {
        (*CONTEXT_MENU_SHARED_STATE).event_dispatcher = event_dispatcher;
    }
}

pub fn pop(
    syslink: &SystemLink,
    composite_tree: &mut CompositeTree<SyncEvent>,
    screen_pos: Point<PixelsUnit>,
) {
    let shared_state = SharedState::get();

    let hinstance = current_instance_handle();
    let h = unsafe {
        CreateWindowExW(
            WS_EX_NOACTIVATE | WS_EX_TOPMOST | WS_EX_NOREDIRECTIONBITMAP,
            PCWSTR(shared_state.window_class as _),
            w!(""),
            WS_POPUP,
            screen_pos.x - SHADOW_SIZE.ceil() as i32,
            screen_pos.y - SHADOW_SIZE.ceil() as i32,
            100 + (SHADOW_SIZE * 2.0).ceil() as i32,
            100 + (SHADOW_SIZE * 2.0).ceil() as i32,
            None,
            None,
            Some(hinstance),
            None,
        )
        .expect("context_menu.create_window")
    };
    let composite_root = composite_tree.create(CompositeRect {
        relative_size_adjustment: [1.0, 1.0],
        has_bitmap: true,
        composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.375])),
        ..Default::default()
    });
    let c_target = unsafe {
        shared_state
            .compositor_desktop_interop
            .CreateDesktopWindowTarget(h, true)
            .expect("compositor_desktop_interop.CreateDesktopWindowTarget")
    };
    let cv_root = shared_state
        .compositor
        .CreateSpriteVisual()
        .expect("compositor.CreateSpriteVisual");
    c_target.SetRoot(&cv_root).expect("c_target.SetRoot");

    let swapchain = unsafe {
        shared_state
            .dxgi_factory
            .CreateSwapChainForComposition(
                &shared_state.d3d12_cq,
                &DXGI_SWAP_CHAIN_DESC1 {
                    Width: 100,
                    Height: 100,
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
    let effect_factory = shared_state
        .compositor
        .CreateEffectFactory(&fx)
        .expect("drag.fx.create_factory");
    let backdrop_brush = shared_state
        .compositor
        .CreateBackdropBrush()
        .expect("drag.backdrop_brush.create");
    let blur_brush = effect_factory.CreateBrush().expect("drag.fx_brush.create");
    blur_brush
        .SetSourceParameter(h!("Source"), &backdrop_brush)
        .expect("drag.fx.set_blur_source");
    cv_root
        .SetSize(Vector2 { X: 100.0, Y: 100.0 })
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
            let x = shared_state
                .compositor
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
    let cv_composited = shared_state
        .compositor
        .CreateSpriteVisual()
        .expect("drag.visual.color_tint.create");
    cv_composited
        .SetBrush(
            &shared_state
                .compositor
                .CreateSurfaceBrushWithSurface(&unsafe {
                    shared_state
                        .compositor_interop
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

    set_state(
        h,
        Box::new(InstanceState {
            composite_root,
            cv_root,
            c_target,
        }),
    );
    syslink
        .rt_sender
        .send(RenderMessage::NewContextMenu(NewContextMenuData {
            w: Handle(h),
            // composition_surface_handle: SendableCompositionSurfaceHandle(dcomp_surface_handle),
            swapchain,
            composite_root,
        }))
        .expect("rt_sender.send");

    let _ = unsafe { ShowWindow(h, SW_SHOWNOACTIVATE) };

    let ct_entry = composite_tree.create(CompositeRect {
        base_scale_factor: Handle(h).render_scale(),
        relative_size_adjustment: [1.0, 0.0],
        size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(20.0)],
        text: Some(CompositeRectText {
            runs: vec![CompositeRectTextRun {
                font_id: FontID::UIDefault,
                content: "Entry1".into(),
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            }],
            horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
            vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
            ..Default::default()
        }),
        ..Default::default()
    });
    let ct_entry_light = composite_tree.create(CompositeRect {
        base_scale_factor: Handle(h).render_scale(),
        relative_size_adjustment: [1.0, 0.0],
        size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(20.0)],
        has_bitmap: true,
        composite_mode: CompositeMode::FillRadialGradient(shared_state.entry_light_grad),
        ..Default::default()
    });
    composite_tree.add_child(composite_root, ct_entry_light);
    composite_tree.add_child(composite_root, ct_entry);
}

pub fn close_all(composite_tree: &mut CompositeTree<SyncEvent>) {
    let window_handles =
        WindowByClassIter::new(PCWSTR(SharedState::get().window_class as _)).collect::<Vec<_>>();
    for window_handle in window_handles {
        let (tx, rx) = std::sync::mpsc::channel::<()>();
        SharedState::get()
            .rt_sender
            .send(RenderMessage::DestroyContextMenu(Handle(window_handle), tx))
            .expect("rt_sender.send");
        rx.recv().expect("rx.recv");

        take_state(window_handle).done(composite_tree);

        if let Err(e) = unsafe { DestroyWindow(window_handle) } {
            tracing::error!(reason = %e, "DestroyWindow");
        }
    }
}

pub fn finalize() {
    unsafe {
        drop(Box::from_raw(core::ptr::replace(
            core::ptr::addr_of_mut!(CONTEXT_MENU_SHARED_STATE),
            core::ptr::null_mut(),
        )));
    }
}
