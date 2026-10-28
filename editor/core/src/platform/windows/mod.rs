use core::{cell::Cell, pin::Pin};

use shared::{LogicalUnit, PixelsUnit, Point, Rect};
use windows::{
    System::DispatcherQueueController,
    UI::{
        Composition::{
            CompositionEffectSourceParameter, Compositor, Desktop::DesktopWindowTarget,
            SpriteVisual,
        },
        Text::Core::CoreTextServicesManager,
    },
    Win32::{
        Foundation::{HANDLE, HINSTANCE, HWND, LPARAM, LRESULT, POINT, WAIT_OBJECT_0, WPARAM},
        Graphics::{
            Direct3D::D3D_FEATURE_LEVEL_12_0,
            Direct3D12::{
                D3D12_COMMAND_LIST_TYPE_DIRECT, D3D12_COMMAND_QUEUE_DESC,
                D3D12_COMMAND_QUEUE_FLAG_NONE, D3D12CreateDevice, D3D12GetDebugInterface,
                ID3D12CommandQueue, ID3D12Debug, ID3D12Device,
            },
            Dxgi::{CreateDXGIFactory2, DXGI_CREATE_FACTORY_DEBUG, IDXGIFactory2},
            Gdi::{HBRUSH, MapWindowPoints},
        },
        System::{
            Com::IDataObject,
            Ole::{DROPEFFECT_MOVE, DoDragDrop, IDropSource, OleInitialize, OleUninitialize},
            Threading::INFINITE,
            WinRT::{
                Composition::{ICompositorDesktopInterop, ICompositorInterop},
                CreateDispatcherQueueController, DQTAT_COM_ASTA, DQTYPE_THREAD_CURRENT,
                DispatcherQueueOptions,
            },
        },
        UI::{
            HiDpi::GetDpiForWindow,
            Input::{
                KeyboardAndMouse::{
                    VK_0, VK_9, VK_A, VK_BACK, VK_CONTROL, VK_DELETE, VK_DOWN, VK_LCONTROL,
                    VK_LEFT, VK_LMENU, VK_LSHIFT, VK_LWIN, VK_MENU, VK_RCONTROL, VK_RETURN,
                    VK_RIGHT, VK_RMENU, VK_RSHIFT, VK_RWIN, VK_SHIFT, VK_UP, VK_Z,
                },
                RAWINPUTDEVICE, RAWINPUTDEVICE_FLAGS, RegisterRawInputDevices,
            },
            WindowsAndMessaging::{
                CreateWindowExW, DefWindowProcW, DestroyWindow, DispatchMessageW, GCW_ATOM,
                GetClassLongPtrW, GetCursorPos, HCURSOR, HICON, IDC_ARROW, IDC_HAND, IDC_IBEAM,
                IDC_SIZENS, IDC_SIZEWE, LoadCursorW, MWMO_INPUTAVAILABLE,
                MsgWaitForMultipleObjectsEx, PM_REMOVE, PeekMessageW, QS_ALLEVENTS, SW_HIDE,
                SW_SHOW, SW_SHOWNOACTIVATE, SW_SHOWNORMAL, SWP_NOACTIVATE, SWP_NOZORDER, SetCursor,
                SetCursorPos, SetWindowPos, ShowWindow, TranslateMessage, WM_QUIT, WNDCLASS_STYLES,
                WNDCLASSEXW, WS_EX_LAYERED, WS_EX_NOACTIVATE, WS_EX_NOREDIRECTIONBITMAP,
                WS_EX_TOPMOST, WS_EX_TRANSPARENT, WS_POPUP,
            },
        },
    },
};
use windows_core::{Interface, PCWSTR, h, w};
use windows_monitor::{
    EnumerateDisplayMonitorContinuous, enumerate_display_monitors, primary_monitor,
};
use windows_numerics::{Vector2, Vector3};
use windows_waitable_timer::WaitableTimer;

use crate::{
    CoreLoop, CoreLoopRunningContextFlags, MainWindowOpenMode, SubWindowOpenMode, WindowType,
    bindgen::Microsoft::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    graphics::Graphics,
    input::{KeyInputCode, ModifierKey, PointerInputUnit, hittest::CursorShape},
    persistence::WindowGeometryState,
    platform::{
        WindowHandleBase,
        windows::drag_drop::{DockPaneDataObject, DockPaneDropSource, DropTarget},
    },
    rendering::{RenderMessage, RenderMessageSender, text::FontSet},
    ui::dock::DockingPreviewState,
    utils::platform::windows::{
        current_instance_handle, point_from_win32, point_to_win32, register_class,
        windows_native_color,
    },
};

mod drag_drop;
pub mod flyout_surface;
mod text_input;
mod toplevel;

pub use drag_drop::DragData;
pub use text_input::*;
pub use toplevel::*;

pub type WindowPersistentStateNativeGeometryUnit = PixelsUnit;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PointerID();
impl PointerID {
    #[inline(always)]
    pub fn set_position(&self, screen_pos: Point<PixelsUnit>) {
        if let Err(e) = unsafe { SetCursorPos(screen_pos.x, screen_pos.y) } {
            tracing::error!(reason = %e, "set_cursor_pos");
        }
    }
}

pub(self) struct WindowClassSet {
    main: u16,
}
impl WindowClassSet {
    fn register(hinstance: HINSTANCE) -> Self {
        let main = NativeWindow::register_class(hinstance);

        Self { main }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub(self) struct ModifierKeyRecorder {
    pub current: ModifierKey,
}
impl ModifierKeyRecorder {
    pub const fn new() -> Self {
        Self {
            current: ModifierKey::empty(),
        }
    }

    pub fn handle_keydown(&mut self, code: usize) {
        if code == VK_SHIFT.0 as _ || code == VK_LSHIFT.0 as _ || code == VK_RSHIFT.0 as _ {
            self.current |= ModifierKey::SHIFT;
        }
        if code == VK_MENU.0 as _ || code == VK_LMENU.0 as _ || code == VK_RMENU.0 as _ {
            self.current |= ModifierKey::ALT;
        }
        if code == VK_CONTROL.0 as _ || code == VK_LCONTROL.0 as _ || code == VK_RCONTROL.0 as _ {
            self.current |= ModifierKey::CONTROL;
        }
        if code == VK_LWIN.0 as _ || code == VK_RWIN.0 as _ {
            self.current |= ModifierKey::SUPER;
        }
    }

    pub fn handle_keyup(&mut self, code: usize) {
        if code == VK_SHIFT.0 as _ || code == VK_LSHIFT.0 as _ || code == VK_RSHIFT.0 as _ {
            self.current &= !ModifierKey::SHIFT;
        }
        if code == VK_MENU.0 as _ || code == VK_LMENU.0 as _ || code == VK_RMENU.0 as _ {
            self.current &= !ModifierKey::ALT;
        }
        if code == VK_CONTROL.0 as _ || code == VK_LCONTROL.0 as _ || code == VK_RCONTROL.0 as _ {
            self.current &= !ModifierKey::CONTROL;
        }
        if code == VK_LWIN.0 as _ || code == VK_RWIN.0 as _ {
            self.current &= !ModifierKey::SUPER;
        }
    }
}

pub(self) fn translate_keycode(code: usize) -> KeyInputCode {
    match code {
        v if v == VK_LEFT.0 as _ => KeyInputCode::LeftArrow,
        v if v == VK_RIGHT.0 as _ => KeyInputCode::RightArrow,
        v if v == VK_UP.0 as _ => KeyInputCode::UpArrow,
        v if v == VK_DOWN.0 as _ => KeyInputCode::DownArrow,
        v if v == VK_RETURN.0 as _ => KeyInputCode::Enter,
        v if v == VK_DELETE.0 as _ => KeyInputCode::Delete,
        v if v == VK_BACK.0 as _ => KeyInputCode::Backspace,
        v if v == VK_LSHIFT.0 as _ => KeyInputCode::LeftShift,
        v if v == VK_RSHIFT.0 as _ => KeyInputCode::RightShift,
        v if v == VK_LCONTROL.0 as _ => KeyInputCode::LeftControl,
        v if v == VK_RCONTROL.0 as _ => KeyInputCode::RightControl,
        v if v == VK_LMENU.0 as _ => KeyInputCode::LeftAlt,
        v if v == VK_RMENU.0 as _ => KeyInputCode::RightAlt,
        v if v == VK_LWIN.0 as _ => KeyInputCode::LeftSuper,
        v if v == VK_RWIN.0 as _ => KeyInputCode::RightSuper,
        // 左右判定のないキーコード いったんLeft扱い
        v if v == VK_SHIFT.0 as _ => KeyInputCode::LeftShift,
        v if v == VK_CONTROL.0 as _ => KeyInputCode::LeftControl,
        v if v == VK_MENU.0 as _ => KeyInputCode::LeftAlt,
        // character
        v if (VK_0.0 as usize <= v && v <= VK_9.0 as usize)
            || (VK_A.0 as usize <= v && v <= VK_Z.0 as usize) =>
        {
            KeyInputCode::Character(v as u8 as _)
        }
        // fallback
        _ => KeyInputCode::UnknownNativeCode(code as _),
    }
}

pub struct DragPreviewPopover {
    w: HWND,
    _composition_target: DesktopWindowTarget,
    root_visual: SpriteVisual,
}
impl Drop for DragPreviewPopover {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = unsafe { DestroyWindow(self.w) } {
            tracing::error!(reason = %e, "dragPreviewPopover.destroyNative");
        }
    }
}
impl DragPreviewPopover {
    fn new(hinstance: HINSTANCE, native_compositor: &Compositor) -> Self {
        let atom_drag_floating = unsafe {
            register_class(&WNDCLASSEXW {
                cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
                style: WNDCLASS_STYLES(0),
                cbClsExtra: 0,
                cbWndExtra: 0,
                lpfnWndProc: Some(Self::wndproc),
                hInstance: hinstance,
                hIcon: HICON(core::ptr::null_mut()),
                hCursor: HCURSOR(core::ptr::null_mut()),
                hbrBackground: HBRUSH(core::ptr::null_mut()),
                lpszMenuName: PCWSTR::null(),
                lpszClassName: w!("DragFloatingWindow"),
                hIconSm: HICON(core::ptr::null_mut()),
            })
            .expect("register_class.drag")
        };
        let w = unsafe {
            CreateWindowExW(
                WS_EX_TRANSPARENT
                    | WS_EX_LAYERED
                    | WS_EX_NOACTIVATE
                    | WS_EX_TOPMOST
                    | WS_EX_NOREDIRECTIONBITMAP,
                PCWSTR(core::ptr::without_provenance(atom_drag_floating as _)),
                w!(""),
                WS_POPUP,
                100,
                100,
                128,
                128,
                None,
                None,
                Some(hinstance),
                None,
            )
            .expect("CreateWindowExW")
        };

        let fx = GaussianBlurEffect::new().expect("drag.fx.create");
        fx.SetSource(
            &CompositionEffectSourceParameter::Create(h!("source"))
                .expect("compositioneffectsourceparameter.create"),
        )
        .expect("drag.fx.set_source");
        fx.SetBlurAmount(16.0).expect("drag.fx.set_blur_amount");
        fx.SetOptimization(EffectOptimization::Speed)
            .expect("drag.fx.set_optimization");
        let effect_factory = native_compositor
            .CreateEffectFactory(&fx)
            .expect("drag.fx.create_factory");
        let backdrop_brush = native_compositor
            .CreateBackdropBrush()
            .expect("drag.backdrop_brush.create");
        let blur_brush = effect_factory.CreateBrush().expect("drag.fx_brush.create");
        blur_brush
            .SetSourceParameter(h!("Source"), &backdrop_brush)
            .expect("drag.fx.set_blur_source");
        let blur_visual = native_compositor
            .CreateSpriteVisual()
            .expect("drag.visual.blur.create");
        blur_visual
            .SetCenterPoint(Vector3::new(0.5, 0.5, 0.5))
            .expect("drag.visual.blur.set_center_point");
        blur_visual
            .SetAnchorPoint(Vector2::new(0.5, 0.5))
            .expect("drag.visual.blur.set_anchor_point");
        blur_visual
            .SetRelativeOffsetAdjustment(Vector3::new(0.5, 0.5, 0.0))
            .expect("drag.visual.blur.set_relative_offset_adjustment");
        blur_visual
            .SetBrush(&blur_brush)
            .expect("drag.visual.blur.set_brush");
        blur_visual
            .SetShadow(&{
                let x = native_compositor
                    .CreateDropShadow()
                    .expect("drag.visual.shadow.create");
                x.SetBlurRadius(32.0)
                    .expect("drag.visual.shadow.set_blur_radius");
                x.SetOffset(Vector3::new(0.0, 16.0, 0.0))
                    .expect("drag.visual.shadow.set_offset");
                x.SetOpacity(0.3).expect("drag.visual.shadow.set_opacity");
                x
            })
            .expect("drag.visual.set_shadow");
        let color_tint_visual = native_compositor
            .CreateSpriteVisual()
            .expect("drag.visual.color_tint.create");
        color_tint_visual
            .SetBrush(
                &native_compositor
                    .CreateColorBrushWithColor(windows_native_color(
                        &crate::DRAG_PREVIEW_POPOVER_BG_COLOR,
                    ))
                    .expect("drag.visual.color_tint.brush.create"),
            )
            .expect("drag.visual.color_tint.set_brush");
        color_tint_visual
            .SetRelativeOffsetAdjustment(Vector3::zero())
            .expect("drag.visual.color_tint.set_relative_offset_adjustment");
        color_tint_visual
            .SetRelativeSizeAdjustment(Vector2::one())
            .expect("drag.visual.color_tint.set_relative_size_adjustment");
        blur_visual
            .Children()
            .expect("drag.visual.get_children")
            .InsertAtTop(&color_tint_visual)
            .expect("drag.visual.add_child");

        let composition_target = unsafe {
            native_compositor
                .cast::<ICompositorDesktopInterop>()
                .expect("native_compositor.cast.desktop_interop")
                .CreateDesktopWindowTarget(w, true)
                .expect("drag.composition_target.create")
        };
        composition_target
            .SetRoot(&blur_visual)
            .expect("drag.visual.set_root");
        blur_visual
            .SetSize(Vector2::new(128.0 - 32.0, 128.0 - 32.0))
            .expect("drag.visual.set_size");

        Self {
            w,
            _composition_target: composition_target,
            root_visual: blur_visual,
        }
    }

    fn show_screen_coord(&self, rect: &Rect<PixelsUnit>) {
        unsafe {
            // 影のぶんだけ余分に設定する
            SetWindowPos(
                self.w,
                None,
                rect.left - 32,
                rect.top - 32,
                (rect.width + 64) as _,
                (rect.height + 64) as _,
                SWP_NOZORDER | SWP_NOACTIVATE,
            )
            .expect("setwindowpos");
            self.root_visual
                .SetSize(Vector2::new(rect.width as _, rect.height as _))
                .expect("drag.visual.set_size");
            let _ = ShowWindow(self.w, SW_SHOWNOACTIVATE);
        }
    }

    fn show(&self, base: HWND, rect: &Rect<LogicalUnit>) {
        unsafe {
            // デスクトップ座標で指定になるので置き換え
            let scale = GetDpiForWindow(base) as f32 / 96.0;
            let pos = rect.left_top().to_pixels_round(scale);
            let size = rect.size().to_pixels_ceil(scale);
            let mut p = [POINT { x: pos.x, y: pos.y }];
            MapWindowPoints(Some(base), None, &mut p);
            let [POINT { x, y }] = p;

            self.show_screen_coord(&Rect::from_lt_size(Point::new_pixels(x, y), size));
        }
    }

    fn set_rect_screen_coord(&self, rect: &Rect<PixelsUnit>) {
        // 影のぶんだけずらして設定する
        unsafe {
            SetWindowPos(
                self.w,
                None,
                rect.left - 32,
                rect.top - 32,
                (rect.width + 32 * 2) as _,
                (rect.height + 32 * 2) as _,
                SWP_NOZORDER | SWP_NOACTIVATE,
            )
            .expect("setwindowpos");
        }
        self.root_visual
            .SetSize(Vector2::new(rect.width as _, rect.height as _))
            .expect("drag.visual.set_size");
    }

    fn set_rect(&self, base: HWND, rect: &Rect<PointerInputUnit>) {
        // デスクトップ座標で指定になるので置き換え
        let scale = unsafe { GetDpiForWindow(base) } as f32 / 96.0;
        let pos = rect.left_top().to_pixels_round(scale);
        let size = rect.size().to_pixels_ceil(scale);
        let mut p = [POINT { x: pos.x, y: pos.y }];
        unsafe {
            MapWindowPoints(Some(base), None, &mut p);
        }
        let [POINT { x, y }] = p;

        self.set_rect_screen_coord(&Rect::from_lt_size(Point::new_pixels(x, y), size));
    }

    fn hide(&self) {
        unsafe {
            let _ = ShowWindow(self.w, SW_HIDE);
        }
    }

    extern "system" fn wndproc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
        unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
    }
}

pub struct ApplicationContext {
    hinstance: HINSTANCE,
    wc_set: WindowClassSet,
    _dispatcher_queue: DispatcherQueueController,
    native_compositor: Compositor,
    native_compositor_desktop_interop: ICompositorDesktopInterop,
    native_compositor_interop: ICompositorInterop,
    ctm: CoreTextServicesManager,
    drag_preview_popover: DragPreviewPopover,
    pane_dragging: Cell<bool>,
}
impl Drop for ApplicationContext {
    fn drop(&mut self) {
        unsafe {
            OleUninitialize();
        }
    }
}
impl ApplicationContext {
    pub fn new() -> Self {
        unsafe {
            OleInitialize(None).expect("win32.ole.initialize");
        }

        // required for winrt functionalities
        let dispatcher_queue = unsafe {
            CreateDispatcherQueueController(DispatcherQueueOptions {
                dwSize: core::mem::size_of::<DispatcherQueueOptions>() as _,
                threadType: DQTYPE_THREAD_CURRENT,
                apartmentType: DQTAT_COM_ASTA,
            })
            .expect("dispatchqueuecontroller.create")
        };

        let hinstance = current_instance_handle();
        let wc_set = WindowClassSet::register(hinstance);
        let native_compositor = Compositor::new().expect("win.compositor.create");
        let ctm =
            CoreTextServicesManager::GetForCurrentView().expect("coretextservicesmanager.get");

        unsafe {
            RegisterRawInputDevices(
                &[RAWINPUTDEVICE {
                    // keyboard
                    usUsagePage: 0x01,
                    usUsage: 0x06,
                    dwFlags: RAWINPUTDEVICE_FLAGS(0),
                    hwndTarget: HWND(core::ptr::null_mut()),
                }],
                size_of::<RAWINPUTDEVICE>() as _,
            )
            .expect("RegisterRawInputDevices")
        }

        Self {
            drag_preview_popover: DragPreviewPopover::new(hinstance, &native_compositor),
            hinstance,
            wc_set,
            _dispatcher_queue: dispatcher_queue,
            native_compositor_desktop_interop: native_compositor
                .cast()
                .expect("native_compositor.cast"),
            native_compositor_interop: native_compositor.cast().expect("native_compositor.cast"),
            native_compositor,
            ctm,
            pane_dragging: Cell::new(false),
        }
    }
}

pub struct DxContext {
    pub dxgi_factory: IDXGIFactory2,
    pub d3d12_device: ID3D12Device,
    pub d3d12_cq: ID3D12CommandQueue,
}
impl DxContext {
    pub fn new() -> Self {
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

        Self {
            dxgi_factory,
            d3d12_device,
            d3d12_cq,
        }
    }
}

fn find_maximized_base_left_top(monitor_index: usize) -> Option<Point<PixelsUnit>> {
    let primary_monitor = primary_monitor();
    let mut target_left_top = None;
    let mut primary_left_top = None;
    let mut enum_index = 0;
    enumerate_display_monitors(|hm, r| {
        if hm == primary_monitor {
            primary_left_top = Some(Point::new_pixels(r.left, r.top));
        }

        if enum_index == monitor_index {
            target_left_top = Some(Point::new_pixels(r.left, r.top));
        }

        enum_index += 1;
        EnumerateDisplayMonitorContinuous::Continue
    });
    if primary_left_top.is_none() {
        tracing::warn!("no primary monitor enumerated?");
    }

    target_left_top.or(primary_left_top)
}

pub fn create_main_window<'sys>(
    mode: MainWindowOpenMode,
    mut coreloop: Pin<&mut CoreLoop<'sys>>,
) -> WindowHandle {
    let (pos, size, initial_maximized);
    match mode {
        MainWindowOpenMode::Restore(WindowGeometryState::Maximized { monitor_index }) => {
            pos = find_maximized_base_left_top(monitor_index);
            size = None;
            initial_maximized = true;
        }
        MainWindowOpenMode::Restore(WindowGeometryState::Restored { rect }) => {
            pos = None;
            size = Some(rect.size());
            initial_maximized = false;
        }
        MainWindowOpenMode::New => {
            pos = None;
            size = None;
            initial_maximized = false;
        }
    }

    NativeWindow::new(
        WindowType::Main {},
        pos,
        size,
        initial_maximized,
        coreloop.as_mut(),
    )
    .make_handle()
}

pub fn open_sub_window<'sys>(
    mode: SubWindowOpenMode,
    mut coreloop: Pin<&mut CoreLoop<'sys>>,
    setup_contents: impl FnOnce(WindowHandle, Pin<&mut CoreLoop<'sys>>),
) -> WindowHandle {
    let (initial_pos, initial_size, initial_maximized);
    match mode {
        SubWindowOpenMode::Restore(WindowGeometryState::Maximized { monitor_index }) => {
            initial_pos = find_maximized_base_left_top(monitor_index);
            initial_size = None;
            initial_maximized = true;
        }
        SubWindowOpenMode::Restore(WindowGeometryState::Restored { rect }) => {
            initial_pos = Some(rect.left_top());
            initial_size = Some(rect.size());
            initial_maximized = false;
        }
        SubWindowOpenMode::DockDiverge {
            rect,
            position_ref_window,
        } => {
            let mut grect_lt = [point_to_win32(
                &rect
                    .left_top()
                    .to_pixels_round(position_ref_window.ui_scale_factor()),
            )];
            unsafe {
                MapWindowPoints(Some(position_ref_window.0), None, &mut grect_lt);
            }

            initial_pos = Some(point_from_win32(grect_lt[0]));
            initial_size = Some(
                rect.size()
                    .to_pixels_ceil(position_ref_window.ui_scale_factor()),
            );
            initial_maximized = false;
        }
    };

    let w = NativeWindow::new(
        WindowType::Sub,
        initial_pos,
        initial_size,
        initial_maximized,
        coreloop.as_mut(),
    );
    let h = w.make_handle();

    setup_contents(h, coreloop);
    unsafe {
        let _ = ShowWindow(h.0, SW_SHOW);
    }
    h
}

pub fn close_sub_window(mut window_handle: WindowHandle) {
    let (done_event_sender, done_event_receiver) = std::sync::mpsc::channel();
    WindowEventHandler::get_for_window(window_handle.0)
        .coreloop()
        .syslink
        .rt_sender
        .send(RenderMessage::DestroyWindow(
            window_handle,
            done_event_sender,
        ))
        .expect("rt_sender.send.destroy_window");
    done_event_receiver
        .recv()
        .expect("done_event_receiver.recv");

    let mut coreloop = WindowEventHandler::get_for_window(window_handle.0).coreloop();
    unsafe { coreloop.as_mut().get_unchecked_mut() }
        .composite_tree
        .free_all(window_handle.state().composite_root);
    unsafe { coreloop.as_mut().get_unchecked_mut() }
        .ht_manager
        .free_all(window_handle.state().ht_root);
    unsafe { coreloop.as_mut().get_unchecked_mut() }
        .keyboard_focus_registry
        .release_group(window_handle.state().keyboard_focus_state.root_group());
    window_handle.destroy();
}

pub struct SystemLink<'sys> {
    pub gfx: &'sys Graphics,
    pub font_set: FontSet,
    pub rt_sender: RenderMessageSender,
    pub app_context: &'sys ApplicationContext,
    pub pointer_hovering_timer_handle: HANDLE,
    pub flyout_surface_context: flyout_surface::SharedState,
    pub native_text_input_context_create_context: NativeTextInputContextCreateContext,
}
impl<'sys> SystemLink<'sys> {
    #[inline(always)]
    pub fn rt_sender(&self) -> &RenderMessageSender {
        &self.rt_sender
    }

    #[inline(always)]
    pub const fn font_set(&self) -> &FontSet {
        &self.font_set
    }

    #[inline(always)]
    pub const fn needs_app_menu_in_surface(&self) -> bool {
        // Windowsは常に必要
        true
    }

    pub fn prelaunch(&self, handle: WindowHandle) {
        unsafe {
            let _ = ShowWindow(handle.0, SW_SHOWNORMAL);
        }
    }

    pub fn set_cursor(&self, _pointer_id: &PointerID, cursor: CursorShape) {
        unsafe {
            // TODO: 必要そうならキャッシュする
            SetCursor(match cursor {
                CursorShape::Default => {
                    Some(LoadCursorW(None, IDC_ARROW).expect("load_cursor.default"))
                }
                CursorShape::Pointer => {
                    Some(LoadCursorW(None, IDC_HAND).expect("load_cursor.pointer"))
                }
                CursorShape::IBeam => {
                    Some(LoadCursorW(None, IDC_IBEAM).expect("load_cursor.ibeam"))
                }
                CursorShape::ResizeHorizontal => {
                    Some(LoadCursorW(None, IDC_SIZEWE).expect("load_cursor.resize_horizontal"))
                }
                CursorShape::ResizeVertical => {
                    Some(LoadCursorW(None, IDC_SIZENS).expect("load_cursor.resize_vertical"))
                }
            });
        }
    }

    pub fn set_pointer_hovering_timeout(&mut self) {
        unsafe { WaitableTimer::ref_from_handle(&self.pointer_hovering_timer_handle) }
            .set_oneshot_relative(crate::input::POINTER_HOVER_TIMEOUT_MS as _)
            .expect("pointer_hovering_timer.set");
    }

    pub fn kill_pointer_hovering_timeout(&mut self) {
        unsafe { WaitableTimer::ref_from_handle(&self.pointer_hovering_timer_handle) }
            .cancel()
            .expect("pointer_hovering_timer.cancel");
    }

    pub fn begin_pane_drag(
        &self,
        initiator_surface: WindowHandle,
        _pointer: &PointerID,
        rect: &Rect<LogicalUnit>,
        state: DockingPreviewState,
    ) {
        let mut coreloop = WindowEventHandler::get_for_window(initiator_surface.0).coreloop();

        self.app_context
            .drag_preview_popover
            .show(initiator_surface.0, rect);
        let mut effect = core::mem::MaybeUninit::uninit();
        let rel = coreloop.as_mut().exclusive_ht_unuse_tmp();
        let drop_source = IDropSource::from(DockPaneDropSource {
            app_context: self.app_context,
            source_rect: Rect::from_lt_size(state.offset, rect.size()),
            active_dpi_scale: Cell::new(initiator_surface.ui_scale_factor()),
            dest_window: Cell::new(initiator_surface.0),
        });
        unsafe { coreloop.as_mut().get_unchecked_mut() }.docking_preview_state = Some(state);
        unsafe {
            DoDragDrop(
                &IDataObject::from(DockPaneDataObject {}),
                &drop_source,
                DROPEFFECT_MOVE,
                effect.as_mut_ptr(),
            )
            .ok()
            .expect("win32.do_drag_drop");
        }
        unsafe { coreloop.as_mut().get_unchecked_mut() }
            .running_context_flags
            .insert(CoreLoopRunningContextFlags::DISABLE_SYNC_VIEW);
        let dest_window = drop_source
            .cast_object::<DockPaneDropSource>()
            .expect("invalid reference")
            .dest_window
            .get();
        if dest_window.is_invalid()
            || unsafe { GetClassLongPtrW(dest_window, GCW_ATOM) }
                != self.app_context.wc_set.main as usize
        {
            let mut p = [POINT { x: 0, y: 0 }];
            unsafe {
                GetCursorPos(&mut p[0]).expect("getcursorpos");
            }
            unsafe {
                MapWindowPoints(None, Some(initiator_surface.0), &mut p);
            }
            let [p] = p;

            coreloop.as_mut().confirm_redock(
                initiator_surface,
                point_from_win32(p).to_logical(initiator_surface.ui_scale_factor()),
            );
        } else {
            let mut p = [POINT { x: 0, y: 0 }];
            unsafe {
                GetCursorPos(&mut p[0]).expect("getcursorpos");
            }
            unsafe {
                MapWindowPoints(None, Some(dest_window), &mut p);
            }
            let [p] = p;

            coreloop.as_mut().confirm_redock(
                WindowHandle(dest_window),
                point_from_win32(p).to_logical(WindowHandle(dest_window).ui_scale_factor()),
            );
        }
        unsafe { coreloop.as_mut().get_unchecked_mut() }
            .running_context_flags
            .remove(CoreLoopRunningContextFlags::DISABLE_SYNC_VIEW);
        coreloop.as_mut().update_view_all();

        drop(rel);
        self.app_context.drag_preview_popover.hide();
        unsafe { coreloop.as_mut().get_unchecked_mut() }.docking_preview_state = None;
    }

    pub fn update_pane_drag(&self, on_surface: WindowHandle, rect: &Rect<LogicalUnit>) {
        self.app_context
            .drag_preview_popover
            .set_rect(on_surface.0, rect);
    }

    pub fn end_pane_drag(&self) {}
}

#[derive(Debug, Clone)]
pub enum Message {
    Quit,
    Handle(usize),
}

pub fn pump_messages(handles: &[HANDLE]) -> std::io::Result<Message> {
    let mut msg = core::mem::MaybeUninit::uninit();

    loop {
        let r = unsafe {
            MsgWaitForMultipleObjectsEx(Some(handles), INFINITE, QS_ALLEVENTS, MWMO_INPUTAVAILABLE)
        };
        if r == windows::Win32::Foundation::WAIT_FAILED {
            return Err(std::io::Error::last_os_error());
        }

        if let Some(hindex) = r.0.checked_sub(WAIT_OBJECT_0.0).map(|x| x as usize) {
            if hindex < handles.len() {
                return Ok(Message::Handle(hindex));
            } else {
                while unsafe { PeekMessageW(msg.as_mut_ptr(), None, 0, 0, PM_REMOVE).as_bool() } {
                    let msg = unsafe { msg.assume_init_ref() };
                    if msg.message == WM_QUIT {
                        return Ok(Message::Quit);
                    }

                    unsafe {
                        let _ = TranslateMessage(msg);
                        DispatchMessageW(msg);
                    }
                }
                continue;
            }
        }

        tracing::warn!(?r, "unhandled mwmo result");
    }
}
