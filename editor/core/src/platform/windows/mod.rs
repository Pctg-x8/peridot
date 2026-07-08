use bedrock::{self as br, InstanceChild, SurfaceCreateInfo};
use windows::{
    Foundation::TypedEventHandler,
    System::DispatcherQueueController,
    UI::{
        Composition::{
            CompositionEffectSourceParameter, Compositor, Desktop::DesktopWindowTarget,
            SpriteVisual,
        },
        Text::Core::{
            CoreTextCompositionCompletedEventArgs, CoreTextCompositionStartedEventArgs,
            CoreTextEditContext, CoreTextFormatUpdatingEventArgs, CoreTextLayoutRequest,
            CoreTextLayoutRequestedEventArgs, CoreTextRange, CoreTextSelectionRequest,
            CoreTextSelectionRequestedEventArgs, CoreTextSelectionUpdatingEventArgs,
            CoreTextServicesManager, CoreTextTextRequestedEventArgs, CoreTextTextUpdatingEventArgs,
        },
    },
    Win32::{
        Devices::HumanInterfaceDevice::KEYBOARD_OVERRUN_MAKE_CODE,
        Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, POINT, RECT, WPARAM},
        Graphics::{
            Direct3D::D3D_FEATURE_LEVEL_12_0,
            Direct3D12::{
                D3D12_COMMAND_LIST_TYPE_DIRECT, D3D12_COMMAND_QUEUE_DESC,
                D3D12_COMMAND_QUEUE_FLAG_NONE, D3D12CreateDevice, D3D12GetDebugInterface,
                ID3D12CommandQueue, ID3D12Debug, ID3D12Device,
            },
            Dwm::{
                DWMWA_EXTENDED_FRAME_BOUNDS, DwmExtendFrameIntoClientArea, DwmGetWindowAttribute,
            },
            Dxgi::{CreateDXGIFactory2, DXGI_CREATE_FACTORY_DEBUG, IDXGIFactory2},
            Gdi::{
                GetMonitorInfoW, HBRUSH, MONITOR_DEFAULTTONEAREST, MONITOR_DEFAULTTOPRIMARY,
                MONITORINFO, MapWindowPoints, MonitorFromWindow,
            },
        },
        System::{
            SystemServices::{MK_CONTROL, MK_SHIFT},
            WinRT::{
                Composition::{ICompositorDesktopInterop, ICompositorInterop},
                CreateDispatcherQueueController, DQTAT_COM_ASTA, DQTYPE_THREAD_CURRENT,
                DispatcherQueueOptions,
            },
        },
        UI::{
            Controls::{MARGINS, WM_MOUSELEAVE},
            HiDpi::GetDpiForWindow,
            Input::{
                GetRawInputData, HRAWINPUT,
                KeyboardAndMouse::{
                    MAPVK_VK_TO_VSC_EX, MAPVK_VSC_TO_VK_EX, MapVirtualKeyW, ReleaseCapture,
                    SetCapture, TME_HOVER, TME_LEAVE, TME_NONCLIENT, TRACKMOUSEEVENT,
                    TrackMouseEvent, VK_0, VK_9, VK_A, VK_BACK, VK_CONTROL, VK_DELETE, VK_DOWN,
                    VK_LCONTROL, VK_LEFT, VK_LMENU, VK_LSHIFT, VK_LWIN, VK_MENU, VK_RCONTROL,
                    VK_RETURN, VK_RIGHT, VK_RMENU, VK_RSHIFT, VK_RWIN, VK_SHIFT, VK_UP, VK_Z,
                },
                RAWINPUT, RAWINPUTDEVICE, RAWINPUTDEVICE_FLAGS, RAWINPUTHEADER, RID_INPUT,
                RIM_TYPEKEYBOARD, RegisterRawInputDevices,
            },
            WindowsAndMessaging::{
                CW_USEDEFAULT, CreateWindowExW, DefWindowProcW, DestroyWindow, GCW_ATOM,
                GetClassLongPtrW, GetClientRect, GetCursorPos, GetSystemMetrics, GetWindowLongPtrW,
                HCURSOR, HICON, HTBOTTOM, HTBOTTOMLEFT, HTBOTTOMRIGHT, HTCAPTION, HTCLIENT,
                HTCLOSE, HTLEFT, HTMAXBUTTON, HTMINBUTTON, HTRIGHT, HTTOP, HTTOPLEFT, HTTOPRIGHT,
                IDC_ARROW, IDC_HAND, IDC_IBEAM, IDC_SIZENS, IDC_SIZEWE, IDI_APPLICATION, IsZoomed,
                LoadCursorW, LoadIconW, NCCALCSIZE_PARAMS, PostMessageW, PostQuitMessage,
                RI_KEY_BREAK, RI_KEY_E0, RI_KEY_E1, SC_CLOSE, SC_MAXIMIZE, SC_MINIMIZE, SC_RESTORE,
                SIZE_MAXIMIZED, SIZE_RESTORED, SM_CXSIZEFRAME, SM_CYSIZEFRAME, SW_HIDE, SW_SHOW,
                SW_SHOWNOACTIVATE, SW_SHOWNORMAL, SWP_FRAMECHANGED, SWP_NOACTIVATE, SWP_NOMOVE,
                SWP_NOSIZE, SWP_NOZORDER, SetCursor, SetCursorPos, SetForegroundWindow,
                SetWindowLongPtrW, SetWindowPos, ShowWindow, WA_INACTIVE, WHEEL_DELTA,
                WINDOW_LONG_PTR_INDEX, WM_ACTIVATE, WM_CHAR, WM_CLOSE, WM_CREATE, WM_DESTROY,
                WM_DPICHANGED, WM_INPUT, WM_KEYDOWN, WM_KEYUP, WM_KILLFOCUS, WM_LBUTTONDOWN,
                WM_LBUTTONUP, WM_MOUSEMOVE, WM_MOUSEWHEEL, WM_MOVE, WM_NCCALCSIZE, WM_NCHITTEST,
                WM_NCLBUTTONDOWN, WM_NCLBUTTONUP, WM_NCMOUSELEAVE, WM_NCMOUSEMOVE,
                WM_NCRBUTTONDOWN, WM_NCRBUTTONUP, WM_RBUTTONDOWN, WM_RBUTTONUP, WM_SETFOCUS,
                WM_SIZE, WM_SYSCOMMAND, WNDCLASS_STYLES, WNDCLASSEXW, WS_EX_APPWINDOW,
                WS_EX_LAYERED, WS_EX_NOACTIVATE, WS_EX_NOREDIRECTIONBITMAP, WS_EX_TOPMOST,
                WS_EX_TRANSPARENT, WS_MAXIMIZE, WS_OVERLAPPEDWINDOW, WS_POPUP, WindowFromPoint,
            },
        },
    },
};
use windows_core::{HSTRING, IInspectable, Interface, PCWSTR, h, w};
use windows_numerics::{Vector2, Vector3};

use core::cell::Cell;
use std::{rc::Rc, sync::Mutex};

use crate::{
    Event, LogicFiberEventDispatcher, MainWindowOpenMode, SubWindowOpenMode, SyncEvent,
    WindowGeometryState, WindowType,
    bindgen::Microsoft::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    graphics::{VulkanDevice, VulkanSurface},
    input::{
        InputEventContext, KeyInputCode, KeyboardFocusGroupRef, KeyboardFocusTokenRegistry,
        ModifierKey, PerWindowKeyboardFocusState, PointerInputManager, PointerInputUnit,
        ShellPointerActions,
        hittest::{
            CursorShape, HitTestTreeData, HitTestTreeManager, HitTestTreeRef, PointerButton,
        },
    },
    rendering::{
        NewWindowData, NewWindowVulkanSurface, RenderMessage, RenderMessageSender,
        composite::{CompositeRect, CompositeTree, CompositeTreeRef},
        text::FontSet,
    },
    uikit::MountTarget,
    utils::{
        LogicalUnit, PixelsUnit, Point, Rect, Size,
        platform::windows::{
            EnumerateDisplayMonitorContinuous, WaitableTimer, current_instance_handle,
            enumerate_display_monitors, register_class,
        },
    },
};

pub mod flyout_surface;

pub type WindowPersistentStateNativeGeometryUnit = PixelsUnit;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WindowHandle(HWND);
unsafe impl Send for WindowHandle {}
unsafe impl Sync for WindowHandle {}
impl core::hash::Hash for WindowHandle {
    #[inline(always)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.0.hash(state)
    }
}
impl WindowHandle {
    #[inline(always)]
    pub fn destroy(&mut self) {
        self.state_mut().destroying = true;
        if let Err(e) = unsafe { DestroyWindow(self.0) } {
            tracing::error!(reason = %e, "window.destroy");
        }
    }

    #[inline(always)]
    pub fn associate_extra_data<T>(&mut self, data: Box<T>) {
        unsafe {
            SetWindowLongPtrW(
                self.0,
                NativeWindow::APP_POINTER_LONG_PTR_OFFSET,
                Box::into_raw(data).addr().cast_signed(),
            );
        }
    }

    #[inline(always)]
    pub unsafe fn extra_data_ref<'a, T>(&'a self) -> &'a T {
        unsafe {
            &*core::ptr::with_exposed_provenance(
                GetWindowLongPtrW(self.0, NativeWindow::APP_POINTER_LONG_PTR_OFFSET)
                    .cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    pub unsafe fn extra_data_mut<'a, T>(&'a mut self) -> &'a mut T {
        unsafe {
            &mut *core::ptr::with_exposed_provenance_mut(
                GetWindowLongPtrW(self.0, NativeWindow::APP_POINTER_LONG_PTR_OFFSET)
                    .cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    pub unsafe fn take_extra_data<T>(&mut self) -> Box<T> {
        let data = unsafe {
            Box::from_raw(core::ptr::with_exposed_provenance_mut(
                GetWindowLongPtrW(self.0, NativeWindow::APP_POINTER_LONG_PTR_OFFSET)
                    .cast_unsigned(),
            ))
        };
        unsafe {
            SetWindowLongPtrW(self.0, NativeWindow::APP_POINTER_LONG_PTR_OFFSET, 0);
        }

        data
    }

    #[inline(always)]
    fn event_handler_mut<'a>(&'a mut self) -> &'a mut WindowEventHandler {
        unsafe {
            &mut *core::ptr::with_exposed_provenance_mut(
                GetWindowLongPtrW(self.0, WindowEventHandler::LONG_PTR_INDEX).cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    pub fn state<'a>(&'a self) -> &'a WindowState {
        unsafe {
            &*core::ptr::with_exposed_provenance(
                GetWindowLongPtrW(self.0, WindowEventHandler::LONG_PTR_INDEX).cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    pub fn state_mut<'a>(&'a mut self) -> &'a mut WindowState {
        unsafe {
            &mut *core::ptr::with_exposed_provenance_mut(
                GetWindowLongPtrW(self.0, WindowEventHandler::LONG_PTR_INDEX).cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        let mut rc = core::mem::MaybeUninit::uninit();
        if let Err(e) = unsafe { GetClientRect(self.0, rc.as_mut_ptr()) } {
            tracing::error!(reason = %e, "get_client_rect");
            return Size::new_logical(0.0, 0.0);
        }

        let rc = unsafe { rc.assume_init_ref() };
        Size::new_pixels(rc.right as _, rc.bottom as _)
            .to_logical(unsafe { GetDpiForWindow(self.0) as f32 / 96.0 })
    }

    #[inline(always)]
    pub fn screen_position(&self) -> Point<PixelsUnit> {
        let mut extended_frame_bounds = core::mem::MaybeUninit::<RECT>::uninit();
        unsafe {
            DwmGetWindowAttribute(
                self.0,
                DWMWA_EXTENDED_FRAME_BOUNDS,
                extended_frame_bounds.as_mut_ptr().cast(),
                core::mem::size_of::<RECT>() as _,
            )
            .expect("DwmGetWindowAttribute")
        }
        let extended_frame_bounds = unsafe { extended_frame_bounds.assume_init_ref() };

        Point::new_pixels(
            extended_frame_bounds.left as _,
            extended_frame_bounds.top as _,
        )
    }

    #[inline(always)]
    pub fn pixels_client_size(&self) -> Size<PixelsUnit> {
        let mut rect = core::mem::MaybeUninit::uninit();
        unsafe {
            GetClientRect(self.0, rect.as_mut_ptr()).expect("GetClientRect");
        }
        let rect = unsafe { rect.assume_init_ref() };
        Size::new_pixels(rect.right as _, rect.bottom as _)
    }

    #[inline(always)]
    pub fn is_maximized(&self) -> bool {
        unsafe { IsZoomed(self.0).as_bool() }
    }

    #[inline(always)]
    pub fn ui_scale_factor(&self) -> f32 {
        unsafe { GetDpiForWindow(self.0) as f32 / 96.0 }
    }

    #[inline(always)]
    pub fn latest_ui_scale_changes(&self) -> &Mutex<Option<f32>> {
        &self.state().latest_ui_scale_changes
    }

    #[inline(always)]
    pub fn client_pos_to_screen_pos(&self, p: Point<LogicalUnit>) -> Point<PixelsUnit> {
        let mut p = [p.to_pixels_round(self.ui_scale_factor()).to_win32()];
        unsafe {
            MapWindowPoints(Some(self.0), None, &mut p);
        }
        Point::from_win32(p[0])
    }

    #[inline(always)]
    pub const fn needs_system_command_buttons(&self) -> bool {
        // Windowsは常にtrue
        true
    }

    #[inline(always)]
    pub const fn needs_corner_cutout_rendering(&self) -> bool {
        // Windowsは常に不要
        false
    }

    #[inline(always)]
    pub fn on_click_sys_close_button(&self) {
        if let Err(e) = unsafe {
            PostMessageW(
                Some(self.0),
                WM_SYSCOMMAND,
                WPARAM(SC_CLOSE as _),
                LPARAM(0),
            )
        } {
            tracing::error!(reason = %e, "postmessage");
        }
    }

    #[inline(always)]
    pub fn on_click_sys_minimize_button(&self) {
        if let Err(e) = unsafe {
            PostMessageW(
                Some(self.0),
                WM_SYSCOMMAND,
                WPARAM(SC_MINIMIZE as _),
                LPARAM(0),
            )
        } {
            tracing::error!(reason = %e, "postmessage");
        }
    }

    #[inline(always)]
    pub fn on_click_sys_maximize_button(&self) {
        if let Err(e) = unsafe {
            PostMessageW(
                Some(self.0),
                WM_SYSCOMMAND,
                WPARAM(SC_MAXIMIZE as _),
                LPARAM(0),
            )
        } {
            tracing::error!(reason = %e, "postmessage");
        }
    }

    #[inline(always)]
    pub fn on_click_sys_restore_button(&self) {
        if let Err(e) = unsafe {
            PostMessageW(
                Some(self.0),
                WM_SYSCOMMAND,
                WPARAM(SC_RESTORE as _),
                LPARAM(0),
            )
        } {
            tracing::error!(reason = %e, "postmessage");
        }
    }

    #[inline(always)]
    pub fn keyboard_focus_state(&self) -> &PerWindowKeyboardFocusState {
        &self.state().keyboard_focus_state
    }

    #[inline(always)]
    pub fn keyboard_focus_state_mut(&mut self) -> &mut PerWindowKeyboardFocusState {
        &mut self.state_mut().keyboard_focus_state
    }

    #[inline(always)]
    pub fn keyboard_focus_group(&self) -> KeyboardFocusGroupRef {
        self.state().root_focus_group
    }

    pub fn geometry_state_snapshot(&self, _system_link: &SystemLink) -> crate::WindowGeometryState {
        if unsafe { IsZoomed(self.0) }.as_bool() {
            let hm = unsafe { MonitorFromWindow(self.0, MONITOR_DEFAULTTONEAREST) };
            let mut monitor_index = 0;
            enumerate_display_monitors(|m, _| {
                if m == hm {
                    return EnumerateDisplayMonitorContinuous::Stop;
                }

                monitor_index += 1;
                EnumerateDisplayMonitorContinuous::Continue
            });

            crate::WindowGeometryState::Maximized { monitor_index }
        } else {
            let mut rc = core::mem::MaybeUninit::<RECT>::uninit();
            unsafe {
                DwmGetWindowAttribute(
                    self.0,
                    DWMWA_EXTENDED_FRAME_BOUNDS,
                    rc.as_mut_ptr().cast(),
                    size_of::<RECT>() as _,
                )
                .expect("dwmgetwindowattribute.extended_frame_bounds")
            };
            let rc = unsafe { rc.assume_init_ref() };

            crate::WindowGeometryState::Restored {
                rect: Rect::from_lt_size(
                    Point::new_pixels(rc.left, rc.top),
                    Size::new_pixels((rc.right - rc.left) as _, (rc.bottom - rc.top) as _),
                ),
            }
        }
    }
}
impl ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {
        unsafe {
            SetCapture(self.0);
        }
    }

    #[inline(always)]
    fn release_pointer(&self) {
        if let Err(e) = unsafe { ReleaseCapture() } {
            tracing::error!(reason = %e, "release_capture");
        }
    }
}
impl crate::uikit::MountTarget for WindowHandle {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.state().composite_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.state().ht_root
    }
}

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

pub struct NativeWindow {
    hinstance: HINSTANCE,
    hwnd: HWND,
}
impl NativeWindow {
    // extra storage assignment
    const EVENT_HANDLER_LONG_PTR_INDEX: WINDOW_LONG_PTR_INDEX = WINDOW_LONG_PTR_INDEX(0);
    const APP_POINTER_LONG_PTR_OFFSET: WINDOW_LONG_PTR_INDEX =
        WINDOW_LONG_PTR_INDEX(core::mem::size_of::<usize>() as _);
    const EXTRA_STORAGE_SIZE: usize = core::mem::size_of::<[usize; 2]>();

    fn register_class(hinstance: HINSTANCE) -> u16 {
        unsafe {
            register_class(&WNDCLASSEXW {
                cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
                style: WNDCLASS_STYLES(0),
                cbClsExtra: 0,
                cbWndExtra: NativeWindow::EXTRA_STORAGE_SIZE as _,
                lpfnWndProc: Some(WindowEventHandler::handle_messages),
                hInstance: hinstance,
                hIcon: LoadIconW(None, IDI_APPLICATION).expect("LoadIconW"),
                hCursor: HCURSOR(core::ptr::null_mut()),
                hbrBackground: HBRUSH(core::ptr::null_mut()),
                lpszMenuName: PCWSTR::null(),
                lpszClassName: w!("MainWindow"),
                hIconSm: LoadIconW(None, IDI_APPLICATION).expect("LoadIconW"),
            })
            .expect("register_class.main")
        }
    }

    fn new(
        app_context: &ApplicationContext,
        window_type: WindowType,
        pos: Option<Point<PixelsUnit>>,
        size: Option<Size<PixelsUnit>>,
        maximized: bool,
        composite_root: CompositeTreeRef,
        ht_root: HitTestTreeRef,
        event_dispatcher: LogicFiberEventDispatcher,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) -> Self {
        let mut window_style = WS_OVERLAPPEDWINDOW;
        if maximized {
            window_style |= WS_MAXIMIZE;
        }
        let w = unsafe {
            CreateWindowExW(
                WS_EX_APPWINDOW,
                PCWSTR(core::ptr::without_provenance(app_context.wc_set.main as _)),
                w!("Peridot Marble Editor"),
                window_style,
                pos.as_ref().map_or(CW_USEDEFAULT, |r| r.x),
                pos.as_ref().map_or(CW_USEDEFAULT, |r| r.y),
                size.as_ref().map_or(CW_USEDEFAULT, |r| r.width as _),
                size.as_ref().map_or(CW_USEDEFAULT, |r| r.height as _),
                None,
                None,
                Some(app_context.hinstance),
                None,
            )
            .expect("CreateWindowExW")
        };
        let root_kf_group = keyboard_focus_registry.acquire_group();
        let event_handler = Box::new(WindowEventHandler {
            state: WindowState {
                r#type: window_type,
                content_scale: unsafe { GetDpiForWindow(w) as f32 / 96.0 },
                composite_root,
                ht_root,
                latest_ui_scale_changes: Mutex::new(None),
                keyboard_focus_state: PerWindowKeyboardFocusState::new(root_kf_group),
                root_focus_group: root_kf_group,
                destroying: false,
            },
            app_context,
            event_dispatcher,
            edit_context: None,
            modifier_key_state: ModifierKey::empty(),
        });
        unsafe {
            SetWindowLongPtrW(
                w,
                WindowEventHandler::LONG_PTR_INDEX,
                Box::into_raw(event_handler).addr().cast_signed(),
            );
        }

        Self {
            hinstance: app_context.hinstance,
            hwnd: w,
        }
    }

    #[inline(always)]
    fn create_vk_surface<'d, 'fs>(&self, device: &'d VulkanDevice<'fs>) -> VulkanSurface<'d, 'fs> {
        VulkanSurface::new(device, unsafe {
            br::Win32SurfaceCreateInfo::new(
                core::mem::transmute(self.hinstance),
                core::mem::transmute(self.hwnd),
            )
            .execute(device.instance(), None)
            .expect("vk_surface.create")
        })
    }

    #[inline(always)]
    const fn make_handle(&self) -> WindowHandle {
        WindowHandle(self.hwnd)
    }
}

pub struct WindowState {
    r#type: WindowType,
    content_scale: f32,
    pub composite_root: CompositeTreeRef,
    pub ht_root: HitTestTreeRef,
    pub latest_ui_scale_changes: Mutex<Option<f32>>,
    pub keyboard_focus_state: PerWindowKeyboardFocusState,
    pub root_focus_group: KeyboardFocusGroupRef,
    destroying: bool,
}

// WindowsではWM_NCHITTESTの返り値の計算に必要なので一旦生ポインタをグローバルにおいて参照もたせる（実際どうするかはあとで考える）
static mut POINTER_INPUT_MANAGER_PTR: *const PointerInputManager = core::ptr::null();
static mut HIT_TEST_TREE_MANAGER_PTR: *const HitTestTreeManager<'static> = core::ptr::null();
pub unsafe fn locate_non_client_hittest_managers(
    pointer_input_manager: &PointerInputManager,
    ht_manager: &HitTestTreeManager,
) {
    unsafe {
        POINTER_INPUT_MANAGER_PTR = pointer_input_manager;
        HIT_TEST_TREE_MANAGER_PTR = core::mem::transmute(ht_manager);
    }
}
pub unsafe fn unlocate_non_client_hittest_managers() {
    unsafe {
        POINTER_INPUT_MANAGER_PTR = core::ptr::null();
        HIT_TEST_TREE_MANAGER_PTR = core::ptr::null();
    }
}

#[repr(C)] // place state at always 0: this structure can be reinterpreted as a WindowState
struct WindowEventHandler {
    state: WindowState,
    app_context: *const ApplicationContext,
    event_dispatcher: LogicFiberEventDispatcher,
    edit_context: Option<CoreTextEditContext>,
    modifier_key_state: ModifierKey,
}
impl WindowEventHandler {
    const LONG_PTR_INDEX: WINDOW_LONG_PTR_INDEX = NativeWindow::EVENT_HANDLER_LONG_PTR_INDEX;

    #[inline(always)]
    fn get_for_window<'a>(w: HWND) -> &'a mut Self {
        unsafe {
            &mut *core::ptr::with_exposed_provenance_mut::<Self>(
                GetWindowLongPtrW(w, Self::LONG_PTR_INDEX).cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    fn try_get_for_window<'a>(w: HWND) -> Option<&'a mut Self> {
        unsafe {
            core::ptr::with_exposed_provenance_mut::<Self>(
                GetWindowLongPtrW(w, Self::LONG_PTR_INDEX).cast_unsigned(),
            )
            .as_mut()
        }
    }

    #[inline(always)]
    fn dispatch_event(w: HWND, e: Event) {
        Self::get_for_window(w).event_dispatcher.dispatch(e);
    }

    #[inline(always)]
    fn try_dispatch_event(w: HWND, e: Event) {
        if let Some(st) = Self::try_get_for_window(w) {
            st.event_dispatcher.dispatch(e);
        }
    }

    fn compute_client_rect(w: HWND, params: &mut NCCALCSIZE_PARAMS) {
        if unsafe { IsZoomed(w).as_bool() } {
            // 最大化状態
            let mon = unsafe { MonitorFromWindow(w, MONITOR_DEFAULTTONEAREST) };
            let mut minfo = core::mem::MaybeUninit::<MONITORINFO>::uninit();
            unsafe {
                core::ptr::write(
                    &mut (*minfo.as_mut_ptr()).cbSize,
                    core::mem::size_of::<MONITORINFO>() as _,
                );
            }
            unsafe {
                GetMonitorInfoW(mon, minfo.as_mut_ptr()).expect("GetMonitorInfo");
            }
            let minfo = unsafe { minfo.assume_init() };

            // 現在のモニタサイズに合わせる（これで合ってるのか不明だけど大体の場合で正しく動くはず）
            params.rgrc[0] = minfo.rcWork;
            return;
        }

        // remove non-client area
        let w = unsafe { GetSystemMetrics(SM_CXSIZEFRAME) };
        let h = unsafe { GetSystemMetrics(SM_CYSIZEFRAME) };
        params.rgrc[0].left += w;
        params.rgrc[0].right -= w;
        params.rgrc[0].bottom -= h;
        // topはいじらない（topいじるともとのタイトルバーが一部表示される 他アプリもそんな感じなのでtopは自前で当たり判定組んでリサイズ判定する）
    }

    #[tracing::instrument(skip(self))]
    fn dpi_changed(&mut self, hwnd: HWND, new_scale: f32, new_rect: &RECT) {
        tracing::trace!("dpi changed");

        self.state.content_scale = new_scale;
        *self.state.latest_ui_scale_changes.lock().expect("poisoned") = Some(new_scale);
        self.event_dispatcher.dispatch(Event::WindowRescaleUI {
            window: WindowHandle(hwnd),
            new_scale,
        });

        unsafe {
            // move to suggested rect
            if let Err(e) = SetWindowPos(
                hwnd,
                None,
                new_rect.left,
                new_rect.top,
                new_rect.right - new_rect.left,
                new_rect.bottom - new_rect.top,
                SWP_NOZORDER,
            ) {
                tracing::error!(reason = %e, "dpi_changed.set_window_pos");
            }
        }
    }

    #[tracing::instrument(skip(self))]
    fn resize(&mut self, hwnd: HWND, new_size: Size<PixelsUnit>) {
        tracing::trace!(?new_size);

        self.event_dispatcher.dispatch(Event::WindowResize {
            window: WindowHandle(hwnd),
            size: new_size.to_logical(self.state.content_scale),
        });
    }

    #[tracing::instrument(skip(self))]
    fn mouse_move(&mut self, hwnd: HWND, client_pos: Point<PixelsUnit>) {
        if unsafe { &*self.app_context }.pane_dragging.get() {
            // in pane dragging
            let mut cursor_pos = core::mem::MaybeUninit::uninit();
            unsafe {
                GetCursorPos(cursor_pos.as_mut_ptr()).expect("GetCursorPos");
            }
            let mut p = [unsafe { cursor_pos.assume_init() }];

            let w = unsafe { WindowFromPoint(p[0]) };
            let pointing_window = if w.is_invalid() {
                None
            } else {
                let wc_atom = unsafe { GetClassLongPtrW(w, GCW_ATOM) as u16 };
                if wc_atom == unsafe { &*self.app_context }.wc_set.main {
                    Some(WindowHandle(w))
                } else {
                    None
                }
            };

            let dest_window = pointing_window.unwrap_or(WindowHandle(hwnd));
            unsafe {
                MapWindowPoints(None, Some(dest_window.0), &mut p);
            }
            let client_pos_in_dest =
                Point::from_win32(p[0]).to_logical(dest_window.ui_scale_factor());

            unsafe {
                SetForegroundWindow(dest_window.0).expect("dest_window.set_foreground");
            }
            self.event_dispatcher.dispatch(Event::DockMovePreview {
                dest_window,
                client_pos_in_dest,
            });

            return;
        }

        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(hwnd),
            client_pos: client_pos.to_logical(self.state.content_scale),
        });
    }

    #[tracing::instrument(skip(self))]
    fn left_button_down(&mut self, hwnd: HWND, client_pos: Point<PixelsUnit>) {
        // move then down
        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(hwnd),
            client_pos: client_pos.to_logical(self.state.content_scale),
        });
        self.event_dispatcher.dispatch(Event::PointerDown {
            window: WindowHandle(hwnd),
            pointer_id: PointerID(),
            button: PointerButton::Primary,
        });
    }

    #[tracing::instrument(skip(self))]
    fn left_button_up(&mut self, hwnd: HWND) {
        if self.try_perform_confirm_drag(hwnd) {
            return;
        }

        self.event_dispatcher.dispatch(Event::PointerUp {
            window: WindowHandle(hwnd),
            pointer_id: PointerID(),
            button: PointerButton::Primary,
        });
    }

    #[tracing::instrument(skip(self))]
    fn right_button_down(&mut self, hwnd: HWND, client_pos: Point<PixelsUnit>) {
        // move then down
        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(hwnd),
            client_pos: client_pos.to_logical(self.state.content_scale),
        });
        self.event_dispatcher.dispatch(Event::PointerDown {
            window: WindowHandle(hwnd),
            pointer_id: PointerID(),
            button: PointerButton::Secondary,
        });
    }

    #[tracing::instrument(skip(self))]
    fn right_button_up(&mut self, hwnd: HWND) {
        if self.try_perform_confirm_drag(hwnd) {
            return;
        }

        self.event_dispatcher.dispatch(Event::PointerUp {
            window: WindowHandle(hwnd),
            pointer_id: PointerID(),
            button: PointerButton::Secondary,
        });
    }

    fn try_perform_confirm_drag(&mut self, hwnd: HWND) -> bool {
        if !unsafe { &*self.app_context }.pane_dragging.replace(false) {
            return false;
        };

        // was pane dragging
        let mut cursor_pos = core::mem::MaybeUninit::uninit();
        unsafe {
            GetCursorPos(cursor_pos.as_mut_ptr()).expect("GetCursorPos");
        }
        let mut p = [unsafe { cursor_pos.assume_init() }];

        let w = unsafe { WindowFromPoint(p[0]) };
        let pointing_window = if w.is_invalid() {
            None
        } else {
            let wc_atom = unsafe { GetClassLongPtrW(w, GCW_ATOM) as u16 };
            if wc_atom == unsafe { &*self.app_context }.wc_set.main {
                Some(WindowHandle(w))
            } else {
                None
            }
        };

        let dest_window = pointing_window.unwrap_or(WindowHandle(hwnd));
        unsafe {
            MapWindowPoints(None, Some(dest_window.0), &mut p);
        }
        let client_pos_in_dest = Point::from_win32(p[0]).to_logical(dest_window.ui_scale_factor());

        unsafe {
            SetForegroundWindow(dest_window.0).expect("dest_window.set_foreground");
        }
        self.event_dispatcher.dispatch(Event::DockConfirm {
            pointer: PointerID(),
            destination_window: dest_window,
            client_pos_in_dest,
        });

        true
    }

    fn non_client_hittest(&self, hwnd: HWND, screen_pos: Point<PixelsUnit>) -> Option<u32> {
        let mut p = [screen_pos.to_win32()];
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
            return None;
        }

        let resize_h = unsafe { GetSystemMetrics(SM_CYSIZEFRAME) };
        if client_pos.y < resize_h {
            return Some(HTTOP);
        }

        if unsafe { POINTER_INPUT_MANAGER_PTR.is_null() } {
            // unlinked from logic fiber
            return Some(HTCLIENT);
        }

        let pointer_input_manager = unsafe { &*POINTER_INPUT_MANAGER_PTR };
        match pointer_input_manager.role(
            &client_pos.to_logical(self.state.content_scale),
            &Size::new_pixels(
                (client_size.right - client_size.left) as _,
                (client_size.bottom - client_size.top) as _,
            )
            .to_logical(self.state.content_scale),
            unsafe { &*HIT_TEST_TREE_MANAGER_PTR },
            self.state.ht_root,
        ) {
            None => Some(HTCLIENT),
            Some(crate::input::hittest::Role::TitleBar) => Some(HTCAPTION),
            Some(crate::input::hittest::Role::ForceClient) => Some(HTCLIENT),
            Some(crate::input::hittest::Role::CloseButton) => Some(HTCLOSE),
            Some(crate::input::hittest::Role::MaximizeButton) => Some(HTMAXBUTTON),
            Some(crate::input::hittest::Role::MinimizeButton) => Some(HTMINBUTTON),
            // Windowsだと同じ位置にあるので同じものを返す
            Some(crate::input::hittest::Role::RestoreButton) => Some(HTMAXBUTTON),
        }
    }

    #[inline(always)]
    const fn is_application_handled_hittest(ht: u32) -> bool {
        ht == HTCLOSE || ht == HTMAXBUTTON || ht == HTMINBUTTON
    }

    fn translate_keycode(code: usize) -> KeyInputCode {
        match code {
            v if v == VK_LEFT.0 as _ => KeyInputCode::LeftArrow,
            v if v == VK_RIGHT.0 as _ => KeyInputCode::RightArrow,
            v if v == VK_UP.0 as _ => KeyInputCode::UpArrow,
            v if v == VK_DOWN.0 as _ => KeyInputCode::DownArrow,
            v if v == VK_RETURN.0 as _ => KeyInputCode::Enter,
            v if v == VK_DELETE.0 as _ => KeyInputCode::Delete,
            v if v == VK_BACK.0 as _ => KeyInputCode::Backspace,
            v if (VK_0.0 as usize <= v && v <= VK_9.0 as usize)
                || (VK_A.0 as usize <= v && v <= VK_Z.0 as usize) =>
            {
                KeyInputCode::Character(v as u8 as _)
            }
            _ => KeyInputCode::UnknownNativeCode(code as _),
        }
    }

    fn keydown(&mut self, hwnd: HWND, code: usize) {
        if code == VK_SHIFT.0 as _ || code == VK_LSHIFT.0 as _ || code == VK_RSHIFT.0 as _ {
            self.modifier_key_state |= ModifierKey::SHIFT;
        }
        if code == VK_MENU.0 as _ || code == VK_LMENU.0 as _ || code == VK_RMENU.0 as _ {
            self.modifier_key_state |= ModifierKey::ALT;
        }
        if code == VK_CONTROL.0 as _ || code == VK_LCONTROL.0 as _ || code == VK_RCONTROL.0 as _ {
            self.modifier_key_state |= ModifierKey::CONTROL;
        }
        if code == VK_LWIN.0 as _ || code == VK_RWIN.0 as _ {
            self.modifier_key_state |= ModifierKey::SUPER;
        }

        self.event_dispatcher.dispatch(Event::KeyDown {
            code: Self::translate_keycode(code),
            modifier: self.modifier_key_state,
            window: WindowHandle(hwnd),
        });
    }

    fn keyup(&mut self, hwnd: HWND, code: usize) {
        if code == VK_SHIFT.0 as _ || code == VK_LSHIFT.0 as _ || code == VK_RSHIFT.0 as _ {
            self.modifier_key_state &= !ModifierKey::SHIFT;
        }
        if code == VK_MENU.0 as _ || code == VK_LMENU.0 as _ || code == VK_RMENU.0 as _ {
            self.modifier_key_state &= !ModifierKey::ALT;
        }
        if code == VK_CONTROL.0 as _ || code == VK_LCONTROL.0 as _ || code == VK_RCONTROL.0 as _ {
            self.modifier_key_state &= !ModifierKey::CONTROL;
        }
        if code == VK_LWIN.0 as _ || code == VK_RWIN.0 as _ {
            self.modifier_key_state &= !ModifierKey::SUPER;
        }

        self.event_dispatcher.dispatch(Event::KeyUp {
            code: Self::translate_keycode(code),
            modifier: self.modifier_key_state,
            window: WindowHandle(hwnd),
        });
    }

    fn char_key(&self, hwnd: HWND, code: usize) {
        if code == 0x08 || code == 0x0d {
            // 一部の記号キーはすでにイベント投げてるのでここでは見ない
            return;
        }

        self.event_dispatcher.dispatch(Event::KeyChar {
            ch: unsafe { char::from_u32_unchecked(code as _) },
            modifier: self.modifier_key_state,
            window: WindowHandle(hwnd),
        });
    }

    extern "system" fn handle_messages(
        hwnd: HWND,
        msg: u32,
        wparam: WPARAM,
        lparam: LPARAM,
    ) -> LRESULT {
        if msg == WM_CLOSE {
            let Some(e) = Self::try_get_for_window(hwnd) else {
                return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
            };

            e.state.destroying = true;
            match e.state.r#type {
                WindowType::Main {} => unsafe {
                    PostQuitMessage(0);
                },
                WindowType::Sub => e.event_dispatcher.dispatch(Event::SubWindowClose {
                    window: WindowHandle(hwnd),
                }),
            }

            return LRESULT(0);
        }

        if msg == WM_DESTROY {
            unsafe {
                drop(Box::from_raw(
                    core::ptr::with_exposed_provenance_mut::<Self>(
                        GetWindowLongPtrW(hwnd, Self::LONG_PTR_INDEX).cast_unsigned(),
                    ),
                ));
            }

            return LRESULT(0);
        }

        if msg == WM_CREATE {
            unsafe {
                // notify frame change
                SetWindowPos(
                    hwnd,
                    None,
                    0,
                    0,
                    0,
                    0,
                    SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_FRAMECHANGED | SWP_NOACTIVATE,
                )
                .expect("create.swp.framechange");
            }

            return LRESULT(0);
        }

        if msg == WM_ACTIVATE {
            if let Err(e) = unsafe {
                DwmExtendFrameIntoClientArea(
                    hwnd,
                    &MARGINS {
                        cxLeftWidth: 1,
                        cxRightWidth: 1,
                        cyTopHeight: 1,
                        cyBottomHeight: 1,
                    },
                )
            } {
                tracing::error!(reason = %e, "DwmExtendFrameIntoClientArea");
            }

            Self::try_dispatch_event(
                hwnd,
                Event::WindowActivatingStateChanged {
                    window: WindowHandle(hwnd),
                    activated: wparam.0 != WA_INACTIVE as _,
                },
            );

            return LRESULT(0);
        }

        if msg == WM_DPICHANGED {
            Self::get_for_window(hwnd).dpi_changed(
                hwnd,
                (wparam.0 & 0xffff) as u16 as f32 / 96.0,
                unsafe { &*core::ptr::without_provenance(lparam.0.cast_unsigned()) },
            );

            return LRESULT(0);
        }

        if msg == WM_SETFOCUS {
            let st = Self::get_for_window(hwnd);
            if st.state.destroying {
                // in destroy sequence
                return LRESULT(0);
            }

            st.event_dispatcher.dispatch(Event::WindowFocusChanged {
                window: WindowHandle(hwnd),
                focused: true,
            });

            return LRESULT(0);
        }

        if msg == WM_KILLFOCUS {
            let st = Self::get_for_window(hwnd);
            if st.state.destroying {
                // in destroy sequence
                return LRESULT(0);
            }

            st.event_dispatcher.dispatch(Event::WindowFocusChanged {
                window: WindowHandle(hwnd),
                focused: false,
            });

            return LRESULT(0);
        }

        if msg == WM_INPUT {
            let handle = HRAWINPUT(core::ptr::without_provenance_mut(lparam.0.cast_unsigned()));
            let mut data = core::mem::MaybeUninit::<RAWINPUT>::uninit();
            let mut data_size = size_of::<RAWINPUT>() as _;
            let r = unsafe {
                GetRawInputData(
                    handle,
                    RID_INPUT,
                    Some(data.as_mut_ptr().cast()),
                    &mut data_size,
                    size_of::<RAWINPUTHEADER>() as _,
                )
            };
            if r.cast_signed() == -1 {
                panic!("GetRawInputData error");
            }
            let data = unsafe { data.assume_init_ref() };
            tracing::debug!(header = ?data.header, "raw input");
            if data.header.dwType == RIM_TYPEKEYBOARD.0 {
                let keyboard = unsafe { &data.data.keyboard };

                // scancode detection and overrun handling: https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/ns-winuser-rawkeyboard
                if keyboard.MakeCode as u32 == KEYBOARD_OVERRUN_MAKE_CODE {
                    // ignore overrun event
                    return LRESULT(0);
                }

                let vk = if keyboard.MakeCode != 0 {
                    let scan_code = (keyboard.MakeCode & 0x7f)
                        | if (keyboard.Flags as u32 & RI_KEY_E0) != 0 {
                            0x0e000u16
                        } else if (keyboard.Flags as u32 & RI_KEY_E1) != 0 {
                            0xe100u16
                        } else {
                            0u16
                        };
                    unsafe { MapVirtualKeyW(scan_code as _, MAPVK_VSC_TO_VK_EX) }
                } else {
                    keyboard.VKey as u32
                };
                let is_down = (keyboard.Flags as u32 & RI_KEY_BREAK) == 0;

                tracing::debug!(data = ?unsafe { data.data.keyboard }, vk, "raw input keyboard");
            }

            return LRESULT(0);
        }

        if msg == WM_KEYDOWN {
            Self::get_for_window(hwnd).keydown(hwnd, wparam.0);
            return LRESULT(0);
        }

        if msg == WM_KEYUP {
            Self::get_for_window(hwnd).keyup(hwnd, wparam.0);
            return LRESULT(0);
        }

        if msg == WM_CHAR {
            Self::get_for_window(hwnd).char_key(hwnd, wparam.0);
            return LRESULT(0);
        }

        if msg == WM_MOVE {
            let Some(ref st) = Self::try_get_for_window(hwnd) else {
                // preinitialized
                return LRESULT(0);
            };

            let p = Point::new_pixels(
                (lparam.0 & 0xffff) as u16 as i16 as _,
                ((lparam.0 >> 16) & 0xffff) as u16 as i16 as _,
            );
            st.event_dispatcher.dispatch(Event::WindowMove {
                window: WindowHandle(hwnd),
                pos: p.to_logical(unsafe { GetDpiForWindow(hwnd) as f32 / 96.0 }),
            });

            return LRESULT(0);
        }

        if msg == WM_SIZE {
            let Some(state) = Self::try_get_for_window(hwnd) else {
                // preinitialized
                return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
            };

            state.resize(
                hwnd,
                Size::new_pixels(
                    (lparam.0 & 0xffff) as u16 as _,
                    ((lparam.0 >> 16) & 0xffff) as u16 as _,
                ),
            );

            if wparam.0 == SIZE_MAXIMIZED as _ {
                state
                    .event_dispatcher
                    .dispatch(Event::WindowMaximizeStateChanged {
                        window: WindowHandle(hwnd),
                        is_maximized: true,
                    });
            }
            if wparam.0 == SIZE_RESTORED as _ {
                state
                    .event_dispatcher
                    .dispatch(Event::WindowMaximizeStateChanged {
                        window: WindowHandle(hwnd),
                        is_maximized: false,
                    });
            }

            return LRESULT(0);
        }

        if msg == WM_NCCALCSIZE {
            if wparam.0 == 1 {
                Self::compute_client_rect(hwnd, unsafe {
                    &mut *core::ptr::without_provenance_mut(lparam.0.cast_unsigned())
                });

                return LRESULT(0);
            }
        }

        if msg == WM_NCHITTEST {
            let Some(state) = Self::try_get_for_window(hwnd) else {
                // 初期化完了前にきた
                return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
            };

            let Some(result) = state.non_client_hittest(
                hwnd,
                Point::new_pixels(
                    (lparam.0 & 0xffff) as i16 as _,
                    ((lparam.0 >> 16) & 0xffff) as i16 as _,
                ),
            ) else {
                // よくわからん(アプリウィンドウ範囲外)のでデフォルトに任せる
                return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
            };

            return LRESULT(result as _);
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
            Self::get_for_window(hwnd).left_button_down(
                hwnd,
                Point::new_pixels(
                    (lparam.0 & 0xffff) as i16 as _,
                    ((lparam.0 >> 16) & 0xffff) as i16 as _,
                ),
            );

            return LRESULT(0);
        }

        if msg == WM_NCLBUTTONDOWN && Self::is_application_handled_hittest(wparam.0 as _) {
            // アプリケーションでハンドリングするNonClientエリア
            // NonClientイベントはスクリーン座標で来る
            let mut p = [POINT {
                x: (lparam.0 & 0xffff) as i16 as _,
                y: ((lparam.0 >> 16) & 0xffff) as i16 as _,
            }];
            unsafe {
                MapWindowPoints(None, Some(hwnd), &mut p);
            }

            Self::get_for_window(hwnd).left_button_down(hwnd, Point::new_pixels(p[0].x, p[0].y));
            return LRESULT(0);
        }

        if msg == WM_LBUTTONUP
            || (msg == WM_NCLBUTTONUP && Self::is_application_handled_hittest(wparam.0 as _))
        {
            Self::get_for_window(hwnd).left_button_up(hwnd);
            return LRESULT(0);
        }

        if msg == WM_RBUTTONDOWN {
            Self::get_for_window(hwnd).right_button_down(
                hwnd,
                Point::new_pixels(
                    (lparam.0 & 0xffff) as i16 as _,
                    ((lparam.0 >> 16) & 0xffff) as i16 as _,
                ),
            );

            return LRESULT(0);
        }

        if msg == WM_NCRBUTTONDOWN && Self::is_application_handled_hittest(wparam.0 as _) {
            // アプリケーションでハンドリングするNonClientエリア
            // NonClientイベントはスクリーン座標で来る
            let mut p = [POINT {
                x: (lparam.0 & 0xffff) as i16 as _,
                y: ((lparam.0 >> 16) & 0xffff) as i16 as _,
            }];
            unsafe {
                MapWindowPoints(None, Some(hwnd), &mut p);
            }

            Self::get_for_window(hwnd).right_button_down(hwnd, Point::new_pixels(p[0].x, p[0].y));
            return LRESULT(0);
        }

        if msg == WM_RBUTTONUP
            || (msg == WM_NCRBUTTONUP && Self::is_application_handled_hittest(wparam.0 as _))
        {
            Self::get_for_window(hwnd).right_button_up(hwnd);
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

            Self::get_for_window(hwnd).mouse_move(
                hwnd,
                Point::new_pixels(
                    (lparam.0 & 0xffff) as i16 as _,
                    ((lparam.0 >> 16) & 0xffff) as i16 as _,
                ),
            );

            return LRESULT(0);
        }

        if msg == WM_NCMOUSEMOVE {
            unsafe {
                TrackMouseEvent(&mut TRACKMOUSEEVENT {
                    cbSize: core::mem::size_of::<TRACKMOUSEEVENT>() as _,
                    dwFlags: TME_LEAVE | TME_HOVER | TME_NONCLIENT,
                    hwndTrack: hwnd,
                    dwHoverTime: 0,
                })
                .expect("TrackMouseEvent");
            }

            // NonClientイベントはスクリーン座標で来る
            let mut p = [POINT {
                x: (lparam.0 & 0xffff) as i16 as _,
                y: ((lparam.0 >> 16) & 0xffff) as i16 as _,
            }];
            unsafe {
                MapWindowPoints(None, Some(hwnd), &mut p);
            }

            Self::get_for_window(hwnd).mouse_move(hwnd, Point::new_pixels(p[0].x, p[0].y));
            // Note: NCMOUSEMOVEはデフォルト動作もさせる
        }

        if msg == WM_MOUSELEAVE || msg == WM_NCMOUSELEAVE {
            let st = Self::get_for_window(hwnd);

            if st.state.destroying {
                // closing
                return LRESULT(0);
            }

            st.event_dispatcher.dispatch(Event::PointerLeaveWindow {
                window: WindowHandle(hwnd),
                pointer_id: PointerID(),
            });

            return LRESULT(0);
        }

        if msg == WM_MOUSEWHEEL {
            let mut key_modifier = ModifierKey::empty();
            let mks = wparam.0 as u16 as u32;
            if mks & MK_CONTROL.0 != 0 {
                key_modifier |= ModifierKey::CONTROL;
            }
            if mks & MK_SHIFT.0 != 0 {
                key_modifier |= ModifierKey::SHIFT;
            }

            Self::dispatch_event(
                hwnd,
                Event::ScrollWheel {
                    amount: (wparam.0 >> 16) as i16 as f32 / WHEEL_DELTA as f32,
                    key_modifier,
                },
            );

            return LRESULT(0);
        }

        unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
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
                    .CreateColorBrushWithColor(
                        crate::DRAG_PREVIEW_POPOVER_BG_COLOR.windows_native_color(),
                    )
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

    fn show(&self, base: HWND, rect: &Rect<LogicalUnit>) {
        unsafe {
            // デスクトップ座標で指定になるので置き換え
            let scale = GetDpiForWindow(base) as f32 / 96.0;
            let pos = rect.left_top().to_pixels_round(scale);
            let size = rect.size().to_pixels_ceil(scale);
            let mut p = [POINT { x: pos.x, y: pos.y }];
            MapWindowPoints(Some(base), None, &mut p);
            let [POINT { x, y }] = p;

            // 影のぶんだけ余分に設定する
            SetWindowPos(
                self.w,
                None,
                x - 32,
                y - 32,
                (size.width + 64) as _,
                (size.height + 64) as _,
                SWP_NOZORDER | SWP_NOACTIVATE,
            )
            .expect("setwindowpos");
            self.root_visual
                .SetSize(Vector2::new(size.width as _, size.height as _))
                .expect("drag.visual.set_size");
            let _ = ShowWindow(self.w, SW_SHOWNOACTIVATE);
        }
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

        // 影のぶんだけずらして設定する
        unsafe {
            SetWindowPos(
                self.w,
                None,
                x - 32,
                y - 32,
                (size.width + 32 * 2) as _,
                (size.height + 32 * 2) as _,
                SWP_NOZORDER | SWP_NOACTIVATE,
            )
            .expect("setwindowpos");
        }
        self.root_visual
            .SetSize(Vector2::new(size.width as _, size.height as _))
            .expect("drag.visual.set_size");
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
impl ApplicationContext {
    pub fn new() -> Self {
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

pub struct SystemLink<'sys> {
    pub font_set: *const FontSet,
    pub vk_device: *const VulkanDevice<'sys>,
    pub rt_sender: RenderMessageSender,
    pub event_dispatcher: *mut LogicFiberEventDispatcher,
    pub app_context: &'sys ApplicationContext,
    pub pointer_hovering_timer: *const WaitableTimer,
    pub flyout_surface_context: flyout_surface::SharedState,
}
impl SystemLink<'_> {
    #[inline(always)]
    pub fn dispatch_event(&self, event: Event) {
        unsafe { &*self.event_dispatcher }.dispatch(event);
    }

    #[inline(always)]
    pub fn event_dispatcher(&self) -> &LogicFiberEventDispatcher {
        unsafe { &*self.event_dispatcher }
    }

    #[inline(always)]
    pub fn rt_sender(&self) -> &RenderMessageSender {
        &self.rt_sender
    }

    #[inline(always)]
    pub fn font_set(&self) -> &FontSet {
        unsafe { &*self.font_set }
    }

    #[inline(always)]
    pub const fn needs_app_menu_in_surface(&self) -> bool {
        // Windowsは常に必要
        true
    }

    pub fn create_main_window(
        &mut self,
        mode: MainWindowOpenMode,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
        delayed_render_messages: &mut Vec<RenderMessage>,
    ) -> WindowHandle {
        let (pos, size, initial_maximized);
        match mode {
            MainWindowOpenMode::Restore(WindowGeometryState::Maximized { monitor_index }) => {
                let primary_monitor = unsafe {
                    MonitorFromWindow(HWND(core::ptr::null_mut()), MONITOR_DEFAULTTOPRIMARY)
                };

                let mut found_hm_left_top = None;
                let mut primary_hm_left_top = None;
                let mut enum_index = 0;
                enumerate_display_monitors(|hm, r| {
                    if hm == primary_monitor {
                        primary_hm_left_top = Some(Point::new_pixels(r.left, r.top));
                    }

                    if enum_index == monitor_index {
                        found_hm_left_top = Some(Point::new_pixels(r.left, r.top));
                    }

                    enum_index += 1;
                    EnumerateDisplayMonitorContinuous::Continue
                });

                pos = Some(found_hm_left_top.unwrap_or_else(|| {
                    primary_hm_left_top.unwrap_or_else(|| {
                        tracing::error!("no primary monitor enumerated?");
                        Point::new_pixels(0, 0)
                    })
                }));
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
        };

        let ht = ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });
        let w = NativeWindow::new(
            self.app_context,
            WindowType::Main {},
            pos,
            size,
            initial_maximized,
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            ht,
            unsafe { &*self.event_dispatcher }.clone(),
            keyboard_focus_registry,
        );
        let h = w.make_handle();
        ht_manager.get_data_mut(ht).root_of_window = Some(h);

        let vk_surface = w.create_vk_surface(unsafe { &*self.vk_device });
        delayed_render_messages.push(RenderMessage::NewWindow(NewWindowData {
            key: h,
            vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
        }));

        unsafe {
            let _ = ShowWindow(h.0, SW_SHOWNORMAL);
        }

        h
    }

    pub fn prelaunch(&self, _handle: WindowHandle) {}

    pub fn open_window<'h>(
        &mut self,
        mode: SubWindowOpenMode,
        composite_tree: &mut CompositeTree<SyncEvent>,
        hit_tree: &mut HitTestTreeManager<'h>,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
        delayed_render_messages: &mut Vec<RenderMessage>,
        setup_contents: impl FnOnce(
            WindowHandle,
            &mut CompositeTree<SyncEvent>,
            &mut HitTestTreeManager<'h>,
            &mut KeyboardFocusTokenRegistry,
            &mut Self,
        ),
    ) -> WindowHandle {
        let (initial_pos, initial_size, initial_maximized);
        match mode {
            SubWindowOpenMode::Restore(WindowGeometryState::Maximized { monitor_index }) => {
                let primary_monitor = unsafe {
                    MonitorFromWindow(HWND(core::ptr::null_mut()), MONITOR_DEFAULTTOPRIMARY)
                };

                let mut found_hm_left_top = None;
                let mut primary_monitor_left_top = None;
                let mut enum_index = 0;
                enumerate_display_monitors(|hm, r| {
                    if hm == primary_monitor {
                        primary_monitor_left_top = Some(Point::new_pixels(r.left, r.top));
                    }

                    if enum_index == monitor_index {
                        found_hm_left_top = Some(Point::new_pixels(r.left, r.top));
                    }

                    enum_index += 1;
                    EnumerateDisplayMonitorContinuous::Continue
                });
                let target_left_top = found_hm_left_top.unwrap_or_else(|| {
                    primary_monitor_left_top.unwrap_or_else(|| {
                        tracing::error!("no primary monitor enumerated?");
                        Point::new_pixels(0, 0)
                    })
                });

                initial_pos = Some(target_left_top);
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
                let mut grect_lt = [rect
                    .left_top()
                    .to_pixels_round(position_ref_window.ui_scale_factor())
                    .to_win32()];
                unsafe {
                    MapWindowPoints(Some(position_ref_window.0), None, &mut grect_lt);
                }

                initial_pos = Some(Point::from_win32(grect_lt[0]));
                initial_size = Some(
                    rect.size()
                        .to_pixels_ceil(position_ref_window.ui_scale_factor()),
                );
                initial_maximized = false;
            }
        };

        let ht = hit_tree.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });
        let w = NativeWindow::new(
            self.app_context,
            WindowType::Sub,
            initial_pos,
            initial_size,
            initial_maximized,
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            ht,
            unsafe { &*self.event_dispatcher }.clone(),
            keyboard_focus_registry,
        );
        let h = w.make_handle();
        hit_tree.get_data_mut(ht).root_of_window = Some(h);

        let vk_surface = w.create_vk_surface(unsafe { &*self.vk_device });
        delayed_render_messages.push(RenderMessage::NewWindow(NewWindowData {
            key: h,
            vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
        }));

        setup_contents(h, composite_tree, hit_tree, keyboard_focus_registry, self);
        unsafe {
            let _ = ShowWindow(w.hwnd, SW_SHOW);
        }
        h
    }

    pub fn close_window(
        &mut self,
        mut window_handle: WindowHandle,
        composite_tree: &mut CompositeTree<SyncEvent>,
        hit_tree: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        let (done_event_sender, done_event_receiver) = std::sync::mpsc::channel();
        self.rt_sender
            .send(RenderMessage::DestroyWindow(
                window_handle,
                done_event_sender,
            ))
            .expect("rt_sender.send.destroy_window");
        done_event_receiver
            .recv()
            .expect("done_event_receiver.recv");

        composite_tree.free_all(window_handle.ct_root());
        hit_tree.free_all(window_handle.ht_root());
        keyboard_focus_registry.release_group(window_handle.state().root_focus_group);
        window_handle.destroy();
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

    #[inline(always)]
    pub fn notify_ui_scale_changes_to_render(&self, window: WindowHandle, new_scale: f32) {
        *window
            .state()
            .latest_ui_scale_changes
            .lock()
            .expect("poisoned") = Some(new_scale);
    }

    pub fn set_pointer_hovering_timeout(&mut self) {
        unsafe { &*self.pointer_hovering_timer }
            .set_oneshot_relative(crate::input::POINTER_HOVER_TIMEOUT_MS as _)
            .expect("pointer_hovering_timer.set");
    }

    pub fn kill_pointer_hovering_timeout(&mut self) {
        unsafe { &*self.pointer_hovering_timer }
            .cancel()
            .expect("pointer_hovering_timer.cancel");
    }

    pub fn begin_pane_drag(
        &self,
        initiator_surface: WindowHandle,
        _pointer: &PointerID,
        _offset: Point<LogicalUnit>,
        rect: &Rect<LogicalUnit>,
    ) {
        unsafe {
            SetCapture(initiator_surface.0);
        }

        self.app_context
            .drag_preview_popover
            .show(initiator_surface.0, rect);
        self.app_context.pane_dragging.set(true);
    }

    pub fn update_pane_drag(&self, on_surface: WindowHandle, rect: &Rect<LogicalUnit>) {
        self.app_context
            .drag_preview_popover
            .set_rect(on_surface.0, rect);
    }

    pub fn end_pane_drag(&self) {
        unsafe {
            ReleaseCapture().expect("win32.release_capture");
        }

        self.app_context.pane_dragging.set(false);
        self.app_context.drag_preview_popover.hide();
    }
}

pub trait TextProvider {
    fn text(&self, range: CoreTextRange) -> windows_core::Result<HSTRING>;
    fn selection(&self, req: &CoreTextSelectionRequest) -> windows_core::Result<()>;
}
pub trait CoreTextDeferrableEventHandler {
    fn layout(
        &self,
        ctx: &mut InputEventContext,
        req: &CoreTextLayoutRequest,
    ) -> windows_core::Result<()>;
    fn text_updating(
        &self,
        ctx: &mut InputEventContext,
        e: &CoreTextTextUpdatingEventArgs,
    ) -> windows_core::Result<()>;
    fn format_updating<'x>(
        &'x self,
        ctx: &mut InputEventContext,
        e: &CoreTextFormatUpdatingEventArgs,
    ) -> windows_core::Result<()>;
}
pub struct NativeTextInputContext {
    edit_context: CoreTextEditContext,
}
impl NativeTextInputContext {
    pub fn new(system_link: &SystemLink) -> Self {
        let edit_context = system_link
            .app_context
            .ctm
            .CreateEditContext()
            .expect("CoreTextServicesManager.CreateEditContext");

        Self { edit_context }
    }

    pub fn bind_action<T: TextProvider + 'static>(
        &self,
        system_link: &SystemLink,
        text_provider: &Rc<T>,
        layout_provider_ht: HitTestTreeRef,
    ) {
        let caller_thread_id = std::thread::current().id();
        self.edit_context
            .LayoutRequested(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextLayoutRequestedEventArgs,
            >::new({
                let event_dispatcher =
                    std::sync::atomic::AtomicPtr::new(system_link.event_dispatcher);
                move |_sender, e| {
                    let e = e.ok().expect("event_args.null");
                    let req = e.Request().expect("layout_requested.event_args.request");

                    assert_eq!(
                        std::thread::current().id(),
                        caller_thread_id,
                        "not main thread"
                    );

                    let ed_ref = unsafe { &**event_dispatcher.as_ptr() };
                    if ed_ref.can_immediate_dispatch() {
                        ed_ref.dispatch(Event::CoreTextLayoutRequested {
                            ht: layout_provider_ht,
                            request: req,
                            deferral: None,
                        });
                    } else {
                        let deferral = req.GetDeferral()?;
                        ed_ref.dispatch(Event::CoreTextLayoutRequested {
                            ht: layout_provider_ht,
                            request: req,
                            deferral: Some(deferral),
                        });
                    }

                    Ok(())
                }
            }))
            .expect("edit_context.layout_requested");
        self.edit_context
            .TextRequested(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextTextRequestedEventArgs,
            >::new({
                let text_provider = std::sync::atomic::AtomicPtr::new(Rc::as_ptr(&text_provider)
                    as *const T
                    as *mut T);
                move |_sender, e| {
                    let e = e.ok().expect("event_args.null");
                    let req = e.Request().expect("text_requested.event_args.request");
                    tracing::trace!(
                        req.range = ?req.Range(),
                        "edit_context.text_requested"
                    );

                    assert_eq!(
                        std::thread::current().id(),
                        caller_thread_id,
                        "not main thread"
                    );

                    req.SetText(&unsafe { &**text_provider.as_ptr() }.text(req.Range()?)?)?;
                    Ok(())
                }
            }))
            .expect("edit_context.text_requested");
        self.edit_context
            .TextUpdating(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextTextUpdatingEventArgs,
            >::new({
                let event_dispatcher =
                    std::sync::atomic::AtomicPtr::new(system_link.event_dispatcher);
                move |_sender, e| {
                    let e = e.cloned().expect("event_args.null");

                    assert_eq!(
                        std::thread::current().id(),
                        caller_thread_id,
                        "not main thread"
                    );

                    let ed_ref = unsafe { &**event_dispatcher.as_ptr() };
                    if ed_ref.can_immediate_dispatch() {
                        ed_ref.dispatch(Event::CoreTextTextUpdating {
                            ht: layout_provider_ht,
                            e,
                            deferral: None,
                        });
                    } else {
                        let deferral = e.GetDeferral()?;
                        ed_ref.dispatch(Event::CoreTextTextUpdating {
                            ht: layout_provider_ht,
                            e,
                            deferral: Some(deferral),
                        });
                    }

                    Ok(())
                }
            }))
            .expect("edit_context.text_updating");
        self.edit_context
            .CompositionStarted(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextCompositionStartedEventArgs,
            >::new(|_sender, e| {
                let _e = e.ok().expect("event_args.null");
                tracing::trace!("edit_context.composition_started");
                Ok(())
            }))
            .expect("edit_context.composition_started");
        self.edit_context
            .CompositionCompleted(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextCompositionCompletedEventArgs,
            >::new(move |_sender, _e| {
                // let e = e.ok().expect("event_args.null");
                tracing::trace!(
                    // composition_segments = ?e.CompositionSegments(),
                    // composition_segments.len = ?e.CompositionSegments().and_then(|x| x.Size()),
                    "edit_context.composition_completed"
                );

                /*for segment in e.CompositionSegments().expect("edit_context.composition_copmleted.composition_segments") {
                    tracing::trace!(
                        preconversion_string = ?segment.PreconversionString().map(|x| x.to_string_lossy()),
                        range = ?segment.Range(),
                        "edit_context.composition_completed.segment"
                    );
                }*/

                Ok(())
            }))
            .expect("edit_context.composition_completed");
        self.edit_context
            .FormatUpdating(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextFormatUpdatingEventArgs,
            >::new({
                let event_dispatcher =
                    std::sync::atomic::AtomicPtr::new(system_link.event_dispatcher);
                move |_sender, e| {
                    let e = e.cloned().expect("event_args.null");

                    assert_eq!(
                        std::thread::current().id(),
                        caller_thread_id,
                        "not main thread"
                    );

                    let ed_ref = unsafe { &**event_dispatcher.as_ptr() };
                    if ed_ref.can_immediate_dispatch() {
                        ed_ref.dispatch(Event::CoreTextFormatUpdating {
                            ht: layout_provider_ht,
                            e,
                            deferral: None,
                        });
                    } else {
                        let deferral = e.GetDeferral()?;
                        ed_ref.dispatch(Event::CoreTextFormatUpdating {
                            ht: layout_provider_ht,
                            e,
                            deferral: Some(deferral),
                        });
                    }

                    Ok(())
                }
            }))
            .expect("edit_context.format_updating");
        self.edit_context
            .FocusRemoved(&TypedEventHandler::<
                CoreTextEditContext,
                windows_core::IInspectable,
            >::new(|_sender, e| {
                tracing::trace!(e = ?e.ok(), "edit_context.focus_removed");

                Ok(())
            }))
            .expect("edit_context.focus_removed");
        self.edit_context
            .NotifyFocusLeaveCompleted(
                &TypedEventHandler::<CoreTextEditContext, IInspectable>::new(|_sender, e| {
                    tracing::trace!(e = ?e.ok(), "edit_context.notify_focus_leave_completed");

                    Ok(())
                }),
            )
            .expect("edit_context.notify_focus_leave_completed");
        self.edit_context
            .SelectionRequested(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextSelectionRequestedEventArgs,
            >::new({
                let text_provider = std::sync::atomic::AtomicPtr::new(Rc::as_ptr(&text_provider)
                    as *const T
                    as *mut T);
                move |_sender, e| {
                    let e = e.ok().expect("event_args.null");
                    let req = e
                        .Request()
                        .expect("edit_context.selection_requested.event_args.request");
                    tracing::trace!("edit_context.selection_requested");

                    assert_eq!(
                        std::thread::current().id(),
                        caller_thread_id,
                        "not main thread"
                    );

                    unsafe { &**text_provider.as_ptr() }.selection(&req)?;
                    Ok(())
                }
            }))
            .expect("edit_context.selection_requested");
        self.edit_context
            .SelectionUpdating(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextSelectionUpdatingEventArgs,
            >::new(|_sender, e| {
                let e = e.ok().expect("event_args.null");
                tracing::trace!(
                    // is_canceled = ?e.IsCanceled(),
                    selection = ?e.Selection(),
                    "edit_context.selection_updating"
                );

                Ok(())
            }))
            .expect("edit_context.selection_updating");
    }

    pub fn notify_focus_enter(&self) {
        self.edit_context
            .NotifyFocusEnter()
            .expect("edit_context.NotifyFocusEnter");
    }

    pub fn notify_focus_leave(&self) {
        self.edit_context
            .NotifyFocusLeave()
            .expect("edit_context.NotifyFocusLeave");
    }

    pub fn notify_layout_changed(&self) {
        self.edit_context
            .NotifyLayoutChanged()
            .expect("edit_context.NotifyLayoutChanged");
    }

    pub fn notify_selection_changed(&self, start_acp: i32, end_acp: i32) {
        self.edit_context
            .NotifySelectionChanged(CoreTextRange {
                StartCaretPosition: start_acp,
                EndCaretPosition: end_acp,
            })
            .expect("edit_context.NotifySelectionChanged");
    }
}
