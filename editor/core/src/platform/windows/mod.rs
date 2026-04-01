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
        Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, POINT, RECT, WPARAM},
        Graphics::{
            Dwm::{
                DWMWA_EXTENDED_FRAME_BOUNDS, DwmExtendFrameIntoClientArea, DwmGetWindowAttribute,
            },
            Gdi::{
                GetMonitorInfoW, HBRUSH, MONITOR_DEFAULTTONEAREST, MONITORINFO, MapWindowPoints,
                MonitorFromWindow, PtInRect,
            },
        },
        System::{
            self,
            Threading::GetCurrentThreadId,
            WinRT::{
                Composition::ICompositorDesktopInterop, CreateDispatcherQueueController,
                DQTAT_COM_ASTA, DQTYPE_THREAD_CURRENT, DispatcherQueueOptions,
            },
        },
        UI::{
            Controls::{MARGINS, WM_MOUSELEAVE},
            HiDpi::GetDpiForWindow,
            Input::KeyboardAndMouse::{
                ReleaseCapture, SetCapture, TME_LEAVE, TME_NONCLIENT, TRACKMOUSEEVENT,
                TrackMouseEvent,
            },
            WindowsAndMessaging::{
                CW_USEDEFAULT, CallNextHookEx, CreateWindowExW, DefWindowProcW, DestroyWindow,
                GET_CLASS_LONG_INDEX, GetClassLongPtrW, GetClientRect, GetCursorPos,
                GetSystemMetrics, GetWindowLongPtrW, GetWindowRect, GetWindowWord, HCURSOR, HHOOK,
                HICON, HTBOTTOM, HTBOTTOMLEFT, HTBOTTOMRIGHT, HTCAPTION, HTCLIENT, HTCLOSE, HTLEFT,
                HTMAXBUTTON, HTMINBUTTON, HTRIGHT, HTTOP, HTTOPLEFT, HTTOPRIGHT, IDC_ARROW,
                IDC_HAND, IDC_IBEAM, IDC_SIZEWE, IDI_APPLICATION, IsZoomed, LoadCursorW, LoadIconW,
                NCCALCSIZE_PARAMS, PostMessageW, PostQuitMessage, SC_CLOSE, SC_MAXIMIZE,
                SC_MINIMIZE, SC_RESTORE, SIZE_MAXIMIZED, SIZE_RESTORED, SM_CXSIZEFRAME,
                SM_CYSIZEFRAME, SW_HIDE, SW_SHOW, SW_SHOWNOACTIVATE, SW_SHOWNORMAL,
                SWP_FRAMECHANGED, SWP_HIDEWINDOW, SWP_NOACTIVATE, SWP_NOMOVE, SWP_NOSIZE,
                SWP_NOZORDER, SWP_SHOWWINDOW, SetClassLongPtrW, SetCursor, SetWindowLongPtrW,
                SetWindowPos, SetWindowsHookExW, ShowWindow, UnhookWindowsHookEx, WH_MOUSE,
                WINDOW_LONG_PTR_INDEX, WINDOWPOS, WM_ACTIVATE, WM_CHAR, WM_CLOSE, WM_CREATE,
                WM_DESTROY, WM_DPICHANGED, WM_KEYDOWN, WM_KEYUP, WM_KILLFOCUS, WM_LBUTTONDOWN,
                WM_LBUTTONUP, WM_MBUTTONDBLCLK, WM_MOUSEMOVE, WM_MOVE, WM_NCCALCSIZE, WM_NCHITTEST,
                WM_NCLBUTTONDOWN, WM_NCLBUTTONUP, WM_NCMOUSELEAVE, WM_NCMOUSEMOVE,
                WM_NCRBUTTONDOWN, WM_RBUTTONDOWN, WM_RBUTTONUP, WM_SETFOCUS, WM_SIZE,
                WM_SYSCOMMAND, WM_WINDOWPOSCHANGED, WNDCLASS_STYLES, WNDCLASSEXW, WS_EX_APPWINDOW,
                WS_EX_LAYERED, WS_EX_NOACTIVATE, WS_EX_NOREDIRECTIONBITMAP, WS_EX_TOPMOST,
                WS_EX_TRANSPARENT, WS_OVERLAPPEDWINDOW, WS_POPUP, WindowFromPoint,
            },
        },
    },
};
use windows_core::{HSTRING, IInspectable, Interface, PCWSTR, h, w};
use windows_numerics::{Vector2, Vector3};

use std::{rc::Rc, sync::Mutex};

use crate::{
    Event, LogicFiberEventDispatcher, SyncEvent, WindowType,
    bindgen::Microsoft::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    graphics::{VulkanDevice, VulkanSurface},
    input::{
        InputEventContext, KeyInputCode, PerWindowKeyboardFocusState, PointerInputManager,
        PointerInputUnit, ShellPointerActions,
        hittest::{
            CursorShape, HitTestTreeCreate, HitTestTreeData, HitTestTreeManager, HitTestTreeRef,
            PointerButton,
        },
    },
    rendering::{
        NewContextMenuData, NewWindowData, NewWindowVulkanSurface, RenderMessage,
        composite::{CompositeRect, CompositeTree, CompositeTreeRef},
        text::RootFontSet,
    },
    utils::{
        LogicalUnit, PixelsUnit, Point, Size,
        platform::windows::{WindowByClassIter, current_instance_handle, register_class},
    },
};

#[derive(Clone, Copy, PartialEq, Eq)]
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
    pub unsafe fn extra_data_mut<'a, T>(&'a self) -> &'a mut T {
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
    pub fn ui_scale_factor(&self) -> f32 {
        unsafe { GetDpiForWindow(self.0) as f32 / 96.0 }
    }

    #[inline(always)]
    pub fn composite_root(&self) -> CompositeTreeRef {
        self.state().composite_root
    }

    #[inline(always)]
    pub fn ht_root(&self) -> HitTestTreeRef {
        self.state().ht_root
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

#[derive(Clone, Copy)]
pub struct PointerID();

pub struct WindowClassSet {
    hinstance: HINSTANCE,
    main: u16,
}
impl WindowClassSet {
    fn register(hinstance: HINSTANCE) -> Self {
        let main = NativeWindow::register_class(hinstance);

        Self { hinstance, main }
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
        wc_set: &WindowClassSet,
        window_type: WindowType,
        composite_root: CompositeTreeRef,
        ht_root: HitTestTreeRef,
        event_dispatcher: LogicFiberEventDispatcher,
    ) -> Self {
        let w = unsafe {
            CreateWindowExW(
                WS_EX_APPWINDOW,
                PCWSTR(core::ptr::without_provenance(wc_set.main as _)),
                w!("Peridot Marble Editor"),
                WS_OVERLAPPEDWINDOW,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                None,
                None,
                Some(wc_set.hinstance),
                None,
            )
            .expect("CreateWindowExW")
        };
        let event_handler = Box::new(WindowEventHandler {
            state: WindowState {
                r#type: window_type,
                content_scale: unsafe { GetDpiForWindow(w) as f32 / 96.0 },
                composite_root,
                ht_root,
                latest_ui_scale_changes: Mutex::new(None),
                keyboard_focus_state: PerWindowKeyboardFocusState::new(),
                destroying: false,
            },
            event_dispatcher,
            edit_context: None,
        });
        unsafe {
            SetWindowLongPtrW(
                w,
                WindowEventHandler::LONG_PTR_INDEX,
                Box::into_raw(event_handler).addr().cast_signed(),
            );
        }

        Self {
            hinstance: wc_set.hinstance,
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
    event_dispatcher: LogicFiberEventDispatcher,
    edit_context: Option<CoreTextEditContext>,
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
        self.event_dispatcher.dispatch(Event::PointerUp {
            window: WindowHandle(hwnd),
            pointer_id: PointerID(),
            button: PointerButton::Secondary,
        });
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

        if msg == WM_KEYDOWN {
            tracing::trace!(keycode = wparam.0, "keydown");
            Self::get_for_window(hwnd)
                .event_dispatcher
                .dispatch(Event::KeyDown {
                    code: match wparam.0 {
                        v if v == windows::Win32::UI::Input::KeyboardAndMouse::VK_LEFT.0 as _ => {
                            KeyInputCode::LeftArrow
                        }
                        v if v == windows::Win32::UI::Input::KeyboardAndMouse::VK_RIGHT.0 as _ => {
                            KeyInputCode::RightArrow
                        }
                        v if v == windows::Win32::UI::Input::KeyboardAndMouse::VK_UP.0 as _ => {
                            KeyInputCode::UpArrow
                        }
                        v if v == windows::Win32::UI::Input::KeyboardAndMouse::VK_DOWN.0 as _ => {
                            KeyInputCode::DownArrow
                        }
                        _ => KeyInputCode::UnknownNativeCode(wparam.0 as _),
                    },
                    window: WindowHandle(hwnd),
                });

            return LRESULT(0);
        }

        if msg == WM_KEYUP {
            tracing::trace!(keycode = wparam.0, "keyup");
            Self::get_for_window(hwnd)
                .event_dispatcher
                .dispatch(Event::KeyUp {
                    code: match wparam.0 {
                        v if v == windows::Win32::UI::Input::KeyboardAndMouse::VK_LEFT.0 as _ => {
                            KeyInputCode::LeftArrow
                        }
                        v if v == windows::Win32::UI::Input::KeyboardAndMouse::VK_RIGHT.0 as _ => {
                            KeyInputCode::RightArrow
                        }
                        v if v == windows::Win32::UI::Input::KeyboardAndMouse::VK_UP.0 as _ => {
                            KeyInputCode::UpArrow
                        }
                        v if v == windows::Win32::UI::Input::KeyboardAndMouse::VK_DOWN.0 as _ => {
                            KeyInputCode::DownArrow
                        }
                        _ => KeyInputCode::UnknownNativeCode(wparam.0 as _),
                    },
                    window: WindowHandle(hwnd),
                });

            return LRESULT(0);
        }

        if msg == WM_CHAR {
            tracing::trace!(keycode = wparam.0, "char input");
            Self::get_for_window(hwnd)
                .event_dispatcher
                .dispatch(Event::KeyDown {
                    code: KeyInputCode::Character(unsafe {
                        char::from_u32_unchecked(wparam.0 as _)
                    }),
                    window: WindowHandle(hwnd),
                });

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
            if let Some(state) = Self::try_get_for_window(hwnd) {
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
            || (msg == WM_NCLBUTTONUP && Self::is_application_handled_hittest(wparam.0 as _))
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
                    dwFlags: TME_LEAVE | TME_NONCLIENT,
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
            Self::get_for_window(hwnd)
                .event_dispatcher
                .dispatch(Event::PointerLeaveWindow {
                    window: WindowHandle(hwnd),
                    pointer_id: PointerID(),
                });

            return LRESULT(0);
        }

        unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
    }
}

pub struct DragPreviewPopoverHandle {
    w: HWND,
    base_window_handle: core::cell::Cell<HWND>,
    _composition_target: DesktopWindowTarget,
    root_visual: SpriteVisual,
}
impl Drop for DragPreviewPopoverHandle {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = unsafe { DestroyWindow(self.w) } {
            tracing::error!(reason = %e, "dragPreviewPopover.destroyNative");
        }
    }
}
impl DragPreviewPopoverHandle {
    pub fn bind_position_base_window(&self, window: WindowHandle) {
        self.base_window_handle.set(window.0);
    }

    pub fn new(app: &ApplicationContext) -> Self {
        let atom_drag_floating = unsafe {
            register_class(&WNDCLASSEXW {
                cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
                style: WNDCLASS_STYLES(0),
                cbClsExtra: 0,
                cbWndExtra: 0,
                lpfnWndProc: Some(Self::wndproc),
                hInstance: app.hinstance,
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
                Some(app.hinstance),
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
        let effect_factory = app
            .native_compositor
            .CreateEffectFactory(&fx)
            .expect("drag.fx.create_factory");
        let backdrop_brush = app
            .native_compositor
            .CreateBackdropBrush()
            .expect("drag.backdrop_brush.create");
        let blur_brush = effect_factory.CreateBrush().expect("drag.fx_brush.create");
        blur_brush
            .SetSourceParameter(h!("Source"), &backdrop_brush)
            .expect("drag.fx.set_blur_source");
        let blur_visual = app
            .native_compositor
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
                let x = app
                    .native_compositor
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
        let color_tint_visual = app
            .native_compositor
            .CreateSpriteVisual()
            .expect("drag.visual.color_tint.create");
        color_tint_visual
            .SetBrush(
                &app.native_compositor
                    .CreateColorBrushWithColor(
                        DragPreviewPopoverHandle::BG_COLOR.windows_native_color(),
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
            app.native_compositor
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
            base_window_handle: core::cell::Cell::new(HWND(core::ptr::null_mut())),
            _composition_target: composition_target,
            root_visual: blur_visual,
        }
    }

    pub fn show(&self, pos: &Point<PointerInputUnit>, size: &Size<LogicalUnit>) {
        unsafe {
            // デスクトップ座標で指定になるので置き換え
            let scale = GetDpiForWindow(self.base_window_handle.get()) as f32 / 96.0;
            let pos = pos.to_pixels_round(scale);
            let size = size.to_pixels_ceil(scale);
            let mut p = [POINT { x: pos.x, y: pos.y }];
            MapWindowPoints(Some(self.base_window_handle.get()), None, &mut p);
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

    pub fn r#move(&self, pos: &Point<PointerInputUnit>) {
        unsafe {
            // デスクトップ座標で指定になるので置き換え
            let scale = GetDpiForWindow(self.base_window_handle.get()) as f32 / 96.0;
            let pos = pos.to_pixels_round(scale);
            let mut p = [POINT { x: pos.x, y: pos.y }];
            MapWindowPoints(Some(self.base_window_handle.get()), None, &mut p);
            let [POINT { x, y }] = p;

            // 影のぶんだけずらして設定する
            SetWindowPos(
                self.w,
                None,
                x - 32,
                y - 32,
                0,
                0,
                SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOSIZE,
            )
            .expect("setwindowpos");
        }
    }

    pub fn hide(&self) {
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
    ctm: CoreTextServicesManager,
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

        Self {
            hinstance,
            wc_set,
            _dispatcher_queue: dispatcher_queue,
            native_compositor,
            ctm,
        }
    }
}

pub struct SystemLink<'sys> {
    pub drag_preview_popover: DragPreviewPopoverHandle,
    pub root_font_set: *const RootFontSet,
    pub vk_device: *const VulkanDevice<'sys>,
    pub rt_sender: std::sync::mpsc::Sender<RenderMessage>,
    pub event_dispatcher: *mut LogicFiberEventDispatcher,
    pub app_context_ptr: *const ApplicationContext,
}
impl SystemLink<'_> {
    pub fn init_main_window(
        vk_device: &VulkanDevice,
        dispatcher: LogicFiberEventDispatcher,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        rt_sender: &std::sync::mpsc::Sender<RenderMessage>,
        app: &ApplicationContext,
    ) -> WindowHandle {
        let ht = ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });
        let w = NativeWindow::new(
            &app.wc_set,
            WindowType::Main {},
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            ht,
            dispatcher,
        );
        let main_window_handle = w.make_handle();
        ht_manager.get_data_mut(ht).root_of_window = Some(main_window_handle);

        let vk_surface = w.create_vk_surface(vk_device);
        rt_sender
            .send(RenderMessage::NewWindow(NewWindowData {
                key: main_window_handle,
                vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            }))
            .expect("rt_sender.send");

        main_window_handle
    }

    pub fn postinit_main_window(
        handle: WindowHandle,
        canonical_dispatcher: LogicFiberEventDispatcher,
    ) {
        WindowEventHandler::get_for_window(handle.0).event_dispatcher = canonical_dispatcher;
    }

    pub fn prelaunch(handle: WindowHandle) {
        unsafe {
            let _ = ShowWindow(handle.0, SW_SHOWNORMAL);
        }
    }

    #[inline(always)]
    pub fn dispatch_event(&self, event: Event) {
        unsafe { &*self.event_dispatcher }.dispatch(event);
    }

    #[inline(always)]
    pub fn rt_sender(&self) -> &std::sync::mpsc::Sender<RenderMessage> {
        &self.rt_sender
    }

    #[inline(always)]
    pub fn root_font_set(&self) -> &RootFontSet {
        unsafe { &*self.root_font_set }
    }

    #[inline(always)]
    pub fn drag_preview_popover(&self) -> &DragPreviewPopoverHandle {
        &self.drag_preview_popover
    }

    pub fn open_window<'h, HT: HitTestTreeCreate<'h> + ?Sized>(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        hit_tree: &mut HT,
        setup_contents: impl FnOnce(WindowHandle, &mut CompositeTree<SyncEvent>, &mut HT),
    ) -> WindowHandle {
        let w = NativeWindow::new(
            unsafe { &(*self.app_context_ptr).wc_set },
            WindowType::Sub,
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            hit_tree.create(HitTestTreeData {
                width_adjustment_factor: 1.0,
                height_adjustment_factor: 1.0,
                ..Default::default()
            }),
            unsafe { &*self.event_dispatcher }.clone(),
        );
        let h = w.make_handle();

        let vk_surface = w.create_vk_surface(unsafe { &*self.vk_device });
        self.rt_sender
            .send(RenderMessage::NewWindow(NewWindowData {
                key: h,
                vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            }))
            .expect("rt_sender.send");

        setup_contents(h, composite_tree, hit_tree);
        unsafe {
            let _ = ShowWindow(w.hwnd, SW_SHOW);
        }
        h
    }

    pub fn close_window(
        &self,
        mut window_handle: WindowHandle,
        composite_tree: &mut CompositeTree<SyncEvent>,
        hit_tree: &mut HitTestTreeManager,
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

        composite_tree.free_all(window_handle.composite_root());
        hit_tree.free_all(window_handle.ht_root());
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
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ContextMenuHandle(HWND);
impl core::hash::Hash for ContextMenuHandle {
    #[inline(always)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.0.addr().hash(state)
    }
}
unsafe impl Sync for ContextMenuHandle {}
unsafe impl Send for ContextMenuHandle {}
impl ContextMenuHandle {
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

        Size::new_pixels(r.right as _, r.bottom as _)
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

pub struct ContextMenuInstance {
    hwnd: HWND,
}
impl ContextMenuInstance {
    const WINDOW_PTR_COMPOSITE_ROOT: WINDOW_LONG_PTR_INDEX = WINDOW_LONG_PTR_INDEX(0);

    fn register_class(hinstance: HINSTANCE) -> u16 {
        unsafe {
            register_class(&WNDCLASSEXW {
                cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
                cbWndExtra: core::mem::size_of::<usize>() as _,
                lpfnWndProc: Some(Self::wndproc),
                hInstance: hinstance,
                lpszClassName: w!("ContextMenu"),
                ..core::mem::MaybeUninit::zeroed().assume_init()
            })
            .expect("context_menu.register_class")
        }
    }

    pub fn new(
        syslink: &SystemLink,
        composite_tree: &mut CompositeTree<SyncEvent>,
        screen_pos: Point<PixelsUnit>,
    ) -> Self {
        let hinstance = current_instance_handle();
        let h = unsafe {
            CreateWindowExW(
                WS_EX_NOACTIVATE | WS_EX_TOPMOST,
                PCWSTR(ContextMenuSharedState::window_class() as _),
                w!(""),
                WS_POPUP,
                screen_pos.x,
                screen_pos.y,
                100,
                100,
                None,
                None,
                Some(hinstance),
                None,
            )
            .expect("context_menu.create_window")
        };
        let composite_root = composite_tree.create(CompositeRect {
            relative_size_adjustment: [1.0, 1.0],
            ..Default::default()
        });
        unsafe {
            SetWindowLongPtrW(
                h,
                Self::WINDOW_PTR_COMPOSITE_ROOT,
                core::mem::transmute(composite_root),
            );
        }
        syslink
            .rt_sender
            .send(RenderMessage::NewContextMenu(NewContextMenuData {
                w: ContextMenuHandle(h),
                vk_surface: NewWindowVulkanSurface(
                    VulkanSurface::new(unsafe { &*syslink.vk_device }, unsafe {
                        br::Win32SurfaceCreateInfo::new(
                            core::mem::transmute(hinstance),
                            core::mem::transmute(h),
                        )
                        .execute((&*syslink.vk_device).instance(), None)
                        .expect("vk_surface.create")
                    })
                    .unbound()
                    .1,
                ),
                composite_root,
            }))
            .expect("rt_sender.send");

        let _ = unsafe { ShowWindow(h, SW_SHOWNOACTIVATE) };
        Self { hwnd: h }
    }

    extern "system" fn wndproc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
        unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
    }
}

struct ContextMenuSharedState {
    window_class: u16,
    installed_hook: HHOOK,
    rt_sender: std::sync::mpsc::Sender<RenderMessage>,
    event_dispatcher: LogicFiberEventDispatcher,
}
impl Drop for ContextMenuSharedState {
    fn drop(&mut self) {
        if let Err(e) = unsafe { UnhookWindowsHookEx(self.installed_hook) } {
            tracing::error!(reason = %e, "UnhookWindowsHookEx");
        }
    }
}
impl ContextMenuSharedState {
    #[inline(always)]
    const fn get() -> &'static Self {
        unsafe { &*CONTEXT_MENU_SHARED_STATE }
    }

    #[inline(always)]
    const fn window_class() -> u16 {
        Self::get().window_class
    }

    // hiding by mouse hook: https://www.codeproject.com/Tips/751520/Custom-Context-Menu
    extern "system" fn mouse_hook(code: i32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
        if WM_LBUTTONDOWN as usize <= wparam.0 && wparam.0 <= WM_MBUTTONDBLCLK as usize {
            let mut p = core::mem::MaybeUninit::<POINT>::uninit();
            unsafe {
                GetCursorPos(p.as_mut_ptr()).expect("Failed to get cursor pos");
            }
            let p = unsafe { p.assume_init() };

            let has_pointing_menu =
                WindowByClassIter::new(PCWSTR(Self::window_class() as _)).any(|x| {
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

static mut CONTEXT_MENU_SHARED_STATE: *mut ContextMenuSharedState = core::ptr::null_mut();

pub fn initialize_context_menu(rt_sender: std::sync::mpsc::Sender<RenderMessage>) {
    let window_class = ContextMenuInstance::register_class(current_instance_handle());
    let installed_hook = unsafe {
        SetWindowsHookExW(
            WH_MOUSE,
            Some(ContextMenuSharedState::mouse_hook),
            None,
            GetCurrentThreadId(),
        )
        .expect("SetWindowsHookExW")
    };

    unsafe {
        CONTEXT_MENU_SHARED_STATE = Box::into_raw(Box::new(ContextMenuSharedState {
            window_class,
            installed_hook,
            rt_sender,
            // あとで初期化するので一旦適当に埋める
            #[allow(invalid_value)]
            event_dispatcher: core::mem::MaybeUninit::uninit().assume_init(),
        }));
    }
}

pub fn post_initialize_context_menu(event_dispatcher: LogicFiberEventDispatcher) {
    unsafe {
        (*CONTEXT_MENU_SHARED_STATE).event_dispatcher = event_dispatcher;
    }
}

pub fn pop_context_menu(
    syslink: &SystemLink,
    composite_tree: &mut CompositeTree<SyncEvent>,
    screen_pos: Point<PixelsUnit>,
) {
    ContextMenuInstance::new(syslink, composite_tree, screen_pos);
}

pub fn close_all_context_menus(composite_tree: &mut CompositeTree<SyncEvent>) {
    let window_handles =
        WindowByClassIter::new(PCWSTR(ContextMenuSharedState::window_class() as _))
            .collect::<Vec<_>>();
    for window_handle in window_handles {
        let (tx, rx) = std::sync::mpsc::channel::<()>();
        ContextMenuSharedState::get()
            .rt_sender
            .send(RenderMessage::DestroyContextMenu(
                ContextMenuHandle(window_handle),
                tx,
            ))
            .expect("rt_sender.send");
        rx.recv().expect("rx.recv");

        let composite_root: CompositeTreeRef = unsafe {
            core::mem::transmute(GetWindowLongPtrW(
                window_handle,
                ContextMenuInstance::WINDOW_PTR_COMPOSITE_ROOT,
            ))
        };
        composite_tree.free_all(composite_root);

        if let Err(e) = unsafe { DestroyWindow(window_handle) } {
            tracing::error!(reason = %e, "DestroyWindow");
        }
    }
}

pub fn finalize_context_menu() {
    unsafe {
        drop(Box::from_raw(core::ptr::replace(
            core::ptr::addr_of_mut!(CONTEXT_MENU_SHARED_STATE),
            core::ptr::null_mut(),
        )));
    }
}

pub fn pointer_pos(_p: PointerID) -> Point<PixelsUnit> {
    // WindowsではPointerIDは無視（マルチタッチ対応を本格的に考えないといけなくなった場合に考える）
    let mut p = core::mem::MaybeUninit::uninit();
    unsafe {
        GetCursorPos(p.as_mut_ptr()).expect("GetCursorPos");
    }
    let p = unsafe { p.assume_init() };

    Point::new_pixels(p.x, p.y)
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
    fn format_updating(
        &self,
        ctx: &mut InputEventContext,
        e: &CoreTextFormatUpdatingEventArgs,
    ) -> windows_core::Result<()>;
}
pub struct NativeTextInputContext {
    edit_context: CoreTextEditContext,
}
impl NativeTextInputContext {
    pub fn new(system_link: &SystemLink) -> Self {
        let edit_context = unsafe { &*system_link.app_context_ptr }
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
                let e = e.ok().expect("event_args.null");
                tracing::trace!("edit_context.composition_started");
                Ok(())
            }))
            .expect("edit_context.composition_started");
        self.edit_context
            .CompositionCompleted(&TypedEventHandler::<
                CoreTextEditContext,
                CoreTextCompositionCompletedEventArgs,
            >::new(move |_sender, e| {
                let e = e.ok().expect("event_args.null");
                tracing::trace!(
                    composition_segments = ?e.CompositionSegments(),
                    composition_segments.len = ?e.CompositionSegments().and_then(|x| x.Size()),
                    "edit_context.composition_completed"
                );

                for segment in e.CompositionSegments().expect("edit_context.composition_copmleted.composition_segments") {
                    tracing::trace!(
                        preconversion_string = ?segment.PreconversionString().map(|x| x.to_string_lossy()),
                        range = ?segment.Range(),
                        "edit_context.composition_completed.segment"
                    );
                }

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
