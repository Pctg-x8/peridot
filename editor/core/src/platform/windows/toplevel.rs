use core::{cell::UnsafeCell, pin::Pin};
use std::sync::Mutex;

use bedrock::{self as br, InstanceChild, SurfaceCreateInfo};
use shared::{LogicalUnit, PixelsUnit, Point, Rect, Size};
use windows::Win32::{
    Devices::HumanInterfaceDevice::KEYBOARD_OVERRUN_MAKE_CODE,
    Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, POINT, RECT, WPARAM},
    Graphics::{
        Dwm::{DWMWA_EXTENDED_FRAME_BOUNDS, DwmExtendFrameIntoClientArea, DwmGetWindowAttribute},
        Gdi::{
            GetMonitorInfoW, HBRUSH, MONITOR_DEFAULTTONEAREST, MONITORINFO, MapWindowPoints,
            MonitorFromWindow,
        },
    },
    System::{
        Ole::{IDropTarget, RegisterDragDrop, RevokeDragDrop},
        SystemServices::{MK_CONTROL, MK_SHIFT},
    },
    UI::{
        Controls::{MARGINS, WM_MOUSELEAVE},
        HiDpi::GetDpiForWindow,
        Input::{
            GetRawInputData, HRAWINPUT,
            KeyboardAndMouse::{
                MAPVK_VSC_TO_VK_EX, MapVirtualKeyW, ReleaseCapture, SetCapture, TME_HOVER,
                TME_LEAVE, TME_NONCLIENT, TRACKMOUSEEVENT, TRACKMOUSEEVENT_FLAGS, TrackMouseEvent,
            },
            RAWINPUT, RAWINPUTHEADER, RID_INPUT, RIM_TYPEKEYBOARD,
        },
        WindowsAndMessaging::{
            CW_USEDEFAULT, CreateWindowExW, DefWindowProcW, DestroyWindow, GCW_ATOM,
            GetClassLongPtrW, GetClientRect, GetCursorPos, GetSystemMetrics, GetWindowLongPtrW,
            HCURSOR, HTBOTTOM, HTBOTTOMLEFT, HTBOTTOMRIGHT, HTCAPTION, HTCLIENT, HTCLOSE, HTLEFT,
            HTMAXBUTTON, HTMINBUTTON, HTRIGHT, HTTOP, HTTOPLEFT, HTTOPRIGHT, IDI_APPLICATION,
            IsZoomed, LoadIconW, NCCALCSIZE_PARAMS, PostMessageW, PostQuitMessage, RI_KEY_BREAK,
            RI_KEY_E0, RI_KEY_E1, SC_CLOSE, SC_MAXIMIZE, SC_MINIMIZE, SC_RESTORE, SIZE_MAXIMIZED,
            SIZE_RESTORED, SM_CXSIZEFRAME, SM_CYSIZEFRAME, SWP_FRAMECHANGED, SWP_NOACTIVATE,
            SWP_NOMOVE, SWP_NOSIZE, SWP_NOZORDER, SetForegroundWindow, SetWindowLongPtrW,
            SetWindowPos, WA_INACTIVE, WHEEL_DELTA, WINDOW_LONG_PTR_INDEX, WM_ACTIVATE, WM_CHAR,
            WM_CLOSE, WM_CREATE, WM_DESTROY, WM_DPICHANGED, WM_INPUT, WM_KEYDOWN, WM_KEYUP,
            WM_KILLFOCUS, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE, WM_MOUSEWHEEL, WM_MOVE,
            WM_NCCALCSIZE, WM_NCHITTEST, WM_NCLBUTTONDOWN, WM_NCLBUTTONUP, WM_NCMOUSELEAVE,
            WM_NCMOUSEMOVE, WM_NCRBUTTONDOWN, WM_NCRBUTTONUP, WM_RBUTTONDOWN, WM_RBUTTONUP,
            WM_SETFOCUS, WM_SIZE, WM_SYSCOMMAND, WNDCLASS_STYLES, WNDCLASSEXW, WS_EX_APPWINDOW,
            WS_MAXIMIZE, WS_OVERLAPPEDWINDOW, WindowFromPoint,
        },
    },
};
use windows_core::{PCWSTR, w};

use crate::{
    CoreLoop, CoreLoopRunningContextFlags, PointerID, SystemLink, WindowType,
    graphics::VulkanSurface,
    input::{
        KeyboardFocusGroupRef, ModifierKey, PerWindowKeyboardFocusState, ShellPointerActions,
        hittest::{HitTestTreeData, HitTestTreeRef, PointerButton},
    },
    platform::{
        WindowHandleBase,
        windows::{ApplicationContext, DropTarget, ModifierKeyRecorder, translate_keycode},
    },
    rendering::{
        NewWindowData, NewWindowVulkanSurface, RenderMessage,
        composite::{CompositeRect, CompositeTreeRef},
    },
    utils::platform::windows::{
        EnumerateDisplayMonitorContinuous, enumerate_display_monitors, point_from_win32,
        point_to_win32, register_class, window_class_atom,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WindowHandle(pub(super) HWND);
unsafe impl Send for WindowHandle {}
unsafe impl Sync for WindowHandle {}
impl core::hash::Hash for WindowHandle {
    #[inline(always)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.0.hash(state)
    }
}
impl WindowHandleBase for WindowHandle {
    #[inline(always)]
    fn associate_extra_data<T>(&mut self, data: Box<T>) {
        unsafe {
            SetWindowLongPtrW(
                self.0,
                NativeWindow::APP_POINTER_LONG_PTR_OFFSET,
                Box::into_raw(data).addr().cast_signed(),
            );
        }
    }

    #[inline(always)]
    unsafe fn extra_data_ref<'a, T>(&'a self) -> &'a T {
        unsafe {
            &*core::ptr::with_exposed_provenance(
                GetWindowLongPtrW(self.0, NativeWindow::APP_POINTER_LONG_PTR_OFFSET)
                    .cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    unsafe fn extra_data_mut<'a, T>(&'a mut self) -> &'a mut T {
        unsafe {
            &mut *core::ptr::with_exposed_provenance_mut(
                GetWindowLongPtrW(self.0, NativeWindow::APP_POINTER_LONG_PTR_OFFSET)
                    .cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    unsafe fn take_extra_data<T>(&mut self) -> Box<T> {
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
    fn keyboard_focus_state(&self) -> &PerWindowKeyboardFocusState {
        &self.state().keyboard_focus_state
    }

    #[inline(always)]
    fn keyboard_focus_state_mut(&mut self) -> &mut PerWindowKeyboardFocusState {
        &mut self.state_mut().keyboard_focus_state
    }

    #[inline(always)]
    fn root_keyboard_focus_group(&self) -> KeyboardFocusGroupRef {
        self.state().keyboard_focus_state.root_group()
    }

    #[inline(always)]
    fn close(&mut self) {
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
    fn maximize(&self) {
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
    fn minimize(&self) {
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
    fn restore(&self) {
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
    fn ui_scale_factor(&self) -> f32 {
        unsafe { GetDpiForWindow(self.0) as f32 / 96.0 }
    }

    #[inline(always)]
    fn client_size(&self) -> Size<LogicalUnit> {
        self.client_size_pixels().to_logical(self.ui_scale_factor())
    }

    fn client_size_pixels(&self) -> Size<PixelsUnit> {
        let mut rect = core::mem::MaybeUninit::uninit();
        if let Err(e) = unsafe { GetClientRect(self.0, rect.as_mut_ptr()) } {
            tracing::error!(reason = %e, "get_client_rect");
            return Size::new_pixels(0, 0);
        }
        let rect = unsafe { rect.assume_init_ref() };
        Size::new_pixels(rect.right as _, rect.bottom as _)
    }

    fn geometry_state_snapshot(&self, _system_link: &SystemLink) -> crate::WindowGeometryState {
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
            crate::WindowGeometryState::Restored {
                rect: self.screen_rect(),
            }
        }
    }
}
impl WindowHandle {
    #[inline(always)]
    pub(super) fn destroy(&mut self) {
        self.state_mut().destroying = true;
        if let Err(e) = unsafe { DestroyWindow(self.0) } {
            tracing::error!(reason = %e, "window.destroy");
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

    fn query_extended_frame_bounds(&self) -> RECT {
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

        unsafe { extended_frame_bounds.assume_init() }
    }

    #[inline(always)]
    pub fn screen_position(&self) -> Point<PixelsUnit> {
        let extended_frame_bounds = self.query_extended_frame_bounds();
        Point::new_pixels(extended_frame_bounds.left, extended_frame_bounds.top)
    }

    pub fn screen_rect(&self) -> Rect<PixelsUnit> {
        let extended_frame_bounds = self.query_extended_frame_bounds();

        Rect::from_lt_size(
            Point::new_pixels(extended_frame_bounds.left, extended_frame_bounds.top),
            Size::new_pixels(
                (extended_frame_bounds.right - extended_frame_bounds.left) as _,
                (extended_frame_bounds.bottom - extended_frame_bounds.top) as _,
            ),
        )
    }

    #[inline(always)]
    pub fn is_maximized(&self) -> bool {
        unsafe { IsZoomed(self.0).as_bool() }
    }

    #[inline(always)]
    pub fn latest_ui_scale_changes(&self) -> &Mutex<Option<f32>> {
        &self.state().latest_ui_scale_changes
    }

    #[inline(always)]
    pub fn client_pos_to_screen_pos(&self, p: Point<LogicalUnit>) -> Point<PixelsUnit> {
        let mut p = [point_to_win32(&p.to_pixels_round(self.ui_scale_factor()))];
        unsafe {
            MapWindowPoints(Some(self.0), None, &mut p);
        }
        point_from_win32(p[0])
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
impl crate::uicore::MountTarget for WindowHandle {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.state().composite_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.state().ht_root
    }
}

pub(super) struct NativeWindow {
    hwnd: HWND,
}
impl NativeWindow {
    // extra storage assignment
    const EVENT_HANDLER_LONG_PTR_INDEX: WINDOW_LONG_PTR_INDEX = WINDOW_LONG_PTR_INDEX(0);
    const APP_POINTER_LONG_PTR_OFFSET: WINDOW_LONG_PTR_INDEX =
        WINDOW_LONG_PTR_INDEX(core::mem::size_of::<usize>() as _);
    const EXTRA_STORAGE_SIZE: usize = core::mem::size_of::<[usize; 2]>();

    pub fn register_class(hinstance: HINSTANCE) -> u16 {
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

    pub fn new<'sys>(
        window_type: WindowType,
        pos: Option<Point<PixelsUnit>>,
        size: Option<Size<PixelsUnit>>,
        maximized: bool,
        mut coreloop: Pin<&mut CoreLoop<'sys>>,
    ) -> Self {
        let mut window_style = WS_OVERLAPPEDWINDOW;
        if maximized {
            window_style |= WS_MAXIMIZE;
        }
        let w = unsafe {
            CreateWindowExW(
                WS_EX_APPWINDOW,
                PCWSTR(core::ptr::without_provenance(
                    coreloop.syslink.app_context.wc_set.main as _,
                )),
                w!("Peridot Marble Editor"),
                window_style,
                pos.as_ref().map_or(CW_USEDEFAULT, |r| r.x),
                pos.as_ref().map_or(CW_USEDEFAULT, |r| r.y),
                size.as_ref().map_or(CW_USEDEFAULT, |r| r.width as _),
                size.as_ref().map_or(CW_USEDEFAULT, |r| r.height as _),
                None,
                None,
                Some(coreloop.syslink.app_context.hinstance),
                None,
            )
            .expect("CreateWindowExW")
        };
        unsafe {
            let _exclusive_ht_use = coreloop.as_mut().exclusive_ht_use();
            SetWindowLongPtrW(
                w,
                WindowEventHandler::LONG_PTR_INDEX,
                Box::into_raw(Box::new(WindowEventHandler {
                    state: WindowState {
                        r#type: window_type,
                        content_scale: GetDpiForWindow(w) as f32 / 96.0,
                        composite_root: CompositeRect::build()
                            .expand_full()
                            .create(&mut coreloop.as_mut().get_unchecked_mut().composite_tree),
                        ht_root: HitTestTreeData::build()
                            .expand_full()
                            .root_of_window(WindowHandle(w))
                            .create(&mut coreloop.as_mut().get_unchecked_mut().ht_manager),
                        latest_ui_scale_changes: Mutex::new(None),
                        keyboard_focus_state: PerWindowKeyboardFocusState::new(
                            coreloop
                                .as_mut()
                                .get_unchecked_mut()
                                .keyboard_focus_registry
                                .acquire_group(),
                        ),
                        destroying: false,
                    },
                    app_context: coreloop.syslink.app_context,
                    coreloop: coreloop.as_mut().get_unchecked_mut(),
                    modifier_key_state: ModifierKeyRecorder::new(),
                }))
                .addr()
                .cast_signed(),
            );
        }

        // register as drop target
        unsafe {
            RegisterDragDrop(
                w,
                &IDropTarget::from(DropTarget {
                    hwnd: w,
                    active_data: UnsafeCell::new(None),
                }),
            )
            .expect("win32.register_drag_drop");
        }

        // create vulkan surface and register to render thread
        let vk_surface = NewWindowVulkanSurface(
            VulkanSurface::new(coreloop.syslink.gfx, unsafe {
                br::Win32SurfaceCreateInfo::new(
                    core::mem::transmute(coreloop.syslink.app_context.hinstance),
                    core::mem::transmute(w),
                )
                .execute(coreloop.syslink.gfx.instance(), None)
                .expect("vk_surface.create")
            })
            .unbound()
            .1,
        );
        unsafe { coreloop.as_mut().get_unchecked_mut() }
            .delayed_render_messages
            .push(RenderMessage::NewWindow(NewWindowData {
                key: WindowHandle(w),
                vk_surface,
            }));

        Self { hwnd: w }
    }

    #[inline(always)]
    pub const fn make_handle(&self) -> WindowHandle {
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

#[repr(C)] // place state at always 0: this structure can be reinterpreted as a WindowState
pub(super) struct WindowEventHandler<'sys> {
    state: WindowState,
    app_context: *const ApplicationContext,
    coreloop: *mut CoreLoop<'sys>,
    modifier_key_state: ModifierKeyRecorder,
}
impl<'sys> WindowEventHandler<'sys> {
    const LONG_PTR_INDEX: WINDOW_LONG_PTR_INDEX = NativeWindow::EVENT_HANDLER_LONG_PTR_INDEX;

    #[inline(always)]
    pub const fn coreloop(&self) -> Pin<&mut CoreLoop<'sys>> {
        unsafe { Pin::new_unchecked(&mut *self.coreloop) }
    }

    #[inline(always)]
    pub fn get_for_window<'a>(w: HWND) -> &'a mut Self {
        unsafe {
            &mut *core::ptr::with_exposed_provenance_mut::<Self>(
                GetWindowLongPtrW(w, Self::LONG_PTR_INDEX).cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    pub fn try_get_for_window<'a>(w: HWND) -> Option<&'a mut Self> {
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
        *self.state.latest_ui_scale_changes.lock().expect("poisoned") = Some(new_scale);
        self.coreloop()
            .rescale_popup_of_window(WindowHandle(hwnd), new_scale);
        self.coreloop().update_view_all();

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
        self.coreloop().resize_window(
            WindowHandle(hwnd),
            new_size.to_logical(self.state.content_scale),
        );
        self.coreloop().update_view_all();
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
                point_from_win32(p[0]).to_logical(dest_window.ui_scale_factor());

            unsafe {
                SetForegroundWindow(dest_window.0).expect("dest_window.set_foreground");
            }
            self.coreloop()
                .move_redock_preview(dest_window, client_pos_in_dest);
            self.coreloop().update_view_all();

            return;
        }

        self.coreloop().handle_pointer_move(
            WindowHandle(hwnd),
            PointerID(),
            client_pos.to_logical(self.state.content_scale),
            self.modifier_key_state.current,
        );
        self.coreloop().update_view_all();
    }

    #[tracing::instrument(skip(self))]
    fn left_button_down(&mut self, hwnd: HWND, client_pos: Point<PixelsUnit>) {
        // move then down
        self.coreloop().handle_pointer_move(
            WindowHandle(hwnd),
            PointerID(),
            client_pos.to_logical(self.state.content_scale),
            self.modifier_key_state.current,
        );
        self.coreloop().handle_pointer_down(
            PointerID(),
            PointerButton::Primary,
            self.modifier_key_state.current,
        );
        self.coreloop().update_view_all();
    }

    #[tracing::instrument(skip(self))]
    fn left_button_up(&mut self, hwnd: HWND) {
        if self.try_perform_confirm_drag(hwnd) {
            return;
        }

        self.coreloop().handle_pointer_up(
            PointerID(),
            PointerButton::Primary,
            self.modifier_key_state.current,
        );
        self.coreloop().update_view_all();
    }

    #[tracing::instrument(skip(self))]
    fn right_button_down(&mut self, hwnd: HWND, client_pos: Point<PixelsUnit>) {
        // move then down
        self.coreloop().handle_pointer_move(
            WindowHandle(hwnd),
            PointerID(),
            client_pos.to_logical(self.state.content_scale),
            self.modifier_key_state.current,
        );
        self.coreloop().handle_pointer_down(
            PointerID(),
            PointerButton::Secondary,
            self.modifier_key_state.current,
        );
        self.coreloop().update_view_all();
    }

    #[tracing::instrument(skip(self))]
    fn right_button_up(&mut self, hwnd: HWND) {
        if self.try_perform_confirm_drag(hwnd) {
            return;
        }

        self.coreloop().handle_pointer_up(
            PointerID(),
            PointerButton::Secondary,
            self.modifier_key_state.current,
        );
        self.coreloop().update_view_all();
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
            if unsafe { window_class_atom(w) } == unsafe { &*self.app_context }.wc_set.main {
                Some(WindowHandle(w))
            } else {
                None
            }
        };

        let dest_window = pointing_window.unwrap_or(WindowHandle(hwnd));
        unsafe {
            MapWindowPoints(None, Some(dest_window.0), &mut p);
        }
        let client_pos_in_dest = point_from_win32(p[0]).to_logical(dest_window.ui_scale_factor());

        unsafe {
            SetForegroundWindow(dest_window.0).expect("dest_window.set_foreground");
        }
        unsafe { &mut *self.coreloop }.running_context_flags |=
            CoreLoopRunningContextFlags::DISABLE_SYNC_VIEW;
        self.coreloop()
            .confirm_redock(dest_window, client_pos_in_dest);
        unsafe { &mut *self.coreloop }.running_context_flags &=
            !CoreLoopRunningContextFlags::DISABLE_SYNC_VIEW;
        self.coreloop().update_view_all();

        true
    }

    fn non_client_hittest(&self, hwnd: HWND, screen_pos: Point<PixelsUnit>) -> Option<u32> {
        let mut p = [point_to_win32(&screen_pos)];
        unsafe {
            MapWindowPoints(None, Some(hwnd), &mut p);
        }
        let client_pos = point_from_win32(p[0]);

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

        let coreloop = self.coreloop();
        match coreloop.pointer_input_manager.role(
            &client_pos.to_logical(self.state.content_scale),
            &Size::new_pixels(
                (client_size.right - client_size.left) as _,
                (client_size.bottom - client_size.top) as _,
            )
            .to_logical(self.state.content_scale),
            &coreloop.ht_manager,
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

    fn keydown(&mut self, hwnd: HWND, code: usize) {
        self.modifier_key_state.handle_keydown(code);

        self.coreloop().dispatch_key_down(
            WindowHandle(hwnd),
            translate_keycode(code),
            self.modifier_key_state.current,
        );
        self.coreloop().update_view_all();
    }

    fn keyup(&mut self, hwnd: HWND, code: usize) {
        self.modifier_key_state.handle_keyup(code);

        self.coreloop().dispatch_key_up(
            WindowHandle(hwnd),
            translate_keycode(code),
            self.modifier_key_state.current,
        );
        self.coreloop().update_view_all();
    }

    fn char_key(&self, hwnd: HWND, code: usize) {
        if code == 0x08 || code == 0x0d {
            // 一部の記号キーはすでにイベント投げてるのでここでは見ない
            return;
        }

        self.coreloop().dispatch_key_char(
            WindowHandle(hwnd),
            unsafe { char::from_u32_unchecked(code as _) },
            self.modifier_key_state.current,
        );
        self.coreloop().update_view_all();
    }

    fn update_mouse_event_tracking(hwnd: HWND, msg: u32) {
        let mut flags = TRACKMOUSEEVENT_FLAGS(0);
        if msg == WM_MOUSEMOVE {
            flags |= TME_LEAVE | TME_HOVER;
        }
        if msg == WM_NCMOUSEMOVE {
            flags |= TME_LEAVE | TME_HOVER | TME_NONCLIENT;
        }

        if flags.0 != 0 {
            unsafe {
                TrackMouseEvent(&mut TRACKMOUSEEVENT {
                    cbSize: size_of::<TRACKMOUSEEVENT>() as _,
                    dwFlags: flags,
                    hwndTrack: hwnd,
                    dwHoverTime: 0,
                })
                .expect("TrackMouseEvent");
            }
        }
    }

    extern "system" fn handle_messages(
        hwnd: HWND,
        msg: u32,
        wparam: WPARAM,
        lparam: LPARAM,
    ) -> LRESULT {
        Self::update_mouse_event_tracking(hwnd, msg);

        if msg == WM_CLOSE {
            let Some(e) = Self::try_get_for_window(hwnd) else {
                return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
            };

            e.state.destroying = true;
            match e.state.r#type {
                WindowType::Main {} => unsafe {
                    PostQuitMessage(0);
                },
                WindowType::Sub => e.coreloop().close_sub_window(WindowHandle(hwnd)),
            }

            return LRESULT(0);
        }

        if msg == WM_DESTROY {
            // unregister from drop target
            unsafe {
                RevokeDragDrop(hwnd).expect("win32.revoke_drag_drop");
            }

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

            if let Some(st) = Self::try_get_for_window(hwnd) {
                if wparam.0 == WA_INACTIVE as _ {
                    // deactivated window
                    st.coreloop().close_all_flyouts(WindowHandle(hwnd));
                    st.coreloop().update_view_all();
                }
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

            st.coreloop().notify_focus_changes(WindowHandle(hwnd), true);
            st.coreloop().update_view_all();
            return LRESULT(0);
        }

        if msg == WM_KILLFOCUS {
            let st = Self::get_for_window(hwnd);
            if st.state.destroying {
                // in destroy sequence
                return LRESULT(0);
            }

            st.coreloop()
                .notify_focus_changes(WindowHandle(hwnd), false);
            st.coreloop().close_all_flyouts(WindowHandle(hwnd)); // focus lost時はflyout系もcloseする
            st.coreloop().update_view_all();
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

                tracing::debug!(data = ?unsafe { data.data.keyboard }, is_down, vk, "raw input keyboard");
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

            st.coreloop().close_all_flyouts(WindowHandle(hwnd));
            st.coreloop().update_view_all();

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
                    .coreloop()
                    .handle_window_maximize_state_changes(WindowHandle(hwnd), true);
                state.coreloop().update_view_all();
            }
            if wparam.0 == SIZE_RESTORED as _ {
                state
                    .coreloop()
                    .handle_window_maximize_state_changes(WindowHandle(hwnd), false);
                state.coreloop().update_view_all();
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

            st.coreloop().handle_pointer_leave_window(PointerID());
            st.coreloop().update_view_all();
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

            Self::get_for_window(hwnd).coreloop().dispatch_scroll_wheel(
                (wparam.0 >> 16) as i16 as f32 / WHEEL_DELTA as f32,
                key_modifier,
            );
            Self::get_for_window(hwnd).coreloop().update_view_all();
            return LRESULT(0);
        }

        unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
    }
}
