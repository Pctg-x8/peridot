use bedrock::{self as br, InstanceChild, SurfaceCreateInfo};
use windows::{
    Foundation::TypedEventHandler,
    UI::{
        Composition::CompositionEffectSourceParameter,
        Text::Core::{
            CoreTextCompositionCompletedEventArgs, CoreTextCompositionStartedEventArgs,
            CoreTextEditContext, CoreTextFormatUpdatingEventArgs, CoreTextLayoutRequestedEventArgs,
            CoreTextSelectionRequestedEventArgs, CoreTextSelectionUpdatingEventArgs,
            CoreTextServicesManager, CoreTextTextRequestedEventArgs, CoreTextTextUpdatingEventArgs,
        },
    },
    Win32::{
        Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, POINT, RECT, WPARAM},
        Graphics::Gdi::{HBRUSH, MapWindowPoints},
        System::WinRT::Composition::ICompositorDesktopInterop,
        UI::{
            HiDpi::GetDpiForWindow,
            WindowsAndMessaging::{
                CW_USEDEFAULT, CreateWindowExW, DefWindowProcW, GetClientRect, GetSystemMetrics,
                GetWindowLongPtrW, HCURSOR, HICON, HTCAPTION, HTCLIENT, HTCLOSE, HTMAXBUTTON,
                HTMINBUTTON, IDI_APPLICATION, LoadIconW, NCCALCSIZE_PARAMS, PostQuitMessage,
                SHOW_WINDOW_CMD, SM_CXSIZEFRAME, SM_CYSIZEFRAME, SW_HIDE, SW_SHOWNOACTIVATE,
                SWP_FRAMECHANGED, SWP_NOACTIVATE, SWP_NOMOVE, SWP_NOSIZE, SWP_NOZORDER,
                SetWindowLongPtrW, SetWindowPos, ShowWindow, WA_ACTIVE, WA_CLICKACTIVE,
                WINDOW_LONG_PTR_INDEX, WM_ACTIVATE, WM_CHAR, WM_CLOSE, WM_CREATE, WM_DPICHANGED,
                WM_KILLFOCUS, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE, WM_NCCALCSIZE,
                WM_NCHITTEST, WM_SETFOCUS, WM_SIZE, WNDCLASS_STYLES, WNDCLASSEXW, WS_EX_APPWINDOW,
                WS_EX_LAYERED, WS_EX_NOACTIVATE, WS_EX_NOREDIRECTIONBITMAP, WS_EX_TOPMOST,
                WS_EX_TRANSPARENT, WS_OVERLAPPEDWINDOW, WS_POPUP,
            },
        },
    },
};
use windows_core::{IInspectable, Interface, PCWSTR, h, w};
use windows_numerics::{Vector2, Vector3};

use core::pin::Pin;
use std::sync::Mutex;

use crate::{
    Event, LogicFiberEventDispatcher,
    bindgen::Microsoft::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    composite::CompositeTreeRef,
    graphics::{VulkanDevice, VulkanSurface},
    hittest::{HitTestTreeManager, HitTestTreeRef},
    input::{PointerInputManager, PointerInputUnit, ShellPointerActions},
    utils::{LogicalUnit, PixelsUnit, Point, Size, platform::windows::register_class},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct WindowHandle(HWND);
impl core::hash::Hash for WindowHandle {
    #[inline(always)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.0.hash(state)
    }
}
impl WindowHandle {
    pub const unsafe fn from_native(hwnd: HWND) -> Self {
        Self(hwnd)
    }

    #[inline(always)]
    fn state<'a, 'h>(&'a self) -> &'a WindowState<'h> {
        unsafe {
            &*core::ptr::with_exposed_provenance(
                GetWindowLongPtrW(self.0, WindowEventHandler::<()>::LONG_PTR_INDEX).cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    pub fn bind_hittest_managers(
        &self,
        pointer_input_manager: &PointerInputManager,
        ht_manager: &HitTestTreeManager,
    ) {
        let st = unsafe {
            &mut *core::ptr::with_exposed_provenance_mut::<WindowState>(
                GetWindowLongPtrW(self.0, WindowEventHandler::<()>::LONG_PTR_INDEX).cast_unsigned(),
            )
        };
        st.pointer_input_manager_ptr = pointer_input_manager;
        st.ht_manager_ptr = ht_manager;
    }

    #[inline(always)]
    pub fn unbind_hittest_managers(&self) {
        let st = unsafe {
            &mut *core::ptr::with_exposed_provenance_mut::<WindowState>(
                GetWindowLongPtrW(self.0, WindowEventHandler::<()>::LONG_PTR_INDEX).cast_unsigned(),
            )
        };
        st.pointer_input_manager_ptr = core::ptr::null();
        st.ht_manager_ptr = core::ptr::null();
    }

    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        let mut rc = core::mem::MaybeUninit::uninit();
        if let Err(e) = unsafe {
            windows::Win32::UI::WindowsAndMessaging::GetClientRect(self.0, rc.as_mut_ptr())
        } {
            tracing::error!(reason = %e, "get_client_rect");
            return Size::new_logical(0.0, 0.0);
        }

        let rc = unsafe { rc.assume_init_ref() };
        Size::new_pixels(rc.right as _, rc.bottom as _)
            .to_logical(unsafe { windows::Win32::UI::HiDpi::GetDpiForWindow(self.0) as f32 / 96.0 })
    }

    #[inline(always)]
    pub fn ui_scale_factor(&self) -> f32 {
        unsafe { windows::Win32::UI::HiDpi::GetDpiForWindow(self.0) as f32 / 96.0 }
    }

    #[inline(always)]
    pub fn composite_root(&self) -> CompositeTreeRef {
        self.state().composite_root
    }

    #[inline(always)]
    pub fn ht_root(&self) -> HitTestTreeRef {
        self.state().ht_root
    }
}
impl ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {
        unsafe {
            windows::Win32::UI::Input::KeyboardAndMouse::SetCapture(self.0);
        }
    }

    #[inline(always)]
    fn release_pointer(&self) {
        if let Err(e) = unsafe { windows::Win32::UI::Input::KeyboardAndMouse::ReleaseCapture() } {
            tracing::error!(reason = %e, "release_capture");
        }
    }
}

#[derive(Clone, Copy)]
pub struct PointerID();

impl crate::SystemLink {
    #[inline(always)]
    pub fn notify_ui_scale_changes_to_render(&self, window: WindowHandle, new_scale: f32) {
        *window
            .state()
            .latest_ui_scale_changes
            .lock()
            .expect("poisoned") = Some(new_scale);
    }
}

pub struct WindowClassSet {
    hinstance: HINSTANCE,
    main: u16,
}
impl WindowClassSet {
    pub fn register<AppFuture: core::future::Future<Output = ()>>(hinstance: HINSTANCE) -> Self {
        let main = unsafe {
            register_class(&WNDCLASSEXW {
                cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
                style: WNDCLASS_STYLES(0),
                cbClsExtra: 0,
                cbWndExtra: core::mem::size_of::<[usize; 3]>() as _,
                lpfnWndProc: Some(WindowEventHandler::<AppFuture>::handle_messages),
                hInstance: hinstance,
                hIcon: LoadIconW(None, IDI_APPLICATION).expect("LoadIconW"),
                hCursor: HCURSOR(core::ptr::null_mut()),
                hbrBackground: HBRUSH(core::ptr::null_mut()),
                lpszMenuName: PCWSTR::null(),
                lpszClassName: w!("MainWindow"),
                hIconSm: LoadIconW(None, IDI_APPLICATION).expect("LoadIconW"),
            })
            .expect("register_class.main")
        };

        Self { hinstance, main }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct SendableWindowHandle(HWND);
unsafe impl Sync for SendableWindowHandle {}
unsafe impl Send for SendableWindowHandle {}
impl SendableWindowHandle {
    #[inline(always)]
    pub fn pixels_client_size(&self) -> Size<PixelsUnit> {
        let mut rect = core::mem::MaybeUninit::uninit();
        unsafe {
            GetClientRect(self.0, rect.as_mut_ptr()).expect("GetClientRect");
        }
        let rect = unsafe { rect.assume_init_ref() };
        Size::new_pixels(rect.right as _, rect.bottom as _)
    }
}

pub struct NativeWindow<'h, AppFuture> {
    hinstance: HINSTANCE,
    hwnd: HWND,
    event_handler: Pin<Box<WindowEventHandler<'h, AppFuture>>>,
}
unsafe impl<AppFuture> Sync for NativeWindow<'_, AppFuture> {}
unsafe impl<AppFuture> Send for NativeWindow<'_, AppFuture> {}
impl<'h, AppFuture> NativeWindow<'h, AppFuture> {
    pub fn new(
        wc_set: &WindowClassSet,
        composite_root: CompositeTreeRef,
        ht_root: HitTestTreeRef,
        event_dispatcher: LogicFiberEventDispatcher<AppFuture>,
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
        let mut event_handler = Box::pin(WindowEventHandler {
            state: WindowState {
                pointer_input_manager_ptr: core::ptr::null(),
                ht_manager_ptr: core::ptr::null(),
                content_scale: unsafe {
                    windows::Win32::UI::HiDpi::GetDpiForWindow(w) as f32 / 96.0
                },
                composite_root,
                ht_root,
                latest_ui_scale_changes: Mutex::new(None),
            },
            event_dispatcher,
            text_services_mgr: None,
            edit_context: None,
        });
        unsafe {
            SetWindowLongPtrW(
                w,
                WindowEventHandler::<()>::LONG_PTR_INDEX,
                event_handler.as_mut().get_mut() as *mut _ as _,
            );
        }

        Self {
            hinstance: wc_set.hinstance,
            hwnd: w,
            event_handler,
        }
    }

    #[inline(always)]
    pub fn create_vk_surface<'d>(&self, device: &'d VulkanDevice) -> VulkanSurface<'d> {
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
    pub fn rebind_event_dispatcher(&mut self, dispatcher: LogicFiberEventDispatcher<AppFuture>) {
        self.event_handler.event_dispatcher = dispatcher;
    }

    #[inline(always)]
    pub const fn make_handle(&self) -> WindowHandle {
        WindowHandle(self.hwnd)
    }

    #[inline(always)]
    pub const fn make_sendable(&self) -> SendableWindowHandle {
        SendableWindowHandle(self.hwnd)
    }

    #[inline(always)]
    pub fn state_ref<'a>(&'a self) -> &'a WindowState<'h> {
        &self.event_handler.state
    }

    #[inline(always)]
    pub fn dpi(&self) -> u32 {
        unsafe { GetDpiForWindow(self.hwnd) }
    }

    #[inline(always)]
    pub fn show(&self, cmd: SHOW_WINDOW_CMD) {
        let _ = unsafe { ShowWindow(self.hwnd, cmd) };
    }
}

pub struct WindowState<'h> {
    pointer_input_manager_ptr: *const PointerInputManager,
    ht_manager_ptr: *const HitTestTreeManager<'h>,
    content_scale: f32,
    pub composite_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    pub latest_ui_scale_changes: Mutex<Option<f32>>,
}
unsafe impl Sync for WindowState<'_> {}
unsafe impl Send for WindowState<'_> {}

#[repr(C)] // place state at always 0: this structure can be reinterpreted as a WindowState
pub struct WindowEventHandler<'h, AppFuture> {
    state: WindowState<'h>,
    event_dispatcher: LogicFiberEventDispatcher<AppFuture>,
    text_services_mgr: Option<CoreTextServicesManager>,
    edit_context: Option<CoreTextEditContext>,
}
impl<AppFuture> WindowEventHandler<'_, AppFuture> {
    const LONG_PTR_INDEX: WINDOW_LONG_PTR_INDEX = WINDOW_LONG_PTR_INDEX(0);
}
impl<AppFuture: core::future::Future<Output = ()>> WindowEventHandler<'_, AppFuture> {
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

    fn compute_client_rect(params: &mut NCCALCSIZE_PARAMS) {
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

        unsafe {
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

        self.state.content_scale = new_scale;
        self.event_dispatcher.dispatch(Event::WindowRescaleUI {
            window: WindowHandle(hwnd),
            new_scale,
        });
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
        });
    }

    #[tracing::instrument(skip(self))]
    fn left_button_up(&mut self, hwnd: HWND) {
        self.event_dispatcher.dispatch(Event::PointerUp {
            window: WindowHandle(hwnd),
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
            return Some(windows::Win32::UI::WindowsAndMessaging::HTTOP);
        }

        if self.state.pointer_input_manager_ptr.is_null() {
            // unlinked from logic fiber
            return Some(HTCLIENT);
        }

        let pointer_input_manager = unsafe { &*self.state.pointer_input_manager_ptr };
        match pointer_input_manager.role(
            &client_pos.to_logical(self.state.content_scale),
            &Size::new_pixels(
                (client_size.right - client_size.left) as _,
                (client_size.bottom - client_size.top) as _,
            )
            .to_logical(self.state.content_scale),
            unsafe { &*self.state.ht_manager_ptr },
            self.state.ht_root,
        ) {
            None => Some(HTCLIENT),
            Some(crate::hittest::Role::TitleBar) => Some(HTCAPTION),
            Some(crate::hittest::Role::ForceClient) => Some(HTCLIENT),
            Some(crate::hittest::Role::CloseButton) => Some(HTCLOSE),
            Some(crate::hittest::Role::MaximizeButton) => Some(HTMAXBUTTON),
            Some(crate::hittest::Role::MinimizeButton) => Some(HTMINBUTTON),
            // Windowsだと同じ位置にあるので同じものを返す
            Some(crate::hittest::Role::RestoreButton) => Some(HTMAXBUTTON),
        }
    }

    extern "system" fn handle_messages(
        hwnd: HWND,
        msg: u32,
        wparam: WPARAM,
        lparam: LPARAM,
    ) -> LRESULT {
        if msg == WM_CLOSE {
            unsafe {
                // TODO: detect main window
                PostQuitMessage(0);
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

        if msg == WM_ACTIVATE && (wparam.0 == WA_ACTIVE as _ || wparam.0 == WA_CLICKACTIVE as _) {
            let state = Self::get_for_window(hwnd);

            if state.text_services_mgr.is_none() {
                // first time activation
                let text_services_mgr = CoreTextServicesManager::GetForCurrentView()
                    .expect("coretextservicesmanager.get");
                let edit_context = text_services_mgr
                    .CreateEditContext()
                    .expect("edit_context.create");
                edit_context
                    .LayoutRequested(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextLayoutRequestedEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        let req = e.Request().expect("layout_requested.event_args.request");
                        tracing::trace!(
                            req.is_canceled = ?req.IsCanceled(),
                            req.range = ?req.Range(),
                            "edit_context.layout_requested"
                        );

                        req.LayoutBounds()
                        .expect("layout_requested.event_args.request.layout_bounds")
                        .SetControlBounds(windows::Foundation::Rect {
                            X: 0.0,
                            Y: 0.0,
                            Width: 100.0,
                            Height: 20.0,
                        })
                        .expect(
                            "layout_requested.event_args.request.layout_bounds.set_control_bounds",
                        );
                        req.LayoutBounds()
                            .expect("layout_requested.event_args.request.layout_bounds")
                            .SetTextBounds(windows::Foundation::Rect {
                                X: 0.0,
                                Y: 0.0,
                                Width: 100.0,
                                Height: 20.0,
                            })
                            .expect(
                                "layout_requested.event_args.request.layout_bounds.set_text_bounds",
                            );

                        Ok(())
                    }))
                    .expect("edit_context.layout_requested");
                edit_context
                    .TextRequested(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextTextRequestedEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        let req = e.Request().expect("text_requested.event_args.request");
                        tracing::trace!(
                            req.is_canceled = ?req.IsCanceled(),
                            req.range = ?req.Range(),
                            req.text = ?req.Text(),
                            "edit_context.text_requested"
                        );

                        Ok(())
                    }))
                    .expect("edit_context.text_requested");
                edit_context
                    .TextUpdating(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextTextUpdatingEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        tracing::trace!(
                            input_language = ?e.InputLanguage(),
                            is_canceled = ?e.IsCanceled(),
                            new_selection = ?e.NewSelection(),
                            range = ?e.Range(),
                            text = ?e.Text().map(|x| x.to_string_lossy()),
                            "edit_context.text_updating"
                        );

                        Ok(())
                    }))
                    .expect("edit_context.text_updating");
                edit_context
                    .CompositionStarted(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextCompositionStartedEventArgs,
                    >::new(|sender, e| {
                        tracing::trace!("composition_started");
                        let e = e.ok().expect("event_args.null");
                        tracing::trace!(
                            is_canceled = ?e.IsCanceled(),
                            "edit_context.composition_started"
                        );
                        Ok(())
                    }))
                    .expect("edit_context.composition_started");
                edit_context.CompositionCompleted(&TypedEventHandler::<
                    CoreTextEditContext,
                    CoreTextCompositionCompletedEventArgs,
                >::new(move |sender, e| {
                    let e = e.ok().expect("event_args.null");
                    tracing::trace!(
                        composition_segments = ?e.CompositionSegments(),
                        composition_segments.len = ?e.CompositionSegments().and_then(|x| x.Size()),
                        is_canceled = ?e.IsCanceled(),
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
                edit_context
                    .FormatUpdating(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextFormatUpdatingEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        tracing::trace!(
                            background_color = ?e.BackgroundColor(),
                            is_canceled = ?e.IsCanceled(),
                            range = ?e.Range(),
                            reason = ?e.Reason(),
                            text_color = ?e.TextColor(),
                            underline_color = ?e.UnderlineColor(),
                            underline_type = ?e.UnderlineType(),
                            "edit_context.format_updating"
                        );

                        Ok(())
                    }))
                    .expect("edit_context.format_updating");
                edit_context
                    .FocusRemoved(&TypedEventHandler::<
                        CoreTextEditContext,
                        windows_core::IInspectable,
                    >::new(|sender, e| {
                        tracing::trace!(e = ?e.ok(), "edit_context.focus_removed");

                        Ok(())
                    }))
                    .expect("edit_context.focus_removed");
                edit_context
                    .NotifyFocusLeaveCompleted(&TypedEventHandler::<
                        CoreTextEditContext,
                        IInspectable,
                    >::new(|sender, e| {
                        tracing::trace!(e = ?e.ok(), "edit_context.notify_focus_leave_completed");

                        Ok(())
                    }))
                    .expect("edit_context.notify_focus_leave_completed");
                edit_context
                    .SelectionRequested(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextSelectionRequestedEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        let req = e
                            .Request()
                            .expect("edit_context.selection_requested.event_args.request");
                        tracing::trace!(
                            req.is_canceled = ?req.IsCanceled(),
                            req.selection = ?req.Selection(),
                            "edit_context.selection_requested"
                        );

                        Ok(())
                    }))
                    .expect("edit_context.selection_requested");
                edit_context
                    .SelectionUpdating(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextSelectionUpdatingEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        tracing::trace!(
                            is_canceled = ?e.IsCanceled(),
                            selection = ?e.Selection(),
                            "edit_context.selection_updating"
                        );

                        Ok(())
                    }))
                    .expect("edit_context.selection_updating");

                state.text_services_mgr = Some(text_services_mgr);
                state.edit_context = Some(edit_context);
            }
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
            let state = Self::get_for_window(hwnd);

            state
                .edit_context
                .as_ref()
                .expect("not activated?")
                .NotifyFocusEnter()
                .expect("edit_context.notify_focus_enter");

            return LRESULT(0);
        }

        if msg == WM_KILLFOCUS {
            let state = Self::get_for_window(hwnd);

            state
                .edit_context
                .as_ref()
                .expect("not activated?")
                .NotifyFocusLeave()
                .expect("edit_context.notify_focus_leave");

            return LRESULT(0);
        }

        if msg == WM_CHAR {
            tracing::trace!(keycode = wparam.0, "char input");

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
            }

            return LRESULT(0);
        }

        if msg == WM_NCCALCSIZE {
            if wparam.0 == 1 {
                Self::compute_client_rect(unsafe {
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

        if msg == WM_LBUTTONUP {
            Self::get_for_window(hwnd).left_button_up(hwnd);

            return LRESULT(0);
        }

        unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
    }
}

pub struct DragPreviewPopoverHandle {
    w: HWND,
    base_window_handle: core::cell::Cell<HWND>,
    _composition_target: windows::UI::Composition::Desktop::DesktopWindowTarget,
    root_visual: windows::UI::Composition::SpriteVisual,
}
impl Drop for DragPreviewPopoverHandle {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = unsafe { windows::Win32::UI::WindowsAndMessaging::DestroyWindow(self.w) } {
            tracing::error!(reason = %e, "dragPreviewPopover.destroyNative");
        }
    }
}
impl DragPreviewPopoverHandle {
    pub fn bind_position_base_window(&self, window: WindowHandle) {
        self.base_window_handle.set(window.0);
    }

    pub fn new(
        hinstance: HINSTANCE,
        native_compositor: &windows::UI::Composition::Compositor,
    ) -> Self {
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
