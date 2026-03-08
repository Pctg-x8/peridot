use bedrock::{self as br, InstanceChild, SurfaceCreateInfo};
use windows::{
    Foundation::TypedEventHandler,
    UI::{
        Composition::{
            CompositionEffectSourceParameter, Compositor, Desktop::DesktopWindowTarget,
            SpriteVisual,
        },
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
            Input::KeyboardAndMouse::{ReleaseCapture, SetCapture},
            WindowsAndMessaging::{
                CW_USEDEFAULT, CreateWindowExW, DefWindowProcW, DestroyWindow, GetClientRect,
                GetSystemMetrics, GetWindowLongPtrW, HCURSOR, HICON, HTCAPTION, HTCLIENT, HTCLOSE,
                HTMAXBUTTON, HTMINBUTTON, HTTOP, IDC_ARROW, IDC_HAND, IDC_IBEAM, IDC_SIZEWE,
                IDI_APPLICATION, LoadCursorW, LoadIconW, NCCALCSIZE_PARAMS, PostQuitMessage,
                SM_CXSIZEFRAME, SM_CYSIZEFRAME, SW_HIDE, SW_SHOW, SW_SHOWNOACTIVATE, SW_SHOWNORMAL,
                SWP_FRAMECHANGED, SWP_NOACTIVATE, SWP_NOMOVE, SWP_NOSIZE, SWP_NOZORDER, SetCursor,
                SetWindowLongPtrW, SetWindowPos, ShowWindow, WA_ACTIVE, WA_CLICKACTIVE,
                WINDOW_LONG_PTR_INDEX, WM_ACTIVATE, WM_CHAR, WM_CLOSE, WM_CREATE, WM_DESTROY,
                WM_DPICHANGED, WM_KILLFOCUS, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE,
                WM_NCCALCSIZE, WM_NCHITTEST, WM_SETFOCUS, WM_SIZE, WNDCLASS_STYLES, WNDCLASSEXW,
                WS_EX_APPWINDOW, WS_EX_LAYERED, WS_EX_NOACTIVATE, WS_EX_NOREDIRECTIONBITMAP,
                WS_EX_TOPMOST, WS_EX_TRANSPARENT, WS_OVERLAPPEDWINDOW, WS_POPUP,
            },
        },
    },
};
use windows_core::{IInspectable, Interface, PCWSTR, h, w};
use windows_numerics::{Vector2, Vector3};

use std::sync::Mutex;

use crate::{
    Event, LogicFiberEventDispatcher, WindowType,
    bindgen::Microsoft::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    graphics::{VulkanDevice, VulkanSurface},
    input::{
        PointerInputManager, PointerInputUnit, ShellPointerActions,
        hittest::{
            CursorShape, HitTestTreeCreate, HitTestTreeData, HitTestTreeManager, HitTestTreeRef,
        },
    },
    rendering::{
        NewWindowData, NewWindowVulkanSurface, RenderMessage,
        composite::{CompositeRect, CompositeTree, CompositeTreeRef},
    },
    utils::{LogicalUnit, PixelsUnit, Point, Size, platform::windows::register_class},
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
    pub unsafe fn extra_data_ref_mut<'a, T>(&'a self) -> &'a mut T {
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
    pub fn register(hinstance: HINSTANCE) -> Self {
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
            },
            event_dispatcher,
            text_services_mgr: None,
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
    text_services_mgr: Option<CoreTextServicesManager>,
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

    pub fn new(hinstance: HINSTANCE, native_compositor: &Compositor) -> Self {
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

pub struct SystemLink<'sys> {
    pub drag_preview_popover: DragPreviewPopoverHandle,
    pub vk_device: *const VulkanDevice<'sys>,
    pub rt_sender: std::sync::mpsc::Sender<RenderMessage>,
    pub event_dispatcher: *mut LogicFiberEventDispatcher,
    pub window_class_set: *const WindowClassSet,
}
impl SystemLink<'_> {
    pub fn init_main_window(
        vk_device: &VulkanDevice,
        dispatcher: LogicFiberEventDispatcher,
        composite_tree: &mut CompositeTree<Event>,
        ht_manager: &mut HitTestTreeManager,
        rt_sender: &std::sync::mpsc::Sender<RenderMessage>,
        wc_set: &WindowClassSet,
    ) -> WindowHandle {
        let w = NativeWindow::new(
            &wc_set,
            WindowType::Main {},
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            ht_manager.create(HitTestTreeData {
                width_adjustment_factor: 1.0,
                height_adjustment_factor: 1.0,
                ..Default::default()
            }),
            dispatcher,
        );
        let main_window_handle = w.make_handle();

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
        unsafe {
            let _ = ShowWindow(handle.0, SW_SHOWNORMAL);
        }
    }

    #[inline(always)]
    pub fn drag_preview_popover(&self) -> &DragPreviewPopoverHandle {
        &self.drag_preview_popover
    }

    pub fn open_window<'h>(
        &self,
        composite_tree: &mut CompositeTree<Event>,
        hit_tree: &mut (impl HitTestTreeCreate<'h> + ?Sized),
    ) -> WindowHandle {
        let w = NativeWindow::new(
            unsafe { &*self.window_class_set },
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

        unsafe {
            let _ = ShowWindow(w.hwnd, SW_SHOW);
            (*self.event_dispatcher).dispatch(Event::SubWindowOpen { window: h });
        }
        h
    }

    pub fn close_window(
        &self,
        mut window_handle: WindowHandle,
        composite_tree: &mut CompositeTree<Event>,
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
