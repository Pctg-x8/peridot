use core::pin::Pin;
use std::sync::{Arc, Mutex, atomic::AtomicBool};

use bedrock::{InstanceChild, SurfaceCreateInfo};
use shared::{LogicalUnit, PixelsUnit, Point, Rect, Size};

use crate::{
    CoreLoop, FlyoutSurfaceHandle, MainWindowOpenMode, SubWindowOpenMode, SystemLink,
    WindowGeometryState, WindowType,
    graphics::VulkanSurface,
    input::{
        KeyInputCode, KeyboardFocusGroupRef, ModifierKey, PerWindowKeyboardFocusState,
        hittest::{CursorShape, HitTestTreeData, HitTestTreeRef, PointerButton},
    },
    platform::{WindowHandleBase, mac::bridge::TextInputClientForwarding},
    rendering::{
        NewWindowData, NewWindowVulkanSurface, RenderMessage,
        composite::{CompositeRect, CompositeTreeRef},
    },
    ui::dock::DockingPreviewState,
};

pub mod bridge;
pub mod flyout_surface;

pub type WindowPersistentStateNativeGeometryUnit = LogicalUnit;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WindowHandle(*mut self::bridge::WindowLink);
unsafe impl Sync for WindowHandle {}
unsafe impl Send for WindowHandle {}
impl WindowHandleBase for WindowHandle {
    #[inline(always)]
    fn associate_extra_data<T>(&mut self, data: Box<T>) {
        self.state_mut().extra_data = Box::into_raw(data) as _;
    }

    #[inline(always)]
    unsafe fn extra_data_ref<T>(&self) -> &T {
        unsafe { &*self.state().extra_data.cast() }
    }

    #[inline(always)]
    unsafe fn extra_data_mut<T>(&mut self) -> &mut T {
        unsafe { &mut *self.state_mut().extra_data.cast() }
    }

    #[inline(always)]
    unsafe fn take_extra_data<T>(&mut self) -> Box<T> {
        unsafe {
            Box::from_raw(
                core::mem::replace(&mut self.state_mut().extra_data, core::ptr::null_mut()).cast(),
            )
        }
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
        self.state().kf_root_group
    }

    // TODO: impl them
    fn close(&mut self) {
        unimplemented!("WindowHandle::on_click_sys_close_button")
    }

    fn maximize(&self) {
        unimplemented!("WindowHandle::on_click_sys_maximize_button")
    }

    fn minimize(&self) {
        unimplemented!("WindowHandle::on_click_sys_minimize_button")
    }

    fn restore(&self) {
        unimplemented!("WindowHandle::on_click_sys_restore_button")
    }

    #[inline(always)]
    fn ui_scale_factor(&self) -> f32 {
        *self.state().active_buffer_scale.lock().expect("poisoned")
    }

    #[inline(always)]
    fn client_size(&self) -> Size<LogicalUnit> {
        *self.state().active_size.lock().expect("poisoned")
    }

    #[inline(always)]
    fn client_size_pixels(&self) -> Size<PixelsUnit> {
        *self.state().active_rt_size.lock().expect("poisoned")
    }

    fn geometry_state_snapshot(&self, _syslink: &SystemLink) -> WindowGeometryState {
        let mut is_maximized = core::mem::MaybeUninit::uninit();
        let mut x = core::mem::MaybeUninit::uninit();
        let mut y = core::mem::MaybeUninit::uninit();
        let mut width = core::mem::MaybeUninit::uninit();
        let mut height = core::mem::MaybeUninit::uninit();
        unsafe {
            bridge::ni_query_window_state(
                self.0,
                is_maximized.as_mut_ptr(),
                x.as_mut_ptr(),
                y.as_mut_ptr(),
                width.as_mut_ptr(),
                height.as_mut_ptr(),
            );
        }
        let is_maximized = unsafe { is_maximized.assume_init() };
        let x = unsafe { x.assume_init() };
        let y = unsafe { y.assume_init() };
        let width = unsafe { width.assume_init() };
        let height = unsafe { height.assume_init() };

        if is_maximized {
            // TODO: multi monitor support?
            WindowGeometryState::Maximized { monitor_index: 0 }
        } else {
            WindowGeometryState::Restored {
                rect: Rect::from_lt_size(
                    Point::new_logical(x as _, y as _),
                    Size::new_logical(width as _, height as _),
                ),
            }
        }
    }
}
impl WindowHandle {
    #[inline(always)]
    pub fn state(&self) -> &WindowState {
        &WindowDispatcher::get(self.0).state
    }

    #[inline(always)]
    fn state_mut(&mut self) -> &mut WindowState {
        &mut WindowDispatcher::get_mut(self.0).state
    }

    #[inline(always)]
    pub fn latest_ui_scale_changes(&self) -> &Mutex<Option<f32>> {
        &self.state().latest_ui_scale_changes
    }

    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        self.state()
            .swapchain_externally_invalidation_signal
            .compare_exchange_weak(
                true,
                false,
                std::sync::atomic::Ordering::Relaxed,
                std::sync::atomic::Ordering::Relaxed,
            )
            == Ok(true)
    }

    #[inline(always)]
    pub const fn needs_corner_cutout_rendering(&self) -> bool {
        // macはシステムが角のくり抜きしてくれる
        false
    }

    #[inline(always)]
    pub const fn needs_system_command_buttons(&self) -> bool {
        // macはシステムがコマンドボタンを出してくれる
        false
    }

    pub fn is_maximized(&self) -> bool {
        false
    }

    pub fn begin_text_input<T: TextInputClientForwarding>(&self, forwarding: *mut T) {
        unsafe {
            self::bridge::ni_accepts_key_inputs_to_view(
                self.0,
                Box::into_raw(Box::new(
                    self::bridge::TextInputClientForwardingFT::r#for::<T>(),
                )),
                forwarding.cast(),
            );
        }
    }

    pub fn end_text_input(&self) {
        let mut ftable = core::mem::MaybeUninit::uninit();
        let mut context = core::mem::MaybeUninit::uninit();

        unsafe {
            self::bridge::ni_accepts_key_inputs_to_window(
                self.0,
                ftable.as_mut_ptr(),
                context.as_mut_ptr(),
            );
        }

        drop(unsafe { Box::from_raw(ftable.assume_init().cast_mut()) })
    }

    #[inline(always)]
    pub fn lock_cursor(&self) {
        unsafe {
            self::bridge::ni_lock_cursor();
        }
    }

    #[inline(always)]
    pub fn unlock_cursor(&self) {
        unsafe {
            self::bridge::ni_unlock_cursor();
        }
    }
}
impl crate::input::ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {}

    #[inline(always)]
    fn release_pointer(&self) {}
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PointerID();

pub struct DragData {}
impl DragData {
    pub fn is_file_drop(&self) -> bool {
        todo!("DragData::is_file_drop")
    }

    pub fn query_file_list(&self) -> Result<Vec<String>, ()> {
        todo!("DragData::query_file_list")
    }
}

pub fn create_main_window<'sys>(
    mode: MainWindowOpenMode,
    mut coreloop: Pin<&mut CoreLoop<'sys>>,
) -> WindowHandle {
    let (size, pos, flags);
    match mode {
        MainWindowOpenMode::New => {
            size = Size::new_logical(960.0, 540.0);
            pos = Point::new_logical(0.0, 0.0);
            flags = bridge::WindowCreationFlags::MAIN;
        }
        MainWindowOpenMode::Restore(WindowGeometryState::Maximized { monitor_index: _ }) => {
            // TODO: multi monitor support?
            size = Size::new_logical(960.0, 540.0);
            pos = Point::new_logical(0.0, 0.0);
            flags = bridge::WindowCreationFlags::MAIN
                | bridge::WindowCreationFlags::INITIAL_FULL_SCREEN;
        }
        MainWindowOpenMode::Restore(WindowGeometryState::Restored { rect }) => {
            size = rect.size();
            pos = rect.left_top();
            flags = bridge::WindowCreationFlags::MAIN;
        }
    }

    let w = NativeWindow::new(
        WindowType::Main {},
        flags,
        Rect::from_lt_size(pos, size),
        coreloop.as_mut(),
    );
    w.create_render_objects(coreloop);

    w.make_handle()
}

pub fn open_sub_window<'sys>(
    mode: SubWindowOpenMode,
    mut coreloop: Pin<&mut CoreLoop<'sys>>,
    setup_content: impl FnOnce(WindowHandle, Pin<&mut CoreLoop<'sys>>),
) -> WindowHandle {
    let (size, pos, flags);
    match mode {
        SubWindowOpenMode::Restore(WindowGeometryState::Maximized { monitor_index: _ }) => {
            // TODO: multi monitor support?
            size = Size::new_logical(960.0, 540.0);
            pos = Point::new_logical(0.0, 0.0);
            flags = bridge::WindowCreationFlags::INITIAL_FULL_SCREEN;
        }
        SubWindowOpenMode::Restore(WindowGeometryState::Restored { rect }) => {
            size = rect.size();
            pos = rect.left_top();
            flags = bridge::WindowCreationFlags::empty();
        }
        SubWindowOpenMode::DockDiverge {
            rect,
            position_ref_window,
        } => {
            size = rect.size();
            let mut x = rect.left as _;
            let mut y = rect.top as _;
            unsafe {
                bridge::ni_convert_point_to_screen(position_ref_window.0, &mut x, &mut y);
            }
            pos = Point::new_logical(x as _, y as _);
            flags = bridge::WindowCreationFlags::empty();
        }
    }

    let mut w = NativeWindow::new(
        WindowType::Sub,
        flags,
        Rect::from_lt_size(pos, size),
        coreloop.as_mut(),
    );
    let handle = w.make_handle();
    w.show();
    w.create_render_objects(coreloop.as_mut());

    setup_content(handle, coreloop);
    handle
}

pub fn close_sub_window(window_handle: WindowHandle) {
    let (done_event_sender, done_event_receiver) = std::sync::mpsc::channel();
    WindowDispatcher::get(window_handle.0)
        .coreloop()
        .syslink
        .rt_sender
        .send(RenderMessage::DestroyWindow(
            window_handle,
            done_event_sender,
        ))
        .expect("rt_sender.send.destroy_window");
    let tpctx = unsafe { bridge::ni_degreade_thread_priroity_temporarily() };
    done_event_receiver
        .recv()
        .expect("done_event_receiver.recv");
    unsafe {
        bridge::ni_restore_thread_priority(tpctx);
    }

    let d = WindowDispatcher::get_mut(window_handle.0);
    unsafe { &mut *d.coreloop }
        .composite_tree
        .free_all(d.state.composite_root);
    unsafe { &mut *d.coreloop }
        .ht_manager
        .free_all(d.state.ht_root);
    unsafe { &mut *d.coreloop }
        .keyboard_focus_registry
        .release_group(d.state.kf_root_group);

    unsafe {
        bridge::ni_release_window(window_handle.0);
    }
}

pub fn create_flyout_surface<'sys>(
    parent: WindowHandle,
    pos: Point<LogicalUnit>,
    size: Size<LogicalUnit>,
    mut coreloop: Pin<&mut CoreLoop<'sys>>,
) -> FlyoutSurfaceHandle {
    let h = FlyoutSurfaceHandle::new(parent, pos, size, coreloop.as_mut());

    let cl = unsafe { coreloop.get_unchecked_mut() };
    h.create_render_thread_objects(cl.syslink.gfx, &mut cl.delayed_render_messages);
    h
}

impl crate::SystemLink<'_> {
    pub const fn needs_app_menu_in_surface(&self) -> bool {
        // macOSはfalse固定
        false
    }

    pub fn prelaunch(&self, main_window: WindowHandle) {
        unsafe {
            self::bridge::ni_show_window_as_primary(main_window.0);
            bridge::ni_window_perform_postinit(main_window.0);
        }
    }

    pub fn set_cursor(&self, _pointer_id: &PointerID, cursor: CursorShape) {
        unsafe {
            self::bridge::ni_set_cursor_shape(match cursor {
                CursorShape::Default => self::bridge::CursorShape::Arrow as _,
                CursorShape::Pointer => self::bridge::CursorShape::Pointer as _,
                CursorShape::IBeam => self::bridge::CursorShape::IBeam as _,
                CursorShape::ResizeHorizontal => self::bridge::CursorShape::ResizeHorizontal as _,
                CursorShape::ResizeVertical => self::bridge::CursorShape::ResizeVertical as _,
            })
        }
    }

    pub fn notify_ui_scale_changes_to_render(&self, _window: WindowHandle, _new_scale: f32) {
        // TODO: これmacでやることあるのか？（起こらない気がする）
    }

    #[inline(always)]
    pub fn set_pointer_hovering_timeout(&self) {
        unsafe {
            self::bridge::ni_set_pointer_hovering_timeout(crate::input::POINTER_HOVER_TIMEOUT_MS);
        }
    }

    #[inline(always)]
    pub fn kill_pointer_hovering_timeout(&self) {
        unsafe {
            self::bridge::ni_kill_pointer_hovering_timeout();
        }
    }

    pub fn begin_pane_drag(
        &self,
        initiator: WindowHandle,
        _pointer: &PointerID,
        rect: &Rect<LogicalUnit>,
        state: DockingPreviewState,
    ) {
        let preview_rect = Rect::from_lt_size(state.offset, rect.size());
        unsafe { &mut *WindowDispatcher::get_mut(initiator.0).coreloop }.docking_preview_state =
            Some(state);
        unsafe {
            bridge::ni_show_drag_preview(
                preview_rect.left as _,
                preview_rect.top as _,
                preview_rect.width as _,
                preview_rect.height as _,
            );
            bridge::ni_begin_drag(initiator.0);
        }
    }

    #[inline(always)]
    pub fn update_pane_drag(&self, on_surface: WindowHandle, rect: &Rect<LogicalUnit>) {
        unsafe {
            bridge::ni_update_drag_preview_rect(
                on_surface.0,
                rect.left as _,
                rect.top as _,
                rect.width as _,
                rect.height as _,
            );
        }
    }

    #[inline(always)]
    pub fn end_pane_drag(&self) {
        unsafe {
            bridge::ni_hide_drag_preview();
        }
    }
}

pub struct NativeWindow {
    native_ptr: *mut bridge::WindowLink,
}
unsafe impl Sync for NativeWindow {}
unsafe impl Send for NativeWindow {}
impl NativeWindow {
    fn new<'sys>(
        window_type: WindowType,
        flags: bridge::WindowCreationFlags,
        rect: Rect<LogicalUnit>,
        mut coreloop: Pin<&mut CoreLoop<'sys>>,
    ) -> Self {
        let cl = unsafe { coreloop.as_mut().get_unchecked_mut() };

        let native_ptr = unsafe {
            bridge::ni_create_window(
                flags.bits(),
                rect.left as _,
                rect.top as _,
                rect.width as _,
                rect.height as _,
            )
        };
        let init_scale = unsafe { bridge::ni_get_content_scale(native_ptr) };
        let ct_root = CompositeRect::build()
            .expand_full()
            .create(&mut cl.composite_tree);
        let ht_root = HitTestTreeData::build()
            .expand_full()
            .root_of_window(WindowHandle(native_ptr))
            .create(&mut cl.ht_manager);
        let kf_root_group = cl.keyboard_focus_registry.acquire_group();
        Box::new(WindowDispatcher {
            coreloop: cl,
            window_type,
            state: WindowState {
                extra_data: core::ptr::null_mut(),
                swapchain_externally_invalidation_signal: Arc::new(AtomicBool::new(false)),
                latest_ui_scale_changes: Mutex::new(None),
                active_size: Mutex::new(rect.size()),
                active_rt_size: Mutex::new(rect.size().to_pixels_ceil(init_scale)),
                active_buffer_scale: Mutex::new(init_scale),
                composite_root: ct_root,
                ht_root,
                keyboard_focus_state: PerWindowKeyboardFocusState::new(kf_root_group),
                kf_root_group,
            },
        })
        .set(native_ptr);

        Self { native_ptr }
    }

    #[inline(always)]
    pub const fn make_handle(&self) -> WindowHandle {
        WindowHandle(self.native_ptr)
    }

    #[inline(always)]
    pub fn show(&mut self) {
        unsafe {
            bridge::ni_show_window(self.native_ptr);
        }
    }

    fn create_render_objects(&self, mut coreloop: Pin<&mut CoreLoop<'_>>) {
        let vk_surface = VulkanSurface::new(
            coreloop.syslink.gfx,
            unsafe {
                bedrock::MetalSurfaceCreateInfo::new(bridge::ni_get_metal_layer(self.native_ptr))
            }
            .execute(coreloop.syslink.gfx.instance(), None)
            .expect("vk_surface.create"),
        );
        unsafe { coreloop.as_mut().get_unchecked_mut() }
            .delayed_render_messages
            .push(RenderMessage::NewWindow(NewWindowData {
                key: self.make_handle(),
                vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            }));
    }

    #[inline(always)]
    pub fn manual_capture_begin(&self) {
        unsafe {
            self::bridge::manual_capture_begin(self.native_ptr);
        }
    }
}

struct WindowDispatcher<'sys> {
    coreloop: *mut CoreLoop<'sys>,
    window_type: WindowType,
    state: WindowState,
}
unsafe impl Sync for WindowDispatcher<'_> {}
unsafe impl Send for WindowDispatcher<'_> {}
impl<'sys> WindowDispatcher<'sys> {
    #[inline(always)]
    fn set(self: Box<Self>, w: *mut bridge::WindowLink) {
        let callbacks: &'static bridge::WindowLinkCallbacks = &const {
            bridge::WindowLinkCallbacks {
                destructor: Self::destructor,
                on_window_close: Self::on_window_close,
                on_resize: Self::on_resize,
                on_pointer_down: Self::on_pointer_down,
                on_pointer_move: Self::on_pointer_move,
                on_pointer_delta_move: Self::on_pointer_delta_move,
                on_pointer_up: Self::on_pointer_up,
                on_key_down: Self::on_key_down,
                on_key_down_with_char: Self::on_key_down_with_char,
                on_key_up: Self::on_key_up,
                on_key_up_with_char: Self::on_key_up_with_char,
                on_key_focus_state_changed: Self::on_key_focus_state_changed,
                on_scroll_wheel: Self::on_scroll_wheel,
                on_drag_begin: Self::on_drag_begin,
                on_drag_move: Self::on_drag_move,
                on_drag_end: Self::on_drag_end,
                on_dock_drag_enter_to_known_surface: Self::on_dock_drag_enter_to_known_surface,
                on_dock_drag_leave_from_known_surface: Self::on_dock_drag_leave_from_known_surface,
            }
        };

        unsafe {
            bridge::ni_set_window_callbacks(w, callbacks, Box::into_raw(self) as _);
        }
    }

    #[inline(always)]
    fn get<'a>(w: *mut bridge::WindowLink) -> &'a Self {
        unsafe { &*bridge::ni_get_window_callback_context(w).cast::<Self>() }
    }

    #[inline(always)]
    fn get_mut<'a>(w: *mut bridge::WindowLink) -> &'a mut Self {
        unsafe { &mut *bridge::ni_get_window_callback_context(w).cast::<Self>() }
    }

    extern "C" fn destructor(this: *mut core::ffi::c_void) {
        tracing::trace!(?this, "window_dispatcher.destruct");
        drop(unsafe { Box::from_raw(this.cast::<Self>()) });
    }

    const fn coreloop(&self) -> Pin<&mut CoreLoop<'sys>> {
        unsafe { core::pin::Pin::new_unchecked(&mut *self.coreloop) }
    }

    extern "C" fn on_window_close(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
    ) {
        let this = unsafe { &*caller_context.cast::<Self>() };
        if let WindowType::Sub = this.window_type {
            this.coreloop().close_sub_window(WindowHandle(window));
            this.coreloop().update_view_all();
        }
    }

    extern "C" fn on_resize(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
        width: f64,
        height: f64,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        let new_rt_size = Size::new_pixels(width as _, height as _);
        let mut active_rt_size_locked = this.state.active_rt_size.lock().expect("poisoned");
        if new_rt_size != *active_rt_size_locked {
            let logical_size =
                new_rt_size.to_logical(*this.state.active_buffer_scale.lock().expect("poisoned"));

            *active_rt_size_locked = new_rt_size;
            *this.state.active_size.lock().expect("poisoned") = logical_size;

            this.state
                .swapchain_externally_invalidation_signal
                .store(true, std::sync::atomic::Ordering::Relaxed);
            this.coreloop()
                .resize_window(WindowHandle(window), logical_size);
            this.coreloop().update_view_all();
        }
    }

    extern "C" fn on_pointer_down(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
        x: f64,
        y: f64,
        button: self::bridge::MouseButton,
        modifier_flags: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let target = WindowHandle(window);
        let pointer_id = PointerID();
        let key_modifier = translate_event_modifier_key(modifier_flags);

        this.coreloop().handle_pointer_move(
            target,
            pointer_id,
            Point::new_logical(x as _, y as _),
            key_modifier,
        );
        this.coreloop().handle_pointer_down(
            pointer_id,
            match button {
                bridge::MouseButton::Left => PointerButton::Primary,
                bridge::MouseButton::Right => PointerButton::Secondary,
            },
            key_modifier,
        );
        this.coreloop().update_view_all();
    }

    extern "C" fn on_pointer_move(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
        x: f64,
        y: f64,
        modifier_flags: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let target = WindowHandle(window);
        let pointer_id = PointerID();
        let key_modifier = translate_event_modifier_key(modifier_flags);

        this.coreloop().handle_pointer_move(
            target,
            pointer_id,
            Point::new_logical(x as _, y as _),
            key_modifier,
        );
        this.coreloop().update_view_all();
    }

    extern "C" fn on_pointer_delta_move(
        caller_context: *mut core::ffi::c_void,
        _window: *mut bridge::WindowLink,
        dx: f64,
        dy: f64,
        _modifier_flags: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let pointer_id = PointerID();

        this.coreloop()
            .handle_pointer_move_relative(pointer_id, Point::new_logical(dx as _, dy as _));
        this.coreloop().update_view_all();
    }

    extern "C" fn on_pointer_up(
        caller_context: *mut core::ffi::c_void,
        _window: *mut bridge::WindowLink,
        button: self::bridge::MouseButton,
        modifier_flags: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let pointer_id = PointerID();
        let key_modifier = translate_event_modifier_key(modifier_flags);

        this.coreloop().handle_pointer_up(
            pointer_id,
            match button {
                bridge::MouseButton::Left => PointerButton::Primary,
                bridge::MouseButton::Right => PointerButton::Secondary,
            },
            key_modifier,
        );
        this.coreloop().update_view_all();
    }

    extern "C" fn on_key_down(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
        code: u16,
        modifier_flags: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        this.coreloop().dispatch_key_down(
            WindowHandle(window),
            KeyInputCode::UnknownNativeCode(code as _),
            translate_event_modifier_key(modifier_flags),
        );
        this.coreloop().update_view_all();
    }

    extern "C" fn on_key_down_with_char(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
        _code: u16,
        modifier_flags: u32,
        char: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let char = unsafe { char::from_u32_unchecked(char) };
        let modifier = translate_event_modifier_key(modifier_flags);

        // Macの場合はいくつか文字コードで入ってくる
        match char {
            '\r' => this.coreloop().dispatch_key_down(
                WindowHandle(window),
                KeyInputCode::Enter,
                modifier,
            ),
            '\x08' => this.coreloop().dispatch_key_down(
                WindowHandle(window),
                KeyInputCode::Backspace,
                modifier,
            ),
            self::bridge::NS_UP_ARROW_FUNCTION_KEY => this.coreloop().dispatch_key_down(
                WindowHandle(window),
                KeyInputCode::UpArrow,
                modifier,
            ),
            self::bridge::NS_DOWN_ARROW_FUNCTION_KEY => this.coreloop().dispatch_key_down(
                WindowHandle(window),
                KeyInputCode::DownArrow,
                modifier,
            ),
            self::bridge::NS_LEFT_ARROW_FUNCTION_KEY => this.coreloop().dispatch_key_down(
                WindowHandle(window),
                KeyInputCode::LeftArrow,
                modifier,
            ),
            self::bridge::NS_RIGHT_ARROW_FUNCTION_KEY => this.coreloop().dispatch_key_down(
                WindowHandle(window),
                KeyInputCode::RightArrow,
                modifier,
            ),
            self::bridge::NS_HOME_FUNCTION_KEY => this.coreloop().dispatch_key_down(
                WindowHandle(window),
                KeyInputCode::Home,
                modifier,
            ),
            self::bridge::NS_END_FUNCTION_KEY => {
                this.coreloop()
                    .dispatch_key_down(WindowHandle(window), KeyInputCode::End, modifier)
            }
            self::bridge::NS_DELETE_FUNCTION_KEY => this.coreloop().dispatch_key_down(
                WindowHandle(window),
                KeyInputCode::Delete,
                modifier,
            ),
            c => {
                this.coreloop().dispatch_key_down(
                    WindowHandle(window),
                    KeyInputCode::Character(c),
                    modifier,
                );
                // 普通の文字の場合はKeyCharも発行（Windowsと挙動を合わせる）
                this.coreloop()
                    .dispatch_key_char(WindowHandle(window), c, modifier);
            }
        }
        this.coreloop().update_view_all();
    }

    extern "C" fn on_key_up(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        code: u16,
        modifier_flags: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        this.coreloop().dispatch_key_up(
            WindowHandle(window),
            KeyInputCode::UnknownNativeCode(code as _),
            translate_event_modifier_key(modifier_flags),
        );
        this.coreloop().update_view_all();
    }

    extern "C" fn on_key_up_with_char(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        code: u16,
        modifier_flags: u32,
        char: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let char = unsafe { char::from_u32_unchecked(char) };
        let modifier = translate_event_modifier_key(modifier_flags);

        // Macの場合はいくつか文字コードで入ってくる
        match char {
            '\r' => {
                this.coreloop()
                    .dispatch_key_up(WindowHandle(window), KeyInputCode::Enter, modifier)
            }
            '\x08' => this.coreloop().dispatch_key_up(
                WindowHandle(window),
                KeyInputCode::Backspace,
                modifier,
            ),
            self::bridge::NS_UP_ARROW_FUNCTION_KEY => this.coreloop().dispatch_key_up(
                WindowHandle(window),
                KeyInputCode::UpArrow,
                modifier,
            ),
            self::bridge::NS_DOWN_ARROW_FUNCTION_KEY => this.coreloop().dispatch_key_up(
                WindowHandle(window),
                KeyInputCode::DownArrow,
                modifier,
            ),
            self::bridge::NS_LEFT_ARROW_FUNCTION_KEY => this.coreloop().dispatch_key_up(
                WindowHandle(window),
                KeyInputCode::LeftArrow,
                modifier,
            ),
            self::bridge::NS_RIGHT_ARROW_FUNCTION_KEY => this.coreloop().dispatch_key_up(
                WindowHandle(window),
                KeyInputCode::RightArrow,
                modifier,
            ),
            self::bridge::NS_HOME_FUNCTION_KEY => {
                this.coreloop()
                    .dispatch_key_up(WindowHandle(window), KeyInputCode::Home, modifier)
            }
            self::bridge::NS_END_FUNCTION_KEY => {
                this.coreloop()
                    .dispatch_key_up(WindowHandle(window), KeyInputCode::End, modifier)
            }
            self::bridge::NS_DELETE_FUNCTION_KEY => this.coreloop().dispatch_key_up(
                WindowHandle(window),
                KeyInputCode::Delete,
                modifier,
            ),
            c => this.coreloop().dispatch_key_up(
                WindowHandle(window),
                KeyInputCode::Character(c),
                modifier,
            ),
        }
        this.coreloop().update_view_all();
    }

    extern "C" fn on_key_focus_state_changed(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
        focused: u8,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        this.coreloop()
            .notify_focus_changes(WindowHandle(window), focused != 0);
        this.coreloop().update_view_all();
    }

    extern "C" fn on_scroll_wheel(
        caller_context: *mut core::ffi::c_void,
        _window: *mut bridge::WindowLink,
        modifier_flags: u32,
        amount: f64,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        this.coreloop()
            .dispatch_scroll_wheel(amount as _, translate_event_modifier_key(modifier_flags));
        this.coreloop().update_view_all();
    }

    extern "C" fn on_drag_begin(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
        x: core::ffi::c_double,
        y: core::ffi::c_double,
    ) {
        tracing::debug!(?window, x, y, "on drag begin");
    }

    extern "C" fn on_drag_move(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
        mut x: core::ffi::c_double,
        mut y: core::ffi::c_double,
    ) {
        tracing::debug!(?window, x, y, "on drag move");

        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let docking_preview_state = unsafe { &*this.coreloop }
            .docking_preview_state
            .as_ref()
            .expect("not dragging");
        if let Some(entering_surface) = docking_preview_state.entering_surface {
            unsafe {
                bridge::ni_convert_point_from_screen(entering_surface.0, &mut x, &mut y);
            }
            this.coreloop()
                .move_redock_preview(entering_surface, Point::new_logical(x as _, y as _));
        } else {
            unsafe {
                bridge::ni_update_drag_preview_rect(
                    core::ptr::null_mut(),
                    x,
                    y,
                    docking_preview_state.original_rect.width as _,
                    docking_preview_state.original_rect.height as _,
                );
            }
        }
    }

    extern "C" fn on_drag_end(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
        mut x: core::ffi::c_double,
        mut y: core::ffi::c_double,
    ) {
        tracing::debug!(?window, x, y, "on drag end");

        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let docking_preview_state = unsafe { &*this.coreloop }
            .docking_preview_state
            .as_ref()
            .expect("no dragging");
        let destination_surface = docking_preview_state
            .entering_surface
            .unwrap_or(docking_preview_state.source_window);
        unsafe {
            bridge::ni_convert_point_from_screen(destination_surface.0, &mut x, &mut y);
        }
        this.coreloop()
            .confirm_redock(destination_surface, Point::new_logical(x as _, y as _));
    }

    extern "C" fn on_dock_drag_enter_to_known_surface(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
    ) {
        tracing::debug!("dock drag enter to known surface");

        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let docking_preview_state = unsafe { &mut *this.coreloop }
            .docking_preview_state
            .as_mut()
            .expect("not docking");
        docking_preview_state.entering_surface = Some(WindowHandle(window));
    }

    extern "C" fn on_dock_drag_leave_from_known_surface(
        caller_context: *mut core::ffi::c_void,
        window: *mut bridge::WindowLink,
    ) {
        tracing::debug!("dock drag leave from known surface");

        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let docking_preview_state = unsafe { &mut *this.coreloop }
            .docking_preview_state
            .as_mut()
            .expect("not docking");
        assert_eq!(
            docking_preview_state.entering_surface.take(),
            Some(WindowHandle(window)),
            "state drifting",
        );
    }
}

pub(self) fn translate_event_modifier_key(mk: u32) -> ModifierKey {
    let mut modifier = ModifierKey::empty();
    if (mk & self::bridge::NSEVENT_MODIFIER_FLAG_SHIFT) != 0 {
        modifier |= ModifierKey::SHIFT;
    }
    if (mk & self::bridge::NSEVENT_MODIFIER_FLAG_CONTROL) != 0 {
        modifier |= ModifierKey::CONTROL;
    }
    if (mk & self::bridge::NSEVENT_MODIFIER_FLAG_OPTION) != 0 {
        modifier |= ModifierKey::ALT;
    }
    if (mk & self::bridge::NSEVENT_MODIFIER_FLAG_COMMAND) != 0 {
        modifier |= ModifierKey::SUPER;
    }

    modifier
}

pub struct WindowState {
    extra_data: *mut core::ffi::c_void,
    pub swapchain_externally_invalidation_signal: Arc<AtomicBool>,
    pub latest_ui_scale_changes: Mutex<Option<f32>>,
    active_size: Mutex<Size<LogicalUnit>>,
    active_rt_size: Mutex<Size<PixelsUnit>>,
    active_buffer_scale: Mutex<f32>,
    composite_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    keyboard_focus_state: PerWindowKeyboardFocusState,
    kf_root_group: KeyboardFocusGroupRef,
}
unsafe impl Sync for WindowState {}
unsafe impl Send for WindowState {}

#[inline(always)]
pub fn ak_spacing_inline_start() -> &'static apple_sdk_port::foundation::String {
    unsafe {
        apple_sdk_port::foundation::String::from_internal_ref(&*bridge::ni_ak_spacing_inline_start())
    }
}

pub fn query_range_for_word_at(input: &str, at_utf16: usize) -> core::range::Range<u64> {
    let mut start = core::mem::MaybeUninit::uninit();
    let mut end = core::mem::MaybeUninit::uninit();

    unsafe {
        bridge::ni_query_range_for_word_at(
            input.as_ptr(),
            input.len() as _,
            at_utf16 as _,
            start.as_mut_ptr(),
            end.as_mut_ptr(),
        );
    }

    unsafe { (start.assume_init()..end.assume_init()).into() }
}
