use std::sync::Mutex;

use bedrock::{InstanceChild, SurfaceCreateInfo};

use crate::{
    ContextMenuHandle, Event, LogicFiberEventDispatcher, SyncEvent, SystemLink, WindowType,
    graphics::VulkanSurface,
    input::{
        KeyInputCode, KeyboardFocusGroupRef, KeyboardFocusTokenRegistry, ModifierKey,
        PerWindowKeyboardFocusState, PointerInputUnit,
        hittest::{
            CursorShape, HitTestTreeData, HitTestTreeManager, HitTestTreeRef, PointerButton,
        },
    },
    rendering::{
        NewWindowData, NewWindowVulkanSurface, RenderMessage,
        composite::{CompositeRect, CompositeTree, CompositeTreeRef},
    },
    uikit::{MenuItemLayout, MenuItemView, ViewInitContext},
    utils::{LogicalUnit, PixelsUnit, Point, Size},
};

pub mod bridge;
pub mod context_menu;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WindowHandle(*mut self::bridge::WindowLink);
unsafe impl Sync for WindowHandle {}
unsafe impl Send for WindowHandle {}
impl WindowHandle {
    #[inline(always)]
    pub fn state(&self) -> &MacWindowState {
        unsafe {
            &(*crate::platform::mac::bridge::ni_get_window_callback_context(self.0)
                .cast::<MacWindowDispatcher>())
            .state
        }
    }

    #[inline(always)]
    fn state_mut(&mut self) -> &mut MacWindowState {
        unsafe {
            &mut (*crate::platform::mac::bridge::ni_get_window_callback_context(self.0)
                .cast::<MacWindowDispatcher>())
            .state
        }
    }

    #[inline(always)]
    pub fn associate_extra_data<T>(&mut self, data: Box<T>) {
        self.state_mut().extra_data = Box::into_raw(data) as _;
    }

    #[inline(always)]
    pub unsafe fn extra_data_ref<T>(&self) -> &T {
        unsafe { &*self.state().extra_data.cast() }
    }

    #[inline(always)]
    pub unsafe fn extra_data_mut<T>(&mut self) -> &mut T {
        unsafe { &mut *self.state_mut().extra_data.cast() }
    }

    #[inline(always)]
    pub unsafe fn take_extra_data<T>(&mut self) -> Box<T> {
        let r = unsafe { Box::from_raw(self.state_mut().extra_data.cast()) };
        self.state_mut().extra_data = core::ptr::null_mut();

        r
    }

    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        let state = self.state();

        state
            .active_rt_size
            .lock()
            .expect("poisoned")
            .to_logical(*state.active_buffer_scale.lock().expect("poisoned"))
    }

    pub fn pixels_client_size(&self) -> Size<PixelsUnit> {
        *self.state().active_rt_size.lock().expect("poisoned")
    }

    #[inline(always)]
    pub fn ui_scale_factor(&self) -> f32 {
        *self.state().active_buffer_scale.lock().expect("poisoned")
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
    pub fn keyboard_focus_state(&self) -> &PerWindowKeyboardFocusState {
        &self.state().keyboard_focus_state
    }

    #[inline(always)]
    pub fn keyboard_focus_state_mut(&mut self) -> &mut PerWindowKeyboardFocusState {
        &mut self.state_mut().keyboard_focus_state
    }

    #[inline(always)]
    pub fn keyboard_focus_group(&self) -> KeyboardFocusGroupRef {
        self.state().kf_root_group
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

    // TODO: impl them
    pub fn on_click_sys_close_button(&self) {
        unimplemented!("WindowHandle::on_click_sys_close_button")
    }

    pub fn on_click_sys_maximize_button(&self) {
        unimplemented!("WindowHandle::on_click_sys_maximize_button")
    }

    pub fn on_click_sys_minimize_button(&self) {
        unimplemented!("WindowHandle::on_click_sys_minimize_button")
    }

    pub fn on_click_sys_restore_button(&self) {
        unimplemented!("WindowHandle::on_click_sys_restore_button")
    }
}
impl crate::input::ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {}

    #[inline(always)]
    fn release_pointer(&self) {}
}

pub struct DragPreviewPopoverHandle {
    position_base_window_link: core::cell::Cell<*mut self::bridge::WindowLink>,
}
impl DragPreviewPopoverHandle {
    #[inline(always)]
    pub fn new(_syslink: &SystemLink) -> Self {
        Self {
            position_base_window_link: core::cell::Cell::new(core::ptr::null_mut()),
        }
    }

    #[inline(always)]
    pub fn bind_position_base_window_link(&self, w: WindowHandle) {
        self.position_base_window_link.set(w.0);
    }

    pub fn show(&self, pos: &Point<PointerInputUnit>, size: &Size<LogicalUnit>) {
        unsafe {
            // macの場合はスクリーン座標が必要
            let mut x = pos.x as f64;
            let mut y = pos.y as f64;
            self::bridge::ni_convert_point_to_screen(
                self.position_base_window_link.get(),
                &mut x,
                &mut y,
            );

            self::bridge::ni_show_drag_preview(x, y, size.width as _, size.height as _);
        }
    }

    pub fn r#move(&self, pos: &Point<PointerInputUnit>) {
        unsafe {
            // macの場合はスクリーン座標が必要
            let mut x = pos.x as f64;
            let mut y = pos.y as f64;
            self::bridge::ni_convert_point_to_screen(
                self.position_base_window_link.get(),
                &mut x,
                &mut y,
            );

            self::bridge::ni_move_drag_preview(x, y);
        }
    }

    pub fn hide(&self) {
        unsafe {
            self::bridge::ni_hide_drag_preview();
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PointerID();
impl PointerID {
    pub fn surface_pos(&self) -> Point<LogicalUnit> {
        unimplemented!("PointerID::surface_pos")
    }
}

impl crate::SystemLink<'_> {
    pub fn create_main_window(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) -> WindowHandle {
        let mut w = MacWindow::new(
            WindowType::Main {},
            self::bridge::WindowCreationFlags::MAIN,
            unsafe { &*self.event_dispatcher }.clone(),
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            ht_manager.create(HitTestTreeData {
                width_adjustment_factor: 1.0,
                height_adjustment_factor: 1.0,
                ..Default::default()
            }),
            keyboard_focus_registry,
        );
        let main_window_handle = w.make_handle();
        w.make_primary_window();

        let vk_surface = VulkanSurface::new(unsafe { &*self.vk_device }, unsafe {
            bedrock::MetalSurfaceCreateInfo::new(w.metal_layer())
                .execute((&*self.vk_device).instance(), None)
                .expect("vk_surface.create")
        });
        self.rt_sender
            .send(RenderMessage::NewWindow(NewWindowData {
                /*init_scale: SafeF32::new(
                    *w.dispatcher()
                        .state
                        .active_buffer_scale
                        .lock()
                        .expect("poisoned"),
                )
                .expect("invalid scale"),
                latest_ui_scale_changes: utils::UnboundedRef::new(
                    &w.dispatcher().state.latest_ui_scale_changes,
                ),*/
                key: main_window_handle,
                vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            }))
            .expect("rt_sender.send");

        main_window_handle
    }

    pub fn prelaunch(&self, _main_window: WindowHandle) {
        // nothing to do
    }

    pub fn open_window<'h>(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        hit_tree: &mut HitTestTreeManager<'h>,
        keyboard_focus_manager: &mut KeyboardFocusTokenRegistry,
        setup_content: impl FnOnce(
            WindowHandle,
            &mut CompositeTree<SyncEvent>,
            &mut HitTestTreeManager<'h>,
            &mut KeyboardFocusTokenRegistry,
            &Self,
        ),
    ) -> WindowHandle {
        let mut w = MacWindow::new(
            WindowType::Sub,
            self::bridge::WindowCreationFlags::empty(),
            unsafe { (*self.event_dispatcher).clone() },
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            hit_tree.create(HitTestTreeData {
                width_adjustment_factor: 1.0,
                height_adjustment_factor: 1.0,
                ..Default::default()
            }),
            keyboard_focus_manager,
        );
        let handle = w.make_handle();
        w.show();
        // notify resize on show(register to pointer input manager)
        let mut width = core::mem::MaybeUninit::uninit();
        let mut height = core::mem::MaybeUninit::uninit();
        unsafe {
            self::bridge::ni_get_size_logical(w.native_ptr, width.as_mut_ptr(), height.as_mut_ptr())
        }
        unsafe { &*self.event_dispatcher }.dispatch(Event::WindowResize {
            window: handle,
            size: Size::new_logical(unsafe { width.assume_init() as _ }, unsafe {
                height.assume_init() as _
            }),
        });

        let vk_surface = VulkanSurface::new(unsafe { &*self.vk_device }, unsafe {
            bedrock::SurfaceCreateInfo::execute(
                &bedrock::MetalSurfaceCreateInfo::new(w.metal_layer()),
                bedrock::InstanceChild::instance(&*self.vk_device),
                None,
            )
            .expect("vk_surface.create")
        });
        self.rt_sender
            .send(RenderMessage::NewWindow(NewWindowData {
                /*init_scale: SafeF32::new(
                    *w.dispatcher()
                        .state
                        .active_buffer_scale
                        .lock()
                        .expect("poisoned"),
                )
                .expect("invalid scale"),
                latest_ui_scale_changes: utils::UnboundedRef::new(
                    &w.dispatcher().state.latest_ui_scale_changes,
                ),*/
                key: handle,
                vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            }))
            .expect("rt_sender.send");

        setup_content(
            handle,
            composite_tree,
            hit_tree,
            keyboard_focus_manager,
            self,
        );
        handle
    }

    pub fn close_window(
        &self,
        mut window_handle: WindowHandle,
        composite_tree: &mut CompositeTree<SyncEvent>,
        hit_tree: &mut HitTestTreeManager,
        keyboard_focus_manager: &mut KeyboardFocusTokenRegistry,
    ) {
        let (done_event_sender, done_event_receiver) = std::sync::mpsc::channel();
        self.rt_sender
            .send(RenderMessage::DestroyWindow(
                window_handle,
                done_event_sender,
            ))
            .expect("rt_sender.send.destroy_window");
        let tpctx = unsafe { self::bridge::ni_degreade_thread_priroity_temporarily() };
        done_event_receiver
            .recv()
            .expect("done_event_receiver.recv");
        unsafe {
            self::bridge::ni_restore_thread_priority(tpctx);
        }

        let st = window_handle.state_mut();
        composite_tree.free_all(st.composite_root);
        hit_tree.free_all(st.ht_root);
        keyboard_focus_manager.release_group(st.kf_root_group);

        unsafe {
            self::bridge::ni_release_window(window_handle.0);
        }
    }

    pub fn set_cursor(&self, _pointer_id: &PointerID, cursor: CursorShape) {
        unsafe {
            self::bridge::ni_set_cursor_shape(match cursor {
                CursorShape::Default => self::bridge::CursorShape::Arrow as _,
                CursorShape::Pointer => self::bridge::CursorShape::Pointer as _,
                CursorShape::IBeam => self::bridge::CursorShape::IBeam as _,
                CursorShape::ResizeHorizontal => self::bridge::CursorShape::ResizeHorizontal as _,
            })
        }
    }

    pub fn notify_ui_scale_changes_to_render(&self, _window: WindowHandle, _new_scale: f32) {
        // TODO: これmacでやることあるのか？（起こらない気がする）
    }

    #[inline(always)]
    pub fn set_pointer_hovering_timeout(&self) {
        unsafe {
            self::bridge::ni_set_pointer_hovering_timeout();
        }
    }

    #[inline(always)]
    pub fn kill_pointer_hovering_timeout(&self) {
        unsafe {
            self::bridge::ni_kill_pointer_hovering_timeout();
        }
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
            ContextMenuHandle,
            &mut ViewInitContext,
        ) -> Vec<MenuItemView>,
    ) -> ContextMenuHandle {
        unimplemented!("pop_context_menu")
    }

    pub fn any_pointer_on_context_menu(&self) -> bool {
        unimplemented!("any_pointer_on_context_menu")
    }
}

pub struct MacWindow {
    native_ptr: *mut self::bridge::WindowLink,
}
unsafe impl Sync for MacWindow {}
unsafe impl Send for MacWindow {}
impl MacWindow {
    pub fn new(
        window_type: WindowType,
        flags: self::bridge::WindowCreationFlags,
        event_dispatcher: LogicFiberEventDispatcher,
        composite_root: CompositeTreeRef,
        ht_root: HitTestTreeRef,
        keyboard_focus_manager: &mut KeyboardFocusTokenRegistry,
    ) -> Self {
        let native_ptr = unsafe { self::bridge::ni_create_window(flags.bits()) };
        let init_scale = unsafe { self::bridge::ni_get_content_scale(native_ptr) };
        let kf_root_group = keyboard_focus_manager.acquire_group();
        let dispatcher = Box::new(MacWindowDispatcher {
            event_dispatcher,
            window_type,
            state: MacWindowState {
                wlink: native_ptr,
                extra_data: core::ptr::null_mut(),
                swapchain_externally_invalidation_signal: std::sync::Arc::new(
                    std::sync::atomic::AtomicBool::new(false),
                ),
                latest_ui_scale_changes: Mutex::new(None),
                active_size: std::sync::Mutex::new(Size::new_logical(960.0, 540.0)),
                active_rt_size: std::sync::Mutex::new(
                    Size::new_logical(960.0, 540.0).to_pixels_ceil(init_scale),
                ),
                active_buffer_scale: std::sync::Mutex::new(init_scale),
                composite_root,
                ht_root,
                keyboard_focus_state: PerWindowKeyboardFocusState::new(kf_root_group),
                kf_root_group,
            },
        });
        let callbacks: &'static self::bridge::WindowLinkCallbacks =
            &self::bridge::WindowLinkCallbacks {
                destructor: MacWindowDispatcher::destructor,
                on_window_close: MacWindowDispatcher::on_window_close,
                on_resize: MacWindowDispatcher::on_resize,
                on_pointer_down: MacWindowDispatcher::on_pointer_down,
                on_pointer_move: MacWindowDispatcher::on_pointer_move,
                on_pointer_up: MacWindowDispatcher::on_pointer_up,
                on_key_down: MacWindowDispatcher::on_key_down,
                on_key_down_with_char: MacWindowDispatcher::on_key_down_with_char,
                on_key_up: MacWindowDispatcher::on_key_up,
            };
        unsafe {
            self::bridge::ni_set_window_callbacks(
                native_ptr,
                callbacks,
                Box::into_raw(dispatcher) as _,
            );
        }

        Self { native_ptr }
    }

    #[inline(always)]
    pub const fn make_handle(&self) -> WindowHandle {
        WindowHandle(self.native_ptr)
    }

    #[inline(always)]
    fn dispatcher(&self) -> &MacWindowDispatcher {
        unsafe { &*self::bridge::ni_get_window_callback_context(self.native_ptr).cast() }
    }

    #[inline(always)]
    fn dispatcher_mut(&mut self) -> &mut MacWindowDispatcher {
        unsafe { &mut *self::bridge::ni_get_window_callback_context(self.native_ptr).cast() }
    }

    #[inline(always)]
    pub fn make_primary_window(&mut self) {
        unsafe {
            self::bridge::ni_make_primary_window(self.native_ptr);
        }
    }

    #[inline(always)]
    pub fn show(&mut self) {
        unsafe {
            self::bridge::ni_show_window(self.native_ptr);
        }
    }

    #[inline(always)]
    pub fn metal_layer(&self) -> *mut core::ffi::c_void {
        unsafe { self::bridge::ni_get_metal_layer(self.native_ptr) }
    }

    #[inline(always)]
    pub fn manual_capture_begin(&self) {
        unsafe {
            self::bridge::manual_capture_begin(self.native_ptr);
        }
    }
}

struct MacWindowDispatcher {
    event_dispatcher: LogicFiberEventDispatcher,
    window_type: WindowType,
    state: MacWindowState,
}
unsafe impl Sync for MacWindowDispatcher {}
unsafe impl Send for MacWindowDispatcher {}
impl MacWindowDispatcher {
    extern "C" fn destructor(this: *mut core::ffi::c_void) {
        tracing::trace!(?this, "window_dispatcher.destruct");
        drop(unsafe { Box::from_raw(this.cast::<Self>()) });
    }

    extern "C" fn on_window_close(
        caller_context: *mut core::ffi::c_void,
        window: *mut self::bridge::WindowLink,
    ) {
        let this = unsafe { &*caller_context.cast::<Self>() };
        if let WindowType::Sub = this.window_type {
            this.event_dispatcher.dispatch(Event::SubWindowClose {
                window: WindowHandle(window),
            });
        }
    }

    extern "C" fn on_resize(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        width: f64,
        height: f64,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        let new_size = Size::new_logical(width as _, height as _);
        let mut active_size_locked = this.state.active_size.lock().expect("poisoned");
        if new_size != *active_size_locked {
            *active_size_locked = new_size;
            *this.state.active_rt_size.lock().expect("poisoned") =
                new_size.to_pixels_ceil(*this.state.active_buffer_scale.lock().expect("poisoned"));
            this.state
                .swapchain_externally_invalidation_signal
                .store(true, std::sync::atomic::Ordering::Relaxed);
            this.event_dispatcher.dispatch(Event::WindowResize {
                window: WindowHandle(window),
                size: new_size,
            });
        }
    }

    extern "C" fn on_pointer_down(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        x: f64,
        y: f64,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        // TODO: いったんPrimary固定
        // tracing::info!(x, y, "pointer down");
        this.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(window),
            client_pos: Point::new_logical(x as _, y as _),
        });
        this.event_dispatcher.dispatch(Event::PointerDown {
            window: WindowHandle(window),
            button: PointerButton::Primary,
            pointer_id: PointerID(),
        });
    }

    extern "C" fn on_pointer_move(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        x: f64,
        y: f64,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        // tracing::trace!(x, y, "pointer move");
        this.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(window),
            client_pos: Point::new_logical(x as _, y as _),
        });
    }

    extern "C" fn on_pointer_up(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        // TODO: いったんPrimary固定
        // tracing::info!("pointer up");
        this.event_dispatcher.dispatch(Event::PointerUp {
            window: WindowHandle(window),
            button: PointerButton::Primary,
            pointer_id: PointerID(),
        });
    }

    extern "C" fn on_key_down(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        code: u16,
        modifier_flags: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        let mut modifier = ModifierKey::empty();
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_SHIFT) != 0 {
            modifier |= ModifierKey::SHIFT;
        }
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_CONTROL) != 0 {
            modifier |= ModifierKey::CONTROL;
        }
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_OPTION) != 0 {
            modifier |= ModifierKey::ALT;
        }
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_COMMAND) != 0 {
            modifier |= ModifierKey::SUPER;
        }

        this.event_dispatcher.dispatch(Event::KeyDown {
            window: WindowHandle(window),
            code: KeyInputCode::UnknownNativeCode(code as _),
            modifier,
        });
    }

    extern "C" fn on_key_down_with_char(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        code: u16,
        modifier_flags: u32,
        char: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };
        let char = unsafe { char::from_u32_unchecked(char) };

        let mut modifier = ModifierKey::empty();
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_SHIFT) != 0 {
            modifier |= ModifierKey::SHIFT;
        }
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_CONTROL) != 0 {
            modifier |= ModifierKey::CONTROL;
        }
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_OPTION) != 0 {
            modifier |= ModifierKey::ALT;
        }
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_COMMAND) != 0 {
            modifier |= ModifierKey::SUPER;
        }

        this.event_dispatcher.dispatch(Event::KeyDown {
            window: WindowHandle(window),
            code: if char == '\r' {
                // これだけKeyInputCodeでとる
                KeyInputCode::Enter
            } else {
                KeyInputCode::Character(char)
            },
            modifier,
        });
    }

    extern "C" fn on_key_up(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        code: u16,
        modifier_flags: u32,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        let mut modifier = ModifierKey::empty();
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_SHIFT) != 0 {
            modifier |= ModifierKey::SHIFT;
        }
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_CONTROL) != 0 {
            modifier |= ModifierKey::CONTROL;
        }
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_OPTION) != 0 {
            modifier |= ModifierKey::ALT;
        }
        if (modifier_flags & self::bridge::NSEVENT_MODIFIER_FLAG_COMMAND) != 0 {
            modifier |= ModifierKey::SUPER;
        }

        this.event_dispatcher.dispatch(Event::KeyUp {
            window: WindowHandle(window),
            code: KeyInputCode::UnknownNativeCode(code as _),
            modifier,
        });
    }
}

pub struct MacWindowState {
    wlink: *mut self::bridge::WindowLink,
    extra_data: *mut core::ffi::c_void,
    swapchain_externally_invalidation_signal: std::sync::Arc<std::sync::atomic::AtomicBool>,
    pub latest_ui_scale_changes: Mutex<Option<f32>>,
    active_size: std::sync::Mutex<Size<LogicalUnit>>,
    active_rt_size: std::sync::Mutex<Size<PixelsUnit>>,
    active_buffer_scale: std::sync::Mutex<f32>,
    composite_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    keyboard_focus_state: PerWindowKeyboardFocusState,
    kf_root_group: KeyboardFocusGroupRef,
}
unsafe impl Sync for MacWindowState {}
unsafe impl Send for MacWindowState {}
