use std::{cell::RefCell, sync::Mutex};

use bedrock::{InstanceChild, SurfaceCreateInfo};
use tracing::{Level, Subscriber};
use tracing_subscriber::{Layer, fmt::FormatFields, registry::LookupSpan};

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
    pub fn state(&self) -> &WindowState {
        unsafe {
            &(*crate::platform::mac::bridge::ni_get_window_callback_context(self.0)
                .cast::<WindowDispatcher>())
            .state
        }
    }

    #[inline(always)]
    fn state_mut(&mut self) -> &mut WindowState {
        unsafe {
            &mut (*crate::platform::mac::bridge::ni_get_window_callback_context(self.0)
                .cast::<WindowDispatcher>())
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
        *self.state().active_size.lock().expect("poisoned")
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

impl crate::SystemLink<'_> {
    pub fn create_main_window(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) -> WindowHandle {
        let ht_root = ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });
        let w = NativeWindow::new(
            WindowType::Main {},
            self::bridge::WindowCreationFlags::MAIN,
            unsafe { &*self.event_dispatcher }.clone(),
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            ht_root,
            keyboard_focus_registry,
        );
        let main_window_handle = w.make_handle();
        ht_manager.get_data_mut(ht_root).root_of_window = Some(main_window_handle);

        let vk_surface = VulkanSurface::new(unsafe { &*self.vk_device }, unsafe {
            bedrock::MetalSurfaceCreateInfo::new(w.metal_layer())
                .execute((&*self.vk_device).instance(), None)
                .expect("vk_surface.create")
        });
        self.rt_sender
            .send(RenderMessage::NewWindow(NewWindowData {
                key: main_window_handle,
                vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            }))
            .expect("rt_sender.send");

        main_window_handle
    }

    pub fn prelaunch(&self, main_window: WindowHandle) {
        unsafe {
            self::bridge::ni_show_window_as_primary(main_window.0);
        }
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
        let ht_root = hit_tree.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });
        let mut w = NativeWindow::new(
            WindowType::Sub,
            self::bridge::WindowCreationFlags::empty(),
            unsafe { (*self.event_dispatcher).clone() },
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            ht_root,
            keyboard_focus_manager,
        );
        let handle = w.make_handle();
        hit_tree.get_data_mut(ht_root).root_of_window = Some(handle);
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
            self::bridge::ni_set_pointer_hovering_timeout(crate::input::POINTER_HOVER_TIMEOUT_MS);
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
        let mut h = ContextMenuHandle::new(
            parent,
            depth,
            surface_pos,
            self,
            &mut view_init_context.mount_context.composite_tree,
            &mut view_init_context.mount_context.ht_manager,
            &mut view_init_context.mount_context.keyboard_focus_registry,
        );

        let layouted_items = layouted_items(h.render_scale());
        let width = MenuItemLayout::min_width(layouted_items.iter());
        let height = MenuItemLayout::height(layouted_items.iter());
        h.resize(Size::new_logical(width.value(), height.value()));

        let views = setup_contents(layouted_items, h, view_init_context);
        h.set_views(views);

        h.create_render_thread_objects(self);
        h
    }
}

pub struct NativeWindow {
    native_ptr: *mut self::bridge::WindowLink,
}
unsafe impl Sync for NativeWindow {}
unsafe impl Send for NativeWindow {}
impl NativeWindow {
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
        let dispatcher = Box::new(WindowDispatcher {
            event_dispatcher,
            window_type,
            state: WindowState {
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
                destructor: WindowDispatcher::destructor,
                on_window_close: WindowDispatcher::on_window_close,
                on_resize: WindowDispatcher::on_resize,
                on_pointer_down: WindowDispatcher::on_pointer_down,
                on_pointer_move: WindowDispatcher::on_pointer_move,
                on_pointer_up: WindowDispatcher::on_pointer_up,
                on_key_down: WindowDispatcher::on_key_down,
                on_key_down_with_char: WindowDispatcher::on_key_down_with_char,
                on_key_up: WindowDispatcher::on_key_up,
                on_key_focus_state_changed: WindowDispatcher::on_key_focus_state_changed,
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

struct WindowDispatcher {
    event_dispatcher: LogicFiberEventDispatcher,
    window_type: WindowType,
    state: WindowState,
}
unsafe impl Sync for WindowDispatcher {}
unsafe impl Send for WindowDispatcher {}
impl WindowDispatcher {
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
            this.event_dispatcher.dispatch(Event::WindowResize {
                window: WindowHandle(window),
                size: logical_size,
            });
        }
    }

    extern "C" fn on_pointer_down(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        x: f64,
        y: f64,
        button: self::bridge::MouseButton,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        this.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(window),
            client_pos: Point::new_logical(x as _, y as _),
        });
        this.event_dispatcher.dispatch(Event::PointerDown {
            window: WindowHandle(window),
            button: match button {
                self::bridge::MouseButton::Left => PointerButton::Primary,
                self::bridge::MouseButton::Right => PointerButton::Secondary,
            },
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
        button: self::bridge::MouseButton,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        this.event_dispatcher.dispatch(Event::PointerUp {
            window: WindowHandle(window),
            button: match button {
                self::bridge::MouseButton::Left => PointerButton::Primary,
                self::bridge::MouseButton::Right => PointerButton::Secondary,
            },
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
        _code: u16,
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
            // Macの場合はいくつか文字コードで入ってくる
            code: match char {
                '\r' => KeyInputCode::Enter,
                '\x08' => KeyInputCode::Backspace,
                self::bridge::NS_LEFT_ARROW_FUNCTION_KEY => KeyInputCode::LeftArrow,
                self::bridge::NS_RIGHT_ARROW_FUNCTION_KEY => KeyInputCode::RightArrow,
                self::bridge::NS_HOME_FUNCTION_KEY => KeyInputCode::Home,
                self::bridge::NS_END_FUNCTION_KEY => KeyInputCode::End,
                self::bridge::NS_DELETE_FUNCTION_KEY => KeyInputCode::Delete,
                c => KeyInputCode::Character(c),
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

    extern "C" fn on_key_focus_state_changed(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        focused: u8,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        this.event_dispatcher.dispatch(Event::WindowFocusChanged {
            window: WindowHandle(window),
            focused: focused != 0,
        });
    }
}

pub struct WindowState {
    wlink: *mut self::bridge::WindowLink,
    extra_data: *mut core::ffi::c_void,
    pub swapchain_externally_invalidation_signal: std::sync::Arc<std::sync::atomic::AtomicBool>,
    pub latest_ui_scale_changes: Mutex<Option<f32>>,
    active_size: std::sync::Mutex<Size<LogicalUnit>>,
    active_rt_size: std::sync::Mutex<Size<PixelsUnit>>,
    active_buffer_scale: std::sync::Mutex<f32>,
    composite_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    keyboard_focus_state: PerWindowKeyboardFocusState,
    kf_root_group: KeyboardFocusGroupRef,
}
unsafe impl Sync for WindowState {}
unsafe impl Send for WindowState {}

pub trait TextInputClientForwarding {
    fn has_marked_text(&self) -> bool;
    fn marked_range(&self, out_location: *mut i64, out_length: *mut i64) -> bool;
    fn selected_range(&self, out_location: *mut i64, out_length: *mut i64);
    fn set_marked_text(
        &self,
        text: &core::ffi::CStr,
        new_selection_location: i64,
        new_selection_length: i64,
        replacement_location: i64,
        replacement_length: i64,
    );
    fn insert_text(
        &self,
        text: &core::ffi::CStr,
        replacement_location: i64,
        replacement_length: i64,
    );
    fn substring(
        &self,
        location: Option<i64>,
        length: i64,
        actual_location: *mut i64,
        actual_length: *mut i64,
        out_chars: *mut *const core::ffi::c_char,
        out_len: *mut u64,
    );
    fn first_rect(
        &self,
        location: i64,
        length: i64,
        actual_location: *mut i64,
        actual_length: *mut i64,
        surface_x: *mut f32,
        surface_y: *mut f32,
        width: *mut f32,
        height: *mut f32,
    );
}

pub struct LogLayer;
impl<S: Subscriber + for<'a> LookupSpan<'a>> Layer<S> for LogLayer {
    fn on_event(&self, event: &tracing::Event<'_>, ctx: tracing_subscriber::layer::Context<'_, S>) {
        thread_local! {
            static BUF: RefCell<String> = RefCell::new(String::new());
        }

        BUF.with(|buf| {
            let mut buflock = buf.try_borrow_mut();
            let mut tmpbuf;
            let mut buf = match buflock.as_mut() {
                Ok(x) => &mut *x,
                Err(_) => {
                    tmpbuf = String::new();
                    &mut tmpbuf
                }
            };
            let current_thread = std::thread::current();
            let nowtime = time::OffsetDateTime::from(std::time::SystemTime::now());

            struct StringIoWrite<'a>(&'a mut String);
            impl std::io::Write for StringIoWrite<'_> {
                #[inline(always)]
                fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                    self.0.push_str(unsafe { str::from_utf8_unchecked(buf) });
                    Ok(buf.len())
                }

                #[inline(always)]
                fn flush(&mut self) -> std::io::Result<()> {
                    Ok(())
                }
            }

            if let Err(_) = nowtime.format_into(
                &mut StringIoWrite(&mut buf),
                &time::format_description::well_known::Iso8601::DEFAULT,
            ) {
                unsafe {
                    self::bridge::ni_log_err(c"unable to format event".as_ptr().cast());
                }
                return;
            }
            let mut writer = tracing_subscriber::fmt::format::Writer::new(&mut *buf);
            if let Err(_) = write!(
                writer,
                " [{}] {}: ",
                event.metadata().level(),
                event.metadata().target()
            ) {
                unsafe {
                    self::bridge::ni_log_err(c"unable to format event".as_ptr().cast());
                }
                return;
            }
            if let Err(_) = tracing_subscriber::fmt::format::DefaultFields::new()
                .format_fields(writer.by_ref(), event)
            {
                unsafe {
                    self::bridge::ni_log_err(c"unable to format event".as_ptr().cast());
                }
                return;
            }
            if let Err(_) = write!(
                writer,
                "\n  at {}:{} ",
                event.metadata().file().unwrap_or("<unknown file>"),
                event.metadata().line().unwrap_or(0)
            ) {
                unsafe {
                    self::bridge::ni_log_err(c"unable to format event".as_ptr().cast());
                }
                return;
            }
            if let Err(_) = match current_thread.name() {
                Some(n) => write!(writer, "[{n}]"),
                None => write!(writer, "[ThreadID#{:?}]", current_thread.id()),
            } {
                unsafe {
                    self::bridge::ni_log_err(c"unable to format event".as_ptr().cast());
                }
                return;
            }

            if let Some(scope) = ctx.event_scope(event) {
                for s in scope {
                    if let Err(_) = write!(
                        writer,
                        "\n  in {}:{} {}",
                        s.metadata().file().unwrap_or("<unknown file>"),
                        s.metadata().line().unwrap_or(0),
                        s.name()
                    ) {
                        unsafe {
                            self::bridge::ni_log_err(c"unable to format event".as_ptr().cast());
                        }
                        return;
                    }
                }
            }

            if let Err(_) = write!(writer, "\0") {
                unsafe {
                    self::bridge::ni_log_err(c"unable to format event".as_ptr().cast());
                }
                return;
            }

            match event.metadata().level() {
                &Level::ERROR => unsafe { self::bridge::ni_log_err(buf.as_ptr()) },
                &Level::WARN => unsafe { self::bridge::ni_log_warn(buf.as_ptr()) },
                &Level::INFO => unsafe { self::bridge::ni_log_info(buf.as_ptr()) },
                &Level::DEBUG => unsafe { self::bridge::ni_log_debug(buf.as_ptr()) },
                &Level::TRACE => unsafe { self::bridge::ni_log_trace(buf.as_ptr()) },
            }

            buf.clear();
        })
    }
}

#[inline(always)]
pub fn ak_spacing_inline_start() -> &'static apple_sdk_port::foundation::String {
    unsafe {
        apple_sdk_port::foundation::String::from_internal_ref(
            &*self::bridge::ni_ak_spacing_inline_start(),
        )
    }
}

#[inline(always)]
pub fn ak_font_id() -> &'static apple_sdk_port::foundation::String {
    unsafe {
        apple_sdk_port::foundation::String::from_internal_ref(&*self::bridge::ni_ak_font_id())
    }
}
