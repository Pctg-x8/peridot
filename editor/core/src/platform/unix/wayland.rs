use core::ptr::NonNull;
use std::{
    collections::HashSet,
    os::fd::{AsRawFd, RawFd},
};

use linux_input::Key;
use peridot_tp_wayland as wl;
use peridot_tp_xkbcommon as xkbcommon;

use crate::{
    Event, LogicFiberEventDispatcher, MainWindowOpenMode, SubWindowOpenMode, SyncEvent,
    WindowGeometryState, WindowType,
    graphics::VulkanDevice,
    input::{
        KeyInputCode, KeyboardFocusTokenRegistry, ModifierKey,
        hittest::{CursorShape, HitTestTreeManager, PointerButton},
    },
    rendering::{RenderMessage, composite::CompositeTree},
    utils::{
        LogicalUnit, Point, Rect,
        platform::unix::{MappedMemory, TemporalSharedMemory, ftruncate},
        rup2,
    },
};

mod drag_preview;
pub mod flyout_surface;
mod toplevel;

pub use self::flyout_surface::Handle as FlyoutSurfaceHandle;
pub use self::toplevel::Handle as ToplevelHandle;

pub type WindowPersistentStateNativeGeometryUnit = LogicalUnit;

macro_rules! event_trace {
    ($($args:tt)+) => {
        tracing::trace!(target: "wl::event-trace", $($args)+);
    };
    () => {
        $crate::platform::unix::wayland::event_trace!("wayland event");
    }
}
pub(self) use event_trace;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SurfaceStateTag {
    ToplevelWindow,
    ResizeEdge,
    FlyoutSurface,
}

#[repr(C)]
struct SurfaceState<T> {
    tag: SurfaceStateTag,
    data: T,
}

#[repr(C)]
struct SurfaceStateUntyped {
    tag: SurfaceStateTag,
}

enum SurfaceScaling {
    Automatic,
    Manual {
        fractional_scale: wl::Owned<wl::WpFractionalScaleV1>,
        viewport: wl::Owned<wl::WpViewport>,
    },
}
impl SurfaceScaling {
    #[inline(always)]
    const fn is_manual(&self) -> bool {
        matches!(self, Self::Manual { .. })
    }
}

pub struct DisplayServerContext {
    pub(self) global_interfaces: GlobalInterfaces,
    pub(self) dp: wl::Display,
}
impl DisplayServerContext {
    pub fn connect() -> Self {
        let dp = wl::Display::connect().expect("display.connect");
        let global_interfaces =
            GlobalInterfaces::collect_sync(&dp).expect("global_interfaces.collect_sync");

        Self {
            dp,
            global_interfaces,
        }
    }

    pub fn display_fd(&self) -> RawFd {
        self.dp.as_raw_fd()
    }

    pub fn prepare_read(&mut self) -> Result<(), ()> {
        loop {
            match self.dp.prepare_read() {
                Ok(_) => break,
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    self.dp
                        .dispatch_pending()
                        .expect("wl.display.dispatch_pending");
                }
                Err(e) => {
                    tracing::error!(reason = ?e, "wl.display.prepare_read");
                    return Err(());
                }
            }
        }

        self.dp.flush().expect("wl.display.flush");
        Ok(())
    }

    pub fn process_events(&mut self) {
        self.dp.read_events().expect("wl_display.read_events");
        self.dp
            .dispatch_pending()
            .expect("wl_display.dispatch_pending");
    }

    pub fn cancel_reading(&mut self) {
        self.dp.cancel_read();
    }

    pub fn check_for_vk(&self, device: &VulkanDevice) -> bool {
        device.presentation_support(&self.dp)
    }

    pub fn bind_global_messaging(
        &mut self,
        global_messaging: core::pin::Pin<&mut GlobalMessaging>,
    ) {
        self.global_interfaces
            .bind_global_messaging(global_messaging);
    }
}

pub struct DisplayServerLink {
    pub context: *mut DisplayServerContext,
    pub static_pixbufs: *const StaticPixbufs,
    pub global_messaging_ptr: *mut GlobalMessaging,
}

impl crate::SystemLink<'_> {
    pub fn prelaunch(&self, _main_window: toplevel::Handle) {
        unsafe { &mut *self.display_server.context }
            .dp
            .roundtrip()
            .expect("roundtrip");
    }

    pub fn needs_app_menu_in_surface(&self) -> bool {
        unsafe { &*self.display_server.context }
            .global_interfaces
            .kde_appmenu_manager
            .is_none()
    }

    pub fn create_main_window(
        &self,
        mode: MainWindowOpenMode,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
        delayed_render_messages: &mut Vec<RenderMessage>,
    ) -> toplevel::Handle {
        let (target_output, pos, size, initial_maximize);
        match mode {
            MainWindowOpenMode::New => {
                target_output = None;
                pos = None;
                size = None;
                initial_maximize = false;
            }
            MainWindowOpenMode::Restore(WindowGeometryState::Restored { rect }) => {
                target_output = None;
                pos = Some(rect.left_top());
                size = Some(rect.size());
                initial_maximize = false;
            }
            MainWindowOpenMode::Restore(WindowGeometryState::Maximized { monitor_index }) => {
                let target_monitor = &unsafe { &*self.display_server.context }
                    .global_interfaces
                    .outputs
                    .get(monitor_index)
                    .unwrap_or(
                        &unsafe { &*self.display_server.context }
                            .global_interfaces
                            .outputs[0],
                    );

                target_output = Some::<&wl::Output>(&target_monitor.0);
                pos = Some(Point::new_logical(
                    target_monitor.1.x as _,
                    target_monitor.1.y as _,
                ));
                size = None;
                initial_maximize = true;
            }
        }

        toplevel::NativeWindow::new(
            WindowType::Main {
                #[cfg(target_os = "linux")]
                termination_event: self.terminate_event.clone(),
            },
            target_output,
            pos,
            size,
            initial_maximize,
            unsafe { &*self.display_server.context },
            unsafe { &*self.dbus },
            unsafe { &*self.event_dispatcher }.clone(),
            composite_tree,
            ht_manager,
            keyboard_focus_registry,
            unsafe { &*self.display_server.static_pixbufs }
                .window_decoration
                .as_ref(),
            unsafe { &*self.vk_device },
            delayed_render_messages,
        )
        .into_handle()
    }

    pub fn open_window<'h>(
        &self,
        mode: SubWindowOpenMode,
        composite_tree: &mut CompositeTree<SyncEvent>,
        hit_tree: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
        delayed_render_messages: &mut Vec<RenderMessage>,
        setup_contents: impl FnOnce(
            toplevel::Handle,
            &mut CompositeTree<SyncEvent>,
            &mut HitTestTreeManager,
            &mut KeyboardFocusTokenRegistry,
            &Self,
        ),
    ) -> toplevel::Handle {
        let (target_output, pos, size, initial_maximize);
        match mode {
            SubWindowOpenMode::DockDiverge { rect, .. } => {
                target_output = None;
                pos = Some(rect.left_top());
                size = Some(rect.size());
                initial_maximize = false;
            }
            SubWindowOpenMode::Restore(WindowGeometryState::Restored { rect }) => {
                target_output = None;
                pos = Some(rect.left_top());
                size = Some(rect.size());
                initial_maximize = false;
            }
            SubWindowOpenMode::Restore(WindowGeometryState::Maximized { monitor_index }) => {
                let target = &unsafe { &*self.display_server.context }
                    .global_interfaces
                    .outputs
                    .get(monitor_index)
                    .unwrap_or(
                        &unsafe { &*self.display_server.context }
                            .global_interfaces
                            .outputs[0],
                    );

                target_output = Some::<&wl::Output>(&target.0);
                pos = Some(Point::new_logical(target.1.x as _, target.1.y as _));
                size = None;
                initial_maximize = true;
            }
        }

        let w = toplevel::NativeWindow::new(
            WindowType::Sub,
            target_output,
            pos,
            size,
            initial_maximize,
            unsafe { &*self.display_server.context },
            unsafe { &*self.dbus },
            unsafe { &*self.event_dispatcher }.clone(),
            composite_tree,
            hit_tree,
            keyboard_focus_registry,
            unsafe { &*self.display_server.static_pixbufs }
                .window_decoration
                .as_ref(),
            unsafe { &*self.vk_device },
            delayed_render_messages,
        );

        setup_contents(
            w.make_handle(),
            composite_tree,
            hit_tree,
            keyboard_focus_registry,
            self,
        );
        w.commit();
        w.into_handle()
    }

    pub fn close_window(
        &self,
        window_handle: toplevel::Handle,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
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

        toplevel::NativeWindow::from_handle(window_handle).terminate(
            composite_tree,
            ht_manager,
            keyboard_focus_registry,
        );
    }

    pub fn set_cursor(&self, _pointer_id: &PointerID, cursor: CursorShape) {
        if let Some(&PointerState {
            enter_state: Some(PointerEnterState { serial, .. }),
            cursor: Some(ref shape_device),
            ..
        }) = unsafe { &*self.display_server.global_messaging_ptr }
            .pointer
            .as_ref()
        {
            shape_device
                .set_shape(
                    serial,
                    match cursor {
                        CursorShape::Default => wl::WpCursorShapeDeviceV1Shape::Default,
                        CursorShape::Pointer => wl::WpCursorShapeDeviceV1Shape::Pointer,
                        CursorShape::IBeam => wl::WpCursorShapeDeviceV1Shape::Text,
                        CursorShape::ResizeHorizontal => wl::WpCursorShapeDeviceV1Shape::EwResize,
                        CursorShape::ResizeVertical => wl::WpCursorShapeDeviceV1Shape::NsResize,
                    },
                )
                .expect("cursor_shape_device.set_cursor");
        }
    }

    pub fn set_ime_cursor_rect(&self, r: Rect<LogicalUnit>) {
        if let Some(ti) = unsafe { &*self.display_server.global_messaging_ptr }
            .keyboard
            .as_ref()
            .and_then(|x| x.text_input.as_ref())
        {
            ti.set_cursor_rectangle(r.left as _, r.top as _, r.width as _, r.height as _)
                .expect("text_input.set_cursor_rectangle");
        }
    }

    pub fn ime_set_surrounding_text(&self, text: &str, cursor: usize, anchor: usize) {
        if let Some(ti) = unsafe { &*self.display_server.global_messaging_ptr }
            .keyboard
            .as_ref()
            .and_then(|x| x.text_input.as_ref())
        {
            ti.set_surrounding_text(
                &std::ffi::CString::new(text).expect("cstring.new"),
                cursor as _,
                anchor as _,
            )
            .expect("text_input.set_surrounding_text");
        }
    }

    pub fn ime_commit(&self) {
        if let Some(ti) = unsafe { &*self.display_server.global_messaging_ptr }
            .keyboard
            .as_ref()
            .and_then(|x| x.text_input.as_ref())
        {
            ti.commit().expect("text_input.commit");
        }
    }

    pub fn set_pointer_hovering_timeout(&self) {
        unsafe { &*self.pointer_hovering_timer }
            .set(
                0,
                crate::input::POINTER_HOVER_TIMEOUT_MS as i64 * 1000 * 1000,
            )
            .expect("timer.set");
    }

    pub fn kill_pointer_hovering_timeout(&self) {
        unsafe { &*self.pointer_hovering_timer }
            .unset()
            .expect("timer.unset");
    }

    pub fn any_pointer_on_context_menu(&self) -> bool {
        let Some(PointerState {
            enter_state: Some(ref p),
            ..
        }) = unsafe { &*self.display_server.global_messaging_ptr }.pointer
        else {
            return false;
        };

        unsafe { &*p.surface.as_ref().user_data().cast::<SurfaceStateUntyped>() }.tag
            == SurfaceStateTag::FlyoutSurface
    }

    pub fn any_pointer_on_dropdown_menu(&self) -> bool {
        let Some(PointerState {
            enter_state: Some(ref p),
            ..
        }) = unsafe { &*self.display_server.global_messaging_ptr }.pointer
        else {
            return false;
        };

        // TODO: ContextMenuと区別ができてないのでなんとかしたい(でもしなくてもいいか......？)
        unsafe { &*p.surface.as_ref().user_data().cast::<SurfaceStateUntyped>() }.tag
            == SurfaceStateTag::FlyoutSurface
    }

    pub fn query_window_under_pointer(&self, pointer: &PointerID) -> Option<toplevel::Handle> {
        let global_msg = unsafe { &mut *(*pointer.0).user_data().cast::<GlobalMessaging>() };
        let surface = global_msg
            .pointer
            .as_ref()
            .expect("no pointer?")
            .enter_state
            .as_ref()?
            .surface;
        Some(toplevel::Handle(surface))
    }

    pub fn begin_pane_drag(
        &self,
        initiator: toplevel::Handle,
        pointer: &PointerID,
        offset: Point<LogicalUnit>,
        rect: &Rect<LogicalUnit>,
    ) {
        tracing::debug!("begin_pane_drag");

        let global_msg = unsafe { &mut *(*pointer.0).user_data().cast::<GlobalMessaging>() };
        let mut data_source = unsafe { global_msg.data_device_manager.as_ref() }
            .create_data_source()
            .expect("data_device_manager.create_data_source");
        data_source
            .set_actions(wl::DataDeviceManagerDndAction::MOVE)
            .expect("data_source.set_actions");
        data_source
            .offer(c"application/x-pme-dock-content")
            .expect("data_source.offer");
        data_source
            .add_listener(global_msg)
            .into_result()
            .expect("data_source.add_listener");

        global_msg.drag_preview_popover.setup_dnd_icon_surface(
            &unsafe { &*self.display_server.context }.global_interfaces,
            offset,
            &rect.size(),
        );
        let pointer_enter_state = global_msg
            .pointer
            .as_ref()
            .expect("no pointer")
            .enter_state
            .as_ref()
            .expect("not entering");
        let dd = global_msg.data_device.as_mut().expect("no data device");
        dd.data_device
            .start_drag(
                // Note: 仕様上はnullにできるはずだがHyprlandでやるとCompositorごとおちる
                Some(&data_source),
                unsafe { initiator.0.as_ref() },
                Some(global_msg.drag_preview_popover.dnd_icon_surface()),
                pointer_enter_state
                    .implicit_grab_serial
                    .expect("not grabbing implicitly"),
            )
            .expect("data_device.start_drag");
        global_msg.drag_preview_popover.post_commit_dnd_icon();
        dd.pane_drag_state = Some(PaneDragState {
            source: data_source,
            initiator,
        });
    }

    pub fn update_pane_drag(&self, _on_surface: toplevel::Handle, rect: &Rect<LogicalUnit>) {
        let global_msg = unsafe { &mut *self.display_server.global_messaging_ptr };
        global_msg.drag_preview_popover.set_surface_rect(
            &unsafe { &*self.display_server.context }.global_interfaces,
            rect,
        );
    }

    pub fn end_pane_drag(&self) {
        let global_msg = unsafe { &mut *self.display_server.global_messaging_ptr };
        global_msg
            .drag_preview_popover
            .teardown_dynamic_resoureces();
        global_msg
            .data_device
            .as_mut()
            .expect("no data device")
            .pane_drag_state
            .take();
    }
}

pub struct StaticPixbufs {
    shm: Option<(wl::Owned<wl::ShmPool>, MappedMemory, TemporalSharedMemory)>,
    window_decoration: Option<toplevel::DecorationPixbuf>,
}
impl StaticPixbufs {
    pub fn new(ctx: &DisplayServerContext) -> Self {
        let popover_buf_shm_bytes = if ctx.global_interfaces.single_pixel_buffer_manager.is_some() {
            0
        } else {
            4
        };
        let window_decoration_pixbuf_offset = rup2(
            popover_buf_shm_bytes,
            toplevel::DecorationPixbuf::REQUIRED_BYTE_ALIGNMENT,
        );
        let shm_total_byte_length = window_decoration_pixbuf_offset
            + if toplevel::should_client_decoration(&ctx.global_interfaces) {
                toplevel::DecorationPixbuf::REQUIRED_BYTE_LENGTH
            } else {
                0
            };

        let shm_pair = if shm_total_byte_length > 0 {
            let shm_region = TemporalSharedMemory::new_unique(c"/pme_shm_st", libc::O_RDWR, 0o0600)
                .expect("buf.shm.create")
                .expect("buf.shm.create.non_unique");
            unsafe {
                ftruncate(&shm_region, shm_total_byte_length as _).expect("buf.shm.resize");
            }

            let mapped = MappedMemory::new(
                None,
                shm_total_byte_length,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED,
                &shm_region,
                0,
            )
            .expect("buf.mmap");

            let shmp = ctx
                .global_interfaces
                .shm
                .create_pool(&shm_region, shm_total_byte_length as _)
                .expect("shmp.create.popup");

            Some((shmp, mapped, shm_region))
        } else {
            None
        };

        if ctx.global_interfaces.single_pixel_buffer_manager.is_none() {
            // setup for traditional shm-based single pixel buffer
            let (_, mapped, _) = shm_pair.as_ref().expect("no shm");

            unsafe {
                core::ptr::write(
                    mapped.as_ptr().cast::<u32>(),
                    crate::DRAG_PREVIEW_POPOVER_BG_COLOR
                        .premultiplied()
                        .argb8888(),
                );
            }
        }
        let window_decoration_pixbuf = if toplevel::should_client_decoration(&ctx.global_interfaces)
        {
            let (shm, mapped, _) = shm_pair.as_ref().expect("no shm");

            toplevel::DecorationPixbuf::generate_content(unsafe {
                mapped.as_ptr().byte_add(window_decoration_pixbuf_offset)
            });
            Some(toplevel::DecorationPixbuf::new(
                shm,
                window_decoration_pixbuf_offset,
            ))
        } else {
            None
        };

        Self {
            shm: shm_pair,
            window_decoration: window_decoration_pixbuf,
        }
    }

    fn create_drag_preview_popover_bufs(
        &self,
        interfaces: &GlobalInterfaces,
    ) -> drag_preview::Buffer {
        if let Some(ref spb) = interfaces.single_pixel_buffer_manager {
            let c = crate::DRAG_PREVIEW_POPOVER_BG_COLOR.premultiplied();
            let b = spb
                .create_u32_rgba_buffer(c.r_u32(), c.g_u32(), c.b_u32(), c.a_u32())
                .expect("popup_buf.create.single_pixel_buffer");

            drag_preview::Buffer::SinglePixel(b)
        } else {
            // traditional shm-based single pixel buffer
            let (shm, _, _) = self.shm.as_ref().expect("no shm");

            let buf = shm
                .create_buffer(0, 1, 1, 4, wl::ShmFormat::ARGB8888)
                .expect("buf.create.popup");

            drag_preview::Buffer::Shm { buf }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PointerID(*mut wl::Pointer);
impl PointerID {
    pub fn acquire_lock_on_surface(&self) {
        let global_msg = unsafe { &mut *(*self.0).user_data().cast::<GlobalMessaging>() };
        let Some(rpm) = global_msg.relative_pointer_manager else {
            // no locking supported by compositor
            return;
        };

        let surface = unsafe {
            global_msg
                .pointer
                .as_ref()
                .expect("no pointer?")
                .enter_state
                .as_ref()
                .expect("not entering surface")
                .surface
                .as_ref()
        };

        let mut relative_pointer_object = unsafe { rpm.as_ref() }
            .get_relative_pointer(unsafe { &*self.0 })
            .expect("relative_pointer_manager.get_relative_pointer");
        let mut lock_constraint_object = if let Some(pc) = global_msg.pointer_constraints {
            Some(
                unsafe { pc.as_ref() }
                    .lock_pointer(
                        surface,
                        unsafe { &*self.0 },
                        None,
                        wl::ZwpPointerConstraintsV1Lifetime::Oneshot,
                    )
                    .expect("pointer_constraints.lock_pointer"),
            )
        } else {
            None
        };

        relative_pointer_object
            .set_listener(global_msg)
            .into_result()
            .expect("relative_pointer_object.set_listener");
        if let Some(ref mut o) = lock_constraint_object {
            o.set_listener(global_msg)
                .into_result()
                .expect("lock_constraint_object.set_listener");
        }

        global_msg
            .pointer
            .as_mut()
            .expect("no pointer?")
            .enter_state
            .as_mut()
            .expect("not entering surface")
            .lock_state = Some(PointerLockState {
            _lock_constraint_object: lock_constraint_object,
            _relative_pointer_object: relative_pointer_object,
        });
    }

    pub fn release_lock(&self) {
        unsafe { &mut *(*self.0).user_data().cast::<GlobalMessaging>() }
            .pointer
            .as_mut()
            .expect("no pointer?")
            .enter_state
            .as_mut()
            .expect("not entering surface")
            .lock_state = None;
    }
}

#[derive(Clone, Debug)]
pub struct PointerEventID {
    serial: u32,
    seat_ptr: *mut wl::Seat,
}
unsafe impl Sync for PointerEventID {}
unsafe impl Send for PointerEventID {}

struct PointerEnterState {
    surface: NonNull<wl::Surface>,
    serial: u32,
    implicit_grab_serial: Option<u32>,
    lock_state: Option<PointerLockState>,
}

struct PointerLockState {
    _lock_constraint_object: Option<wl::Owned<wl::ZwpLockedPointerV1>>,
    _relative_pointer_object: wl::Owned<wl::ZwpRelativePointerV1>,
}

pub struct PointerState {
    _wl_object: wl::Owned<wl::Pointer>,
    seat_ptr: *mut wl::Seat,
    cursor: Option<wl::Owned<wl::WpCursorShapeDeviceV1>>,
    pos: Point<LogicalUnit>,
    enter_state: Option<PointerEnterState>,
}

struct KeyboardEnterState {
    pub surface: NonNull<wl::Surface>,
}

struct KeyboardState {
    _wl_object: wl::Owned<wl::Keyboard>,
    xkb_keymap: Option<xkbcommon::Keymap>,
    xkb_state: Option<xkbcommon::State>,
    xkb_shift_mod_index: Option<u32>,
    xkb_alt_mod_index: Option<u32>,
    xkb_ctrl_mod_index: Option<u32>,
    xkb_super_mod_index: Option<u32>,
    text_input: Option<wl::Owned<wl::ZwpTextInputV3>>,
    enter_state: Option<KeyboardEnterState>,
}
impl KeyboardState {
    pub fn build_modifier(&self) -> ModifierKey {
        let mut modifier = ModifierKey::empty();

        if let Some(ref xkb_state) = self.xkb_state {
            if self.xkb_shift_mod_index.is_some_and(|n| {
                xkb_state.mod_index_is_active(n, xkbcommon::StateComponent::MODS_EFFECTIVE)
            }) {
                modifier |= ModifierKey::SHIFT;
            }
            if self.xkb_alt_mod_index.is_some_and(|n| {
                xkb_state.mod_index_is_active(n, xkbcommon::StateComponent::MODS_EFFECTIVE)
            }) {
                modifier |= ModifierKey::ALT;
            }
            if self.xkb_ctrl_mod_index.is_some_and(|n| {
                xkb_state.mod_index_is_active(n, xkbcommon::StateComponent::MODS_EFFECTIVE)
            }) {
                modifier |= ModifierKey::CONTROL;
            }
            if self.xkb_super_mod_index.is_some_and(|n| {
                xkb_state.mod_index_is_active(n, xkbcommon::StateComponent::MODS_EFFECTIVE)
            }) {
                modifier |= ModifierKey::SUPER;
            }
        }

        modifier
    }
}

struct IMEPendingState {
    committed_text: Option<String>,
    preedit_text: Option<String>,
}

struct DataDeviceActiveOfferState {
    object: wl::Owned<wl::DataOffer>,
    entering_surface: Option<NonNull<wl::Surface>>,
    client_pos: Point<LogicalUnit>,
    mime_types: HashSet<std::ffi::CString>,
    source_actions: wl::DataDeviceManagerDndAction,
}

struct PaneDragState {
    source: wl::Owned<wl::DataSource>,
    initiator: toplevel::Handle,
}

struct DataDeviceState {
    data_device: wl::Owned<wl::DataDevice>,
    pane_drag_state: Option<PaneDragState>,
    active_offer: Option<DataDeviceActiveOfferState>,
}

pub struct GlobalMessaging {
    global_interfaces: *const GlobalInterfaces,
    text_input_manager: NonNull<wl::ZwpTextInputManagerV3>,
    data_device_manager: NonNull<wl::DataDeviceManager>,
    xkb_context: xkbcommon::Context,
    keyboard: Option<KeyboardState>,
    pointer: Option<PointerState>,
    data_device: Option<DataDeviceState>,
    cursor_shape_manager: Option<NonNull<wl::WpCursorShapeManagerV1>>,
    pointer_constraints: Option<NonNull<wl::ZwpPointerConstraintsV1>>,
    relative_pointer_manager: Option<NonNull<wl::ZwpRelativePointerManagerV1>>,
    event_dispatcher: LogicFiberEventDispatcher,
    ime_pending_state: IMEPendingState,
    drag_preview_popover: drag_preview::Controller,
    _pinned: core::marker::PhantomPinned,
}
impl GlobalMessaging {
    pub fn new(
        ctx: &mut DisplayServerContext,
        static_pixbufs: &StaticPixbufs,
        event_dispatcher: LogicFiberEventDispatcher,
    ) -> Self {
        Self {
            global_interfaces: &ctx.global_interfaces,
            text_input_manager: unsafe { ctx.global_interfaces.text_input_manager.copy_ptr() },
            data_device_manager: unsafe { ctx.global_interfaces.data_device_manager.copy_ptr() },
            xkb_context: xkbcommon::Context::new(xkbcommon::ContextFlags::NO_FLAGS)
                .expect("xkb_context.create"),
            keyboard: None,
            pointer: None,
            data_device: None,
            cursor_shape_manager: ctx
                .global_interfaces
                .cursor_shape_manager
                .as_ref()
                .map(|x| unsafe { x.copy_ptr() }),
            pointer_constraints: ctx
                .global_interfaces
                .pointer_constraints
                .as_ref()
                .map(|x| unsafe { x.copy_ptr() }),
            relative_pointer_manager: ctx
                .global_interfaces
                .relative_pointer_manager
                .as_ref()
                .map(|x| unsafe { x.copy_ptr() }),
            event_dispatcher,
            ime_pending_state: IMEPendingState {
                committed_text: None,
                preedit_text: None,
            },
            drag_preview_popover: drag_preview::Controller::new(ctx, static_pixbufs),
            _pinned: core::marker::PhantomPinned,
        }
    }

    pub fn reset_event_dispatcher(
        self: core::pin::Pin<&mut Self>,
        event_dispatcher: LogicFiberEventDispatcher,
    ) {
        unsafe {
            self.get_unchecked_mut().event_dispatcher = event_dispatcher;
        }
    }
}
impl wl::XdgWmBaseEventListener for GlobalMessaging {
    #[inline(always)]
    fn ping(&mut self, sender: &mut peridot_tp_wayland::XdgWmBase, serial: u32) {
        sender.pong(serial).expect("xdg_wm_base pong");
    }
}
impl wl::SeatEventListener for GlobalMessaging {
    #[tracing::instrument(skip(self, seat))]
    fn capabilities(&mut self, seat: &mut wl::Seat, capabilities: wl::SeatCapability) {
        event_trace!();

        let mut dd = unsafe { self.data_device_manager.as_ref() }
            .get_data_device(seat)
            .expect("seat.get_data_device");
        dd.set_listener(self)
            .into_result()
            .expect("data_device.set_listener");
        self.data_device = Some(DataDeviceState {
            data_device: dd,
            pane_drag_state: None,
            active_offer: None,
        });

        if capabilities.contains(wl::SeatCapability::POINTER) {
            // pointer
            let mut p = seat.get_pointer().expect("seat.get_pointer");
            p.set_listener(self)
                .into_result()
                .expect("pointer.set_listener");
            let c = if let Some(mgr) = self.cursor_shape_manager {
                Some(
                    unsafe { mgr.as_ref() }
                        .get_pointer(&p)
                        .expect("cursor_shape_manager.get_pointer"),
                )
            } else {
                None
            };

            self.pointer = Some(PointerState {
                _wl_object: p,
                seat_ptr: seat as *mut _,
                cursor: c,
                pos: Point::new_logical(0.0, 0.0),
                enter_state: None,
            });
        } else {
            // remove pointer
            self.pointer = None;
        }

        if capabilities.contains(wl::SeatCapability::KEYBOARD) {
            let mut k = seat.get_keyboard().expect("seat.get_keyboard");
            k.set_listener(self)
                .into_result()
                .expect("keyboard.set_listener");
            let mut ti = unsafe { self.text_input_manager.as_ref() }
                .get_text_input(seat)
                .expect("text_input_manager.get_text_input");
            ti.set_listener(self)
                .into_result()
                .expect("text_input.set_listener");

            self.keyboard = Some(KeyboardState {
                _wl_object: k,
                xkb_keymap: None,
                xkb_state: None,
                xkb_shift_mod_index: None,
                xkb_alt_mod_index: None,
                xkb_ctrl_mod_index: None,
                xkb_super_mod_index: None,
                text_input: Some(ti),
                enter_state: None,
            });
        } else {
            // remove keyboard
            self.keyboard = None;
        }
    }

    #[tracing::instrument(skip(self, _seat))]
    fn name(&mut self, _seat: &mut wl::Seat, name: &core::ffi::CStr) {
        event_trace!();
    }
}
impl wl::PointerEventListener for GlobalMessaging {
    #[tracing::instrument(name = "pointer::enter", skip(self, pointer, surface), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
    fn enter(
        &mut self,
        pointer: &mut wl::Pointer,
        serial: u32,
        surface: Option<&mut wl::Surface>,
        surface_x: wl::Fixed,
        surface_y: wl::Fixed,
    ) {
        event_trace!();

        let Some(surface) = surface else {
            return;
        };

        let state = self.pointer.as_mut().expect("no pointer state initialized");
        state.enter_state = Some(PointerEnterState {
            surface: NonNull::from_mut(surface),
            serial,
            implicit_grab_serial: None,
            lock_state: None,
        });
        state.pos = Point::new_logical(surface_x.to_f32(), surface_y.to_f32());

        let surface_state_ptr = surface.user_data().cast::<SurfaceStateUntyped>();
        if surface_state_ptr.is_null() {
            tracing::warn!("entering into unknown surface");
            return;
        }
        let surface_state = unsafe { &*surface_state_ptr };
        tracing::debug!(tag = ?surface_state.tag, "entering into known surface");
        match surface_state.tag {
            SurfaceStateTag::ResizeEdge => {
                let surface_state = unsafe {
                    core::mem::transmute::<&_, &SurfaceState<toplevel::ResizeEdgeSurfaceData>>(
                        surface_state,
                    )
                };
                if let Some(ref c) = state.cursor {
                    c.set_shape(serial, surface_state.data.cursor_shape())
                        .expect("cursor.set_shape")
                }
            }
            SurfaceStateTag::ToplevelWindow => {
                self.event_dispatcher.dispatch(Event::PointerMove {
                    pointer_id: PointerID(pointer),
                    window: toplevel::Handle::from_mut(surface),
                    client_pos: state.pos,
                });
            }
            SurfaceStateTag::FlyoutSurface => {
                self.event_dispatcher.dispatch(Event::MenuPointerMove {
                    pointer_id: PointerID(pointer),
                    target: flyout_surface::Handle(NonNull::from_mut(surface)),
                    client_pos: state.pos,
                });
            }
        }
    }

    #[tracing::instrument(name = "pointer::leave", skip(self, pointer, surface))]
    fn leave(&mut self, pointer: &mut wl::Pointer, serial: u32, surface: Option<&mut wl::Surface>) {
        event_trace!();

        let state = self.pointer.as_mut().expect("no pointer state initialized");

        if let Some(surface) = surface {
            let surface_state_ptr = surface.user_data().cast::<SurfaceStateUntyped>();
            if !surface_state_ptr.is_null() {
                let surface_state = unsafe { &*surface_state_ptr };
                match surface_state.tag {
                    SurfaceStateTag::ToplevelWindow => {
                        self.event_dispatcher.dispatch(Event::PointerLeaveWindow {
                            pointer_id: PointerID(pointer),
                            window: toplevel::Handle::from_mut(surface),
                        });
                    }
                    SurfaceStateTag::FlyoutSurface => {
                        self.event_dispatcher.dispatch(Event::MenuPointerLeave {
                            pointer_id: PointerID(pointer),
                            target: flyout_surface::Handle(NonNull::from_mut(surface)),
                        });
                    }
                    _ => (),
                }
            }
        }

        state.enter_state = None;
    }

    #[tracing::instrument(name = "pointer::motion", skip(self, pointer), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
    fn motion(
        &mut self,
        pointer: &mut wl::Pointer,
        time: u32,
        surface_x: wl::Fixed,
        surface_y: wl::Fixed,
    ) {
        event_trace!();

        let state = self.pointer.as_mut().expect("no pointer state initialized");
        let Some(ref enter_state) = state.enter_state else {
            return;
        };

        state.pos = Point::new_logical(surface_x.to_f32(), surface_y.to_f32());
        let surface_state_ptr = unsafe {
            enter_state
                .surface
                .as_ref()
                .user_data()
                .cast::<SurfaceStateUntyped>()
        };
        if surface_state_ptr.is_null() {
            return;
        }
        let surface_state = unsafe { &*surface_state_ptr };
        match surface_state.tag {
            SurfaceStateTag::ResizeEdge => {}
            SurfaceStateTag::ToplevelWindow => {
                self.event_dispatcher.dispatch(Event::PointerMove {
                    pointer_id: PointerID(pointer),
                    window: toplevel::Handle(enter_state.surface),
                    client_pos: state.pos,
                });
            }
            SurfaceStateTag::FlyoutSurface => {
                self.event_dispatcher.dispatch(Event::MenuPointerMove {
                    pointer_id: PointerID(pointer),
                    target: flyout_surface::Handle(enter_state.surface),
                    client_pos: state.pos,
                });
            }
        }
    }

    #[tracing::instrument(name = "pointer::button", skip(self, pointer), fields(state = state as u32))]
    fn button(
        &mut self,
        pointer: &mut wl::Pointer,
        serial: u32,
        time: u32,
        button: u32,
        state: wl::PointerButtonState,
    ) {
        event_trace!();

        let pointer_state = self.pointer.as_mut().expect("no pointer state initialized");
        let Some(ref mut enter_state) = pointer_state.enter_state else {
            return;
        };

        if state == wl::PointerButtonState::Pressed {
            enter_state.implicit_grab_serial = Some(serial);
            let surface_state = unsafe {
                &*enter_state
                    .surface
                    .as_ref()
                    .user_data()
                    .cast::<SurfaceStateUntyped>()
            };
            match surface_state.tag {
                SurfaceStateTag::ResizeEdge => {
                    let surface_state = unsafe {
                        core::mem::transmute::<&_, &SurfaceState<toplevel::ResizeEdgeSurfaceData>>(
                            surface_state,
                        )
                    };
                    surface_state
                        .data
                        .perform_resize(unsafe { &*pointer_state.seat_ptr }, serial);
                }
                SurfaceStateTag::ToplevelWindow => {
                    self.event_dispatcher.dispatch(Event::PointerDown {
                        window: toplevel::Handle(enter_state.surface),
                        pointer_id: PointerID(pointer),
                        button: if button == linux_input::Key::MouseLeft as u32 {
                            PointerButton::Primary
                        } else {
                            PointerButton::Secondary
                        },
                        event_id: PointerEventID {
                            serial,
                            seat_ptr: pointer_state.seat_ptr,
                        },
                    });
                }
                SurfaceStateTag::FlyoutSurface => {
                    self.event_dispatcher.dispatch(Event::MenuPointerDown {
                        pointer_id: PointerID(pointer),
                        target: flyout_surface::Handle(enter_state.surface),
                        button: if button == linux_input::Key::MouseLeft as u32 {
                            PointerButton::Primary
                        } else {
                            PointerButton::Secondary
                        },
                        event_id: PointerEventID {
                            serial,
                            seat_ptr: pointer_state.seat_ptr,
                        },
                    });
                }
            }
        } else if state == wl::PointerButtonState::Released {
            enter_state.implicit_grab_serial = None;
            let surface_state = unsafe {
                &*enter_state
                    .surface
                    .as_ref()
                    .user_data()
                    .cast::<SurfaceStateUntyped>()
            };
            match surface_state.tag {
                SurfaceStateTag::ResizeEdge => (/* no pointer up event for resize edge */),
                SurfaceStateTag::ToplevelWindow => {
                    self.event_dispatcher.dispatch(Event::PointerUp {
                        window: toplevel::Handle(enter_state.surface),
                        pointer_id: PointerID(pointer),
                        button: if button == linux_input::Key::MouseLeft as u32 {
                            PointerButton::Primary
                        } else {
                            PointerButton::Secondary
                        },
                    });
                }
                SurfaceStateTag::FlyoutSurface => {
                    self.event_dispatcher.dispatch(Event::MenuPointerUp {
                        target: flyout_surface::Handle(enter_state.surface),
                        pointer_id: PointerID(pointer),
                        button: if button == linux_input::Key::MouseLeft as u32 {
                            PointerButton::Primary
                        } else {
                            PointerButton::Secondary
                        },
                    });
                }
            }
        }
    }

    #[tracing::instrument(name = "pointer::axis", skip(self, _pointer))]
    fn axis(&mut self, _pointer: &mut wl::Pointer, time: u32, axis: u32, value: wl::Fixed) {
        event_trace!();
    }

    #[tracing::instrument(name = "pointer::frame", skip(self, _pointer))]
    fn frame(&mut self, _pointer: &mut wl::Pointer) {
        event_trace!();
    }

    #[tracing::instrument(name = "pointer::axis_source", skip(self, _pointer))]
    fn axis_source(&mut self, _pointer: &mut wl::Pointer, axis_source: u32) {
        event_trace!();
    }

    #[tracing::instrument(name = "pointer::axis_stop", skip(self, _pointer))]
    fn axis_stop(&mut self, _pointer: &mut wl::Pointer, time: u32, axis: u32) {
        event_trace!();
    }

    #[tracing::instrument(name = "pointer::axis_discrete", skip(self, _pointer))]
    fn axis_discrete(&mut self, _pointer: &mut wl::Pointer, axis: u32, discrete: i32) {
        event_trace!();
    }

    #[tracing::instrument(name = "pointer::axis_value120", skip(self, _pointer))]
    fn axis_value120(&mut self, _pointer: &mut wl::Pointer, axis: u32, value120: i32) {
        event_trace!();

        // TODO: 必要なら他のaxisイベントシーケンスも処理する
        self.event_dispatcher.dispatch(Event::ScrollWheel {
            // 逆でくる
            amount: -value120 as f32 / 120.0,
            key_modifier: self
                .keyboard
                .as_ref()
                .map_or_else(ModifierKey::empty, |x| x.build_modifier()),
        });
    }

    #[tracing::instrument(name = "pointer::axis_relative_direction", skip(self, _pointer))]
    fn axis_relative_direction(&mut self, _pointer: &mut wl::Pointer, axis: u32, direction: u32) {
        event_trace!();
    }
}
impl wl::ZwpLockedPointerV1EventListener for GlobalMessaging {
    #[tracing::instrument(name = "locked_pointer::locked", skip(self, _sender))]
    fn locked(&mut self, _sender: &mut wl::ZwpLockedPointerV1) {
        event_trace!();
    }

    #[tracing::instrument(name = "locked_pointer::unlocked", skip(self, _sender))]
    fn unlocked(&mut self, _sender: &mut wl::ZwpLockedPointerV1) {
        event_trace!();
    }
}
impl wl::ZwpRelativePointerV1EventListener for GlobalMessaging {
    #[tracing::instrument(name = "pointer::relative_motion", skip(self, _sender))]
    fn relative_motion(
        &mut self,
        _sender: &mut wl::ZwpRelativePointerV1,
        utime_hi: u32,
        utime_lo: u32,
        dx: wl::Fixed,
        dy: wl::Fixed,
        dx_unaccel: wl::Fixed,
        dy_unaccel: wl::Fixed,
    ) {
        event_trace!();

        let state = self.pointer.as_mut().expect("no pointer state initialized");
        let Some(ref enter_state) = state.enter_state else {
            return;
        };

        self.event_dispatcher.dispatch(Event::PointerMoveRelative {
            pointer_id: PointerID(state._wl_object.as_ptr()),
            window: toplevel::Handle(enter_state.surface),
            relative: Point::new_logical(dx.to_f32(), dy.to_f32()),
        });
    }
}
impl wl::DataDeviceEventListener for GlobalMessaging {
    #[tracing::instrument(name = "data_device::data_offer", skip(self, _sender, id))]
    fn data_offer(&mut self, _sender: &mut wl::DataDevice, mut id: wl::Owned<wl::DataOffer>) {
        event_trace!();

        id.set_listener(self)
            .into_result()
            .expect("data_offer.set_listener");
        self.data_device
            .as_mut()
            .expect("no data device")
            .active_offer = Some(DataDeviceActiveOfferState {
            object: id,
            entering_surface: None,
            client_pos: Point::new_logical(0.0, 0.0),
            mime_types: HashSet::new(),
            source_actions: wl::DataDeviceManagerDndAction::empty(),
        });
    }

    #[tracing::instrument(name = "data_device::enter", skip(self, _sender, surface, _id))]
    fn enter(
        &mut self,
        _sender: &mut wl::DataDevice,
        serial: u32,
        surface: &wl::Surface,
        x: wl::Fixed,
        y: wl::Fixed,
        _id: Option<&wl::DataOffer>,
    ) {
        event_trace!();

        let surface_state_ptr = surface.user_data().cast::<SurfaceStateUntyped>();
        if surface_state_ptr.is_null() {
            self.drag_preview_popover.hide_surface();
            self.drag_preview_popover.show_dnd_icon();
            self.data_device
                .as_mut()
                .expect("no data device")
                .active_offer
                .take();
            return;
        }
        let surface_state = unsafe { &*surface_state_ptr };
        if surface_state.tag != SurfaceStateTag::ToplevelWindow {
            self.drag_preview_popover.hide_surface();
            self.drag_preview_popover.show_dnd_icon();
            self.data_device
                .as_mut()
                .expect("no data device")
                .active_offer
                .take();
            return;
        }

        self.drag_preview_popover.hide_dnd_icon();
        self.drag_preview_popover.show_surface(
            unsafe { &*self.global_interfaces },
            &unsafe {
                core::mem::transmute::<_, &SurfaceState<toplevel::InstanceState>>(surface_state)
            }
            .data
            .xdg_surface,
        );
        let Some(ref mut active_offer) = self
            .data_device
            .as_mut()
            .expect("no data device")
            .active_offer
        else {
            // no active offer
            return;
        };

        let accepting_mime_type;
        if active_offer
            .mime_types
            .contains(c"application/x-pme-dock-content")
        {
            tracing::debug!("offered(accepting): dock content");
            accepting_mime_type = Some(c"application/x-pme-dock-content");
        } else {
            accepting_mime_type = None;
        }
        active_offer
            .object
            .accept(0, accepting_mime_type)
            .expect("data_offer.accept");
        active_offer.entering_surface = Some(NonNull::from_ref(surface));
        active_offer.client_pos = Point::new_logical(x.to_f32(), y.to_f32());
    }

    #[tracing::instrument(name = "data_device::leave", skip(self, _sender))]
    fn leave(&mut self, _sender: &mut wl::DataDevice) {
        event_trace!();

        self.data_device
            .as_mut()
            .expect("no data device")
            .active_offer
            .take();
    }

    #[tracing::instrument(name = "data_device::motion", skip(self, _sender))]
    fn motion(&mut self, _sender: &mut wl::DataDevice, time: u32, x: wl::Fixed, y: wl::Fixed) {
        event_trace!();

        let Some(ref mut active_offer) = self
            .data_device
            .as_mut()
            .expect("no data device")
            .active_offer
        else {
            // no active offer
            return;
        };
        active_offer.client_pos = Point::new_logical(x.to_f32(), y.to_f32());
        self.event_dispatcher.dispatch(Event::DockMovePreview {
            dest_window: toplevel::Handle(
                active_offer.entering_surface.expect("no entering surface?"),
            ),
            client_pos_in_dest: Point::new_logical(x.to_f32(), y.to_f32()),
        });
    }

    #[tracing::instrument(name = "data_device::drop", skip(self, _sender))]
    fn drop(&mut self, _sender: &mut wl::DataDevice) {
        event_trace!();

        let Some(active_offer) = self
            .data_device
            .as_mut()
            .expect("no data device")
            .active_offer
            .take()
        else {
            // no active offers
            return;
        };
        let mut pipe_fds = [0; 2];
        let r = unsafe { libc::pipe(pipe_fds.as_mut_ptr()) };
        if r < 0 {
            panic!("libc.pipe: {}", std::io::Error::last_os_error());
        }
        active_offer
            .object
            .receive(c"application/x-pme-dock-content", &pipe_fds[1])
            .expect("active_offer.receive");
        active_offer.object.finish().expect("active_offer.finish");

        self.event_dispatcher.dispatch(Event::DockConfirm {
            pointer: PointerID(
                self.pointer
                    .as_ref()
                    .expect("no pointer")
                    ._wl_object
                    .as_ptr(),
            ),
            destination_window: toplevel::Handle(
                active_offer.entering_surface.expect("no entering surface?"),
            ),
            client_pos_in_dest: active_offer.client_pos,
        })
    }

    #[tracing::instrument(name = "data_device::selection", skip(self, _sender, _id))]
    fn selection(&mut self, _sender: &mut wl::DataDevice, _id: Option<&wl::DataOffer>) {
        event_trace!();
    }
}
impl wl::DataSourceEventListener for GlobalMessaging {
    #[tracing::instrument(name = "data_source::target", skip(self, _sender))]
    fn target(&mut self, _sender: &mut wl::DataSource, mime_type: Option<&core::ffi::CStr>) {
        event_trace!();
    }

    #[tracing::instrument(name = "data_source::send", skip(self, _sender))]
    fn send(
        &mut self,
        _sender: &mut wl::DataSource,
        mime_type: &core::ffi::CStr,
        fd: std::os::fd::RawFd,
    ) {
        event_trace!();

        let r = unsafe { libc::close(fd) };
        if r < 0 {
            panic!("libc.close: {}", std::io::Error::last_os_error());
        }
    }

    #[tracing::instrument(name = "data_source::cancelled", skip(self, sender))]
    fn cancelled(&mut self, sender: &mut wl::DataSource) {
        event_trace!();

        let Some(state) = self
            .data_device
            .as_mut()
            .expect("no data device")
            .pane_drag_state
            .take_if(|x| x.source.ref_eq(sender))
        else {
            return;
        };

        self.event_dispatcher.dispatch(Event::DockConfirm {
            pointer: PointerID(
                self.pointer
                    .as_ref()
                    .expect("no pointer")
                    ._wl_object
                    .as_ptr(),
            ),
            destination_window: state.initiator,
            client_pos_in_dest: Point::new_logical(-1.0, -1.0),
        })
    }

    #[tracing::instrument(name = "data_source::dnd_drop_performed", skip(self, _sender))]
    fn dnd_drop_performed(&mut self, _sender: &mut wl::DataSource) {
        event_trace!();
    }

    #[tracing::instrument(name = "data_source::dnd_finished", skip(self, sender))]
    fn dnd_finished(&mut self, sender: &mut wl::DataSource) {
        event_trace!();

        self.data_device
            .as_mut()
            .expect("no data device")
            .pane_drag_state
            .take_if(|x| x.source.ref_eq(sender));
    }

    #[tracing::instrument(name = "data_source::action", skip(self, _sender))]
    fn action(&mut self, _sender: &mut wl::DataSource, dnd_action: wl::DataDeviceManagerDndAction) {
        event_trace!();
    }
}
impl wl::DataOfferEventListener for GlobalMessaging {
    #[tracing::instrument(name = "data_offer::offer", skip(self, sender))]
    fn offer(&mut self, sender: &mut wl::DataOffer, mime_type: &core::ffi::CStr) {
        event_trace!();

        let active_offer = self
            .data_device
            .as_mut()
            .expect("no data device")
            .active_offer
            .as_mut()
            .expect("no offer active");
        assert!(active_offer.object.ref_eq(sender), "another offer request");
        active_offer.mime_types.insert(mime_type.to_owned());
    }

    #[tracing::instrument(name = "data_offer::source_actions", skip(self, sender))]
    fn source_actions(
        &mut self,
        sender: &mut wl::DataOffer,
        source_actions: wl::DataDeviceManagerDndAction,
    ) {
        event_trace!();

        let active_offer = self
            .data_device
            .as_mut()
            .expect("no data device")
            .active_offer
            .as_mut()
            .expect("no offer active");
        assert!(active_offer.object.ref_eq(sender), "another offer request");
        active_offer.source_actions |= source_actions;
    }

    #[tracing::instrument(name = "data_offer::action", skip(self, _sender))]
    fn action(&mut self, _sender: &mut wl::DataOffer, dnd_action: wl::DataDeviceManagerDndAction) {
        event_trace!();
    }
}
impl wl::KeyboardEventListener for GlobalMessaging {
    #[tracing::instrument(name = "keyboard::keymap", skip(self, _sender))]
    fn keymap(
        &mut self,
        _sender: &mut wl::Keyboard,
        format: wl::KeyboardKeymapFormat,
        fd: i32,
        size: u32,
    ) {
        event_trace!();

        let state = self.keyboard.as_mut().expect("keyboard_state.uninit");
        if format != wl::KeyboardKeymapFormat::XkbV1 {
            unimplemented!("unknown keymap format: {format:?}");
        }

        let mapped = crate::utils::platform::unix::MappedMemory::new(
            None,
            size as _,
            libc::PROT_READ,
            libc::MAP_PRIVATE,
            &fd,
            0,
        )
        .expect("keyboard.keymap.mmap");
        let content = unsafe {
            core::ffi::CStr::from_bytes_with_nul(core::slice::from_raw_parts(
                mapped.as_ptr().cast::<u8>(),
                size as _,
            ))
            .expect("invalid content")
            .to_str()
            .expect("invalid content")
        };
        let keymap = xkbcommon::Keymap::from_buffer(
            &self.xkb_context,
            unsafe { core::slice::from_raw_parts(content.as_ptr(), size as _) },
            xkbcommon::KeymapFormat::TextV1,
            xkbcommon::KeymapCompileFlags::NO_FLAGS,
        )
        .expect("xkb_keymap.create");
        let xkb_state = xkbcommon::State::new(&keymap).expect("xkb_state.create");

        state.xkb_shift_mod_index = keymap.mod_index(xkbcommon::ffi::XKB_MOD_NAME_SHIFT);
        state.xkb_alt_mod_index = keymap.mod_index(xkbcommon::ffi::XKB_MOD_NAME_ALT);
        state.xkb_ctrl_mod_index = keymap.mod_index(xkbcommon::ffi::XKB_MOD_NAME_CTRL);
        state.xkb_super_mod_index = keymap.mod_index(xkbcommon::ffi::XKB_VMOD_NAME_SUPER);
        state.xkb_keymap = Some(keymap);
        state.xkb_state = Some(xkb_state);
    }

    #[tracing::instrument(name = "keyboard::enter", skip(self, _sender, surface))]
    fn enter(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        surface: &mut wl::Surface,
        keys: &[u32],
    ) {
        event_trace!();

        let state = self.keyboard.as_mut().expect("no keyboard");
        state.enter_state = Some(KeyboardEnterState {
            surface: NonNull::from_mut(surface),
        });
        self.event_dispatcher.dispatch(Event::WindowFocusChanged {
            window: toplevel::Handle::from_mut(surface),
            focused: true,
        });
    }

    #[tracing::instrument(name = "keyboard::leave", skip(self, _sender, surface))]
    fn leave(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        surface: Option<&mut wl::Surface>,
    ) {
        event_trace!();

        let state = self.keyboard.as_mut().expect("no keyboard");
        state.enter_state = None;
        if let Some(s) = surface {
            self.event_dispatcher.dispatch(Event::WindowFocusChanged {
                window: toplevel::Handle::from_mut(s),
                focused: false,
            });
        }
    }

    #[tracing::instrument(name = "keyboard::key", skip(self, _sender))]
    fn key(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        time: u32,
        key: u32,
        state: wl::KeyboardKeyState,
    ) {
        event_trace!();

        let k_state = self.keyboard.as_ref().expect("keyboard_state.uninit");
        let Some(ref enter_state) = k_state.enter_state else {
            return;
        };

        let modifier = k_state.build_modifier();
        let ch = if let Some(ref x) = k_state.xkb_state {
            // evdevのスキャンコードでくるので、xkbのスキャンコードにする(8を足せばいいらしい: https://wayland-book.com/seat/keyboard.html)
            char::from_u32(x.key_get_utf32(key + 8)).unwrap_or('\0')
        } else {
            '\0'
        };
        tracing::trace!(?ch, ?modifier, "keyinput");

        let code = match ch {
            '\0' => match key {
                k if k == Key::LeftControl as u32 => KeyInputCode::LeftControl,
                k if k == Key::RightControl as u32 => KeyInputCode::RightControl,
                k if k == Key::LeftShift as u32 => KeyInputCode::LeftShift,
                k if k == Key::RightShift as u32 => KeyInputCode::RightShift,
                k if k == Key::LeftAlt as u32 => KeyInputCode::LeftAlt,
                k if k == Key::RightAlt as u32 => KeyInputCode::RightAlt,
                k if k == Key::LeftMeta as u32 => KeyInputCode::LeftSuper,
                k if k == Key::RightMeta as u32 => KeyInputCode::RightSuper,
                k if k == Key::Left as u32 => KeyInputCode::LeftArrow,
                k if k == Key::Right as u32 => KeyInputCode::RightArrow,
                k if k == Key::Up as u32 => KeyInputCode::UpArrow,
                k if k == Key::Down as u32 => KeyInputCode::DownArrow,
                k if k == Key::Home as u32 => KeyInputCode::Home,
                k if k == Key::End as u32 => KeyInputCode::End,
                k if k == Key::PageUp as u32 => KeyInputCode::PageUp,
                k if k == Key::PageDown as u32 => KeyInputCode::PageDown,
                k if k == Key::Insert as u32 => KeyInputCode::Insert,
                k if k == Key::Tab as u32 => KeyInputCode::Tab,
                k if k == Key::Esc as u32 => KeyInputCode::Esc,
                _ => KeyInputCode::UnknownNativeCode(key),
            },
            // 文字でくるキーの一部
            '\x1b' => KeyInputCode::Esc,
            '\r' => KeyInputCode::Enter,
            '\x08' => KeyInputCode::Backspace,
            '\x7f' => KeyInputCode::Delete,
            c => KeyInputCode::Character(c),
        };
        match state {
            wl::KeyboardKeyState::Pressed | wl::KeyboardKeyState::Repeated => {
                self.event_dispatcher.dispatch(Event::KeyDown {
                    window: toplevel::Handle(enter_state.surface),
                    modifier,
                    code,
                });
            }
            wl::KeyboardKeyState::Released => {
                self.event_dispatcher.dispatch(Event::KeyUp {
                    window: toplevel::Handle(enter_state.surface),
                    modifier,
                    code,
                });
            }
            _ => unreachable!(),
        }
    }

    #[tracing::instrument(name = "keyboard::modifiers", skip(self, _sender))]
    fn modifiers(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        mods_depressed: u32,
        mods_latched: u32,
        mods_locked: u32,
        group: u32,
    ) {
        event_trace!();

        let state = self.keyboard.as_mut().expect("keyboard_state.uninit");
        if let Some(ref mut x) = state.xkb_state {
            x.update_mask(
                mods_depressed,
                mods_latched,
                mods_locked,
                group,
                group,
                group,
            );
        }
    }

    #[tracing::instrument(name = "keyboard::repeat_info", skip(self, _sender))]
    fn repeat_info(&mut self, _sender: &mut wl::Keyboard, rate: i32, delay: i32) {
        event_trace!();
    }
}
impl wl::ZwpTextInputV3EventListener for GlobalMessaging {
    #[tracing::instrument(name = "text_input_v3::enter", skip(self, sender, _surface))]
    fn enter(&mut self, sender: &mut wl::ZwpTextInputV3, _surface: Option<&mut wl::Surface>) {
        event_trace!();

        sender.enable().expect("text_input.enable");
        sender.commit().expect("text_input.commit");
    }

    #[tracing::instrument(name = "text_input_v3::leave", skip(self, sender, _surface))]
    fn leave(&mut self, sender: &mut wl::ZwpTextInputV3, _surface: Option<&mut wl::Surface>) {
        event_trace!();

        sender.disable().expect("text_input.disable");
        sender.commit().expect("text_input.commit");
    }

    #[tracing::instrument(name = "text_input_v3::preedit_string", skip(self, _sender))]
    fn preedit_string(
        &mut self,
        _sender: &mut wl::ZwpTextInputV3,
        text: Option<&core::ffi::CStr>,
        cursor_begin: i32,
        cursor_end: i32,
    ) {
        event_trace!();

        self.ime_pending_state.preedit_text = Some(
            text.map(|t| t.to_string_lossy().into_owned())
                .unwrap_or_default(),
        );
    }

    #[tracing::instrument(name = "text_input_v3::commit_string", skip(self, _sender))]
    fn commit_string(&mut self, _sender: &mut wl::ZwpTextInputV3, text: Option<&core::ffi::CStr>) {
        event_trace!();

        self.ime_pending_state.committed_text = Some(
            text.map(|t| t.to_string_lossy().into_owned())
                .unwrap_or_default(),
        );
    }

    #[tracing::instrument(name = "text_input_v3::delete_surrounding_text", skip(self, _sender))]
    fn delete_surrounding_text(
        &mut self,
        _sender: &mut wl::ZwpTextInputV3,
        before_length: u32,
        after_length: u32,
    ) {
        event_trace!();
    }

    #[tracing::instrument(name = "text_input_v3::done", skip(self, _sender))]
    fn done(&mut self, _sender: &mut wl::ZwpTextInputV3, serial: u32) {
        event_trace!();

        let k_state = self.keyboard.as_ref().expect("keyboard.uninit");
        let Some(ref k_enter_state) = k_state.enter_state else {
            return;
        };

        let committed_string = self.ime_pending_state.committed_text.take();
        let preedit_string = self.ime_pending_state.preedit_text.take();
        if committed_string.is_none() && preedit_string.is_none() {
            // no changes occured
            return;
        }

        self.event_dispatcher.dispatch(Event::IMEStateChanges {
            window: toplevel::Handle(k_enter_state.surface),
            committed_string,
            preedit_string,
        });
    }
}
impl wl::ZwlrLayerSurfaceV1EventListener for GlobalMessaging {
    #[tracing::instrument(skip(self, sender))]
    fn configure(
        &mut self,
        sender: &mut wl::ZwlrLayerSurfaceV1,
        serial: u32,
        width: u32,
        height: u32,
    ) {
        event_trace!();

        sender
            .ack_configure(serial)
            .expect("layer_surface.ack_configure");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn closed(&mut self, _sender: &mut wl::ZwlrLayerSurfaceV1) {
        event_trace!();
    }
}

struct GlobalInterfaces {
    outputs: Vec<(wl::Owned<wl::Output>, Box<OutputProperties>)>,
    compositor: wl::Owned<wl::Compositor>,
    subcompositor: wl::Owned<wl::Subcompositor>,
    xdg_wm_base: wl::Owned<wl::XdgWmBase>,
    seat: wl::Owned<wl::Seat>,
    data_device_manager: wl::Owned<wl::DataDeviceManager>,
    shm: wl::Owned<wl::Shm>,
    viewporter: wl::Owned<wl::WpViewporter>,
    text_input_manager: wl::Owned<wl::ZwpTextInputManagerV3>,
    // optional requirements
    single_pixel_buffer_manager: Option<wl::Owned<wl::WpSinglePixelBufferManagerV1>>,
    kde_blur_manager: Option<wl::Owned<wl::OrgKdeKwinBlurManager>>,
    kde_appmenu_manager: Option<wl::Owned<wl::OrgKdeKwinAppmenuManager>>,
    zxdg_decoration_manager: Option<wl::Owned<wl::ZxdgDecorationManagerV1>>,
    cursor_shape_manager: Option<wl::Owned<wl::WpCursorShapeManagerV1>>,
    fractional_scale_manager: Option<wl::Owned<wl::WpFractionalScaleManagerV1>>,
    alpha_modifier: Option<wl::Owned<wl::WpAlphaModifierV1>>,
    pointer_constraints: Option<wl::Owned<wl::ZwpPointerConstraintsV1>>,
    relative_pointer_manager: Option<wl::Owned<wl::ZwpRelativePointerManagerV1>>,
    kde_plasma_shell: Option<wl::Owned<wl::OrgKdePlasmaShell>>,
    // flags
    is_hyprland: bool,
}
impl GlobalInterfaces {
    fn collect_sync(display: &wl::Display) -> std::io::Result<Self> {
        let mut wl_registry = display.get_registry()?;
        let mut rl = RegistryListener::default();
        wl_registry
            .set_listener(&mut rl)
            .into_result()
            .expect("wl_registry.set_listener");
        display.roundtrip()?;

        Ok(Self {
            outputs: rl.outputs,
            compositor: rl.compositor.expect("no compositor"),
            subcompositor: rl.subcompositor.expect("no subcompositor"),
            xdg_wm_base: rl.xdg_wm_base.expect("no xdg-shell"),
            seat: rl.seat.expect("no seat"),
            data_device_manager: rl.data_device_manager.expect("no data-device-manager"),
            shm: rl.shm.expect("no shm"),
            viewporter: rl.viewporter.expect("no viewporter"),
            text_input_manager: rl.text_input_manager.expect("no text-input"),
            single_pixel_buffer_manager: rl.single_pixel_buffer_manager,
            kde_blur_manager: rl.kde_blur_manager,
            kde_appmenu_manager: rl.kde_appmenu_manager,
            zxdg_decoration_manager: rl.zxdg_decoration_manager,
            cursor_shape_manager: rl.cursor_shape_manager,
            fractional_scale_manager: rl.fractional_scale_manager,
            alpha_modifier: rl.alpha_modifier,
            pointer_constraints: rl.pointer_constraints,
            relative_pointer_manager: rl.relative_pointer_manager,
            kde_plasma_shell: rl.kde_plasma_shell,
            is_hyprland: rl.is_hyprland,
        })
    }

    pub fn bind_global_messaging(&mut self, mut g: core::pin::Pin<&mut GlobalMessaging>) {
        self.xdg_wm_base
            .set_listener(unsafe { g.as_mut().get_unchecked_mut() })
            .into_result()
            .expect("xdg_wm_base set_listener");
        self.seat
            .set_listener(unsafe { g.as_mut().get_unchecked_mut() })
            .into_result()
            .expect("seat set_listener");
    }
}

#[derive(Default)]
struct RegistryListener {
    compositor: Option<wl::Owned<wl::Compositor>>,
    subcompositor: Option<wl::Owned<wl::Subcompositor>>,
    outputs: Vec<(wl::Owned<wl::Output>, Box<OutputProperties>)>,
    xdg_wm_base: Option<wl::Owned<wl::XdgWmBase>>,
    seat: Option<wl::Owned<wl::Seat>>,
    data_device_manager: Option<wl::Owned<wl::DataDeviceManager>>,
    shm: Option<wl::Owned<wl::Shm>>,
    viewporter: Option<wl::Owned<wl::WpViewporter>>,
    text_input_manager: Option<wl::Owned<wl::ZwpTextInputManagerV3>>,
    single_pixel_buffer_manager: Option<wl::Owned<wl::WpSinglePixelBufferManagerV1>>,
    kde_blur_manager: Option<wl::Owned<wl::OrgKdeKwinBlurManager>>,
    kde_appmenu_manager: Option<wl::Owned<wl::OrgKdeKwinAppmenuManager>>,
    zxdg_decoration_manager: Option<wl::Owned<wl::ZxdgDecorationManagerV1>>,
    cursor_shape_manager: Option<wl::Owned<wl::WpCursorShapeManagerV1>>,
    fractional_scale_manager: Option<wl::Owned<wl::WpFractionalScaleManagerV1>>,
    alpha_modifier: Option<wl::Owned<wl::WpAlphaModifierV1>>,
    pointer_constraints: Option<wl::Owned<wl::ZwpPointerConstraintsV1>>,
    relative_pointer_manager: Option<wl::Owned<wl::ZwpRelativePointerManagerV1>>,
    kde_plasma_shell: Option<wl::Owned<wl::OrgKdePlasmaShell>>,
    is_hyprland: bool,
}
impl wl::RegistryListener for RegistryListener {
    fn global(
        &mut self,
        registry: &mut wl::Registry,
        name: u32,
        interface: &core::ffi::CStr,
        version: u32,
    ) {
        tracing::info!(target: "wl::diag::global_interface", name, ?interface, version);

        if interface == c"hyprland_surface_manager_v1" {
            // 暫定的にこのインターフェイスがあった場合はHyprland扱いする
            self.is_hyprland = true;
        }

        if interface == c"wl_compositor" {
            self.compositor = Some(registry.bind(name, version).expect("bind compositor"));
            return;
        }
        if interface == c"wl_subcompositor" {
            self.subcompositor = Some(registry.bind(name, version).expect("bind subcompositor"));
            return;
        }
        if interface == c"wl_output" {
            let mut o = registry
                .bind::<wl::Output>(name, version)
                .expect("bind output");
            let mut properties_store = Box::new(OutputProperties { x: 0, y: 0 });
            o.set_listener_impl_only(OUTPUT_EVENT_LISTENER_IMPL)
                .into_result()
                .expect("output.set_listener_impl_only");
            o.set_user_data(core::ptr::from_mut(properties_store.as_mut()).cast());
            self.outputs.push((o, properties_store));
            return;
        }
        if interface == c"xdg_wm_base" {
            self.xdg_wm_base = Some(registry.bind(name, version).expect("bind xdg_wm_base"));
            return;
        }
        if interface == c"wl_seat" {
            assert!(self.seat.is_none(), "multiple seat?");
            self.seat = Some(registry.bind(name, version).expect("bind seat"));
            return;
        }
        if interface == c"wl_data_device_manager" {
            self.data_device_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind data_device_manager"),
            );
            return;
        }
        if interface == c"wl_shm" {
            self.shm = Some(registry.bind(name, version).expect("bind shm"));
            return;
        }
        if interface == c"wp_viewporter" {
            self.viewporter = Some(registry.bind(name, version).expect("bind viewporter"));
            return;
        }
        if interface == c"wp_single_pixel_buffer_manager_v1" {
            self.single_pixel_buffer_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind single_pixel_buffer_manager"),
            );
            return;
        }
        if interface == c"org_kde_kwin_blur_manager" {
            self.kde_blur_manager =
                Some(registry.bind(name, version).expect("bind kde_blur_manager"));
            return;
        }
        if interface == c"org_kde_kwin_appmenu_manager" {
            self.kde_appmenu_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind kde_appmenu_manager"),
            );
            return;
        }
        if interface == c"zxdg_decoration_manager_v1" {
            self.zxdg_decoration_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind zxdg_decoration_manager"),
            );
            return;
        }
        if interface == c"zwp_text_input_manager_v3" {
            self.text_input_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind text_input_manager"),
            );
            return;
        }
        if interface == c"wp_cursor_shape_manager_v1" {
            self.cursor_shape_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind cursor_shape_manager"),
            );
            return;
        }
        if interface == c"wp_fractional_scale_manager_v1" {
            self.fractional_scale_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind fractional_scale_manager"),
            );
            return;
        }
        if interface == c"wp_alpha_modifier_v1" {
            self.alpha_modifier = Some(registry.bind(name, version).expect("bind alpha_modifier"));
            return;
        }
        if interface == c"zwp_pointer_constraints_v1" {
            self.pointer_constraints = Some(
                registry
                    .bind(name, version)
                    .expect("bind pointer_constraints"),
            );
            return;
        }
        if interface == c"zwp_relative_pointer_manager_v1" {
            self.relative_pointer_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind relative_pointer_manager"),
            );
            return;
        }
        if interface == c"org_kde_plasma_shell" {
            self.kde_plasma_shell =
                Some(registry.bind(name, version).expect("bind kde_plasma_shell"));
            return;
        }
    }

    fn global_remove(&mut self, _registry: &mut wl::Registry, name: u32) {
        tracing::info!(target: "wl::diag", name, "wl interface remove");
    }
}

struct OutputProperties {
    x: i32,
    y: i32,
}

extern "C" fn output_event_geometry(
    context: *mut core::ffi::c_void,
    sender: *mut wl::Output,
    x: i32,
    y: i32,
    physical_width: i32,
    physical_height: i32,
    subpixel: i32,
    make: *const core::ffi::c_char,
    model: *const core::ffi::c_char,
    transform: i32,
) {
    if context.is_null() {
        return;
    }

    let store = unsafe { &mut *context.cast::<OutputProperties>() };
    let make = unsafe { core::ffi::CStr::from_ptr(make) };
    let model = unsafe { core::ffi::CStr::from_ptr(model) };

    tracing::debug!(
        x,
        y,
        physical_width,
        physical_height,
        subpixel,
        ?make,
        ?model,
        transform,
        "output geometry {sender:p}"
    );
    store.x = x;
    store.y = y;
}
extern "C" fn output_event_mode(
    context: *mut core::ffi::c_void,
    _sender: *mut wl::Output,
    flags: u32,
    width: i32,
    height: i32,
    refresh: i32,
) {
    if context.is_null() {
        return;
    }

    tracing::debug!(flags, width, height, refresh, "output mode");
}
extern "C" fn output_event_done(context: *mut core::ffi::c_void, sender: *mut wl::Output) {
    if context.is_null() {
        return;
    }

    tracing::debug!("output done {sender:p}");
    unsafe { &mut *sender }.set_user_data(core::ptr::null_mut());
}
extern "C" fn output_event_scale(
    context: *mut core::ffi::c_void,
    _sender: *mut wl::Output,
    scale: i32,
) {
    if context.is_null() {
        return;
    }

    tracing::debug!(scale, "output scale");
}
extern "C" fn output_event_name(
    context: *mut core::ffi::c_void,
    _sender: *mut wl::Output,
    name: *const core::ffi::c_char,
) {
    if context.is_null() {
        return;
    }

    let name = unsafe { core::ffi::CStr::from_ptr(name) };
    tracing::debug!(?name, "output name");
}
extern "C" fn output_event_description(
    context: *mut core::ffi::c_void,
    _sender: *mut wl::Output,
    description: *const core::ffi::c_char,
) {
    if context.is_null() {
        return;
    }

    let description = unsafe { core::ffi::CStr::from_ptr(description) };
    tracing::debug!(?description, "output description");
}
const OUTPUT_EVENT_LISTENER_IMPL: &wl::OutputEventListenerImpl = &wl::OutputEventListenerImpl {
    geometry: output_event_geometry,
    mode: output_event_mode,
    done: output_event_done,
    scale: output_event_scale,
    name: output_event_name,
    description: output_event_description,
};
