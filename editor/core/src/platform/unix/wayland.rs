use core::ptr::NonNull;
use std::os::fd::{AsRawFd, RawFd};

use linux_input::Key;
use peridot_tp_wayland as wl;
use peridot_tp_xkbcommon as xkbcommon;

use crate::{
    Event, LogicFiberEventDispatcher, SyncEvent, SystemLink, WindowType,
    graphics::VulkanDevice,
    input::{
        KeyInputCode, KeyboardFocusTokenRegistry, ModifierKey, PointerInputUnit,
        hittest::{CursorShape, HitTestTreeManager, PointerButton},
    },
    rendering::{RenderMessage, composite::CompositeTree},
    utils::{
        LogicalUnit, Point, Rect, Size,
        platform::unix::{MappedMemory, TemporalSharedMemory, ftruncate},
        rup2,
    },
};

pub mod flyout_surface;
mod toplevel;

pub use self::flyout_surface::Handle as FlyoutSurfaceHandle;
pub use self::toplevel::Handle as ToplevelHandle;

macro_rules! event_trace {
    ($($args:tt)+) => {
        tracing::trace!(target: "wl::event-trace", $($args)+);
    };
    () => {
        $crate::platform::unix::wayland::event_trace!("wayland event");
    }
}
pub(self) use event_trace;

#[derive(Clone, Copy, PartialEq, Eq)]
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

pub const APPMENU_OBJECT_PATH: &core::ffi::CStr = c"/AppMenu";

pub struct DragPreviewPopoverHandle {
    display: *mut wl::Display,
    wl_interfaces: *const GlobalInterfaces,
    root_window: core::cell::Cell<*mut wl::XdgSurface>,
    buf: DragPreviewPopoverBuffer,
    popup: core::cell::UnsafeCell<
        Option<(
            Option<wl::Owned<wl::OrgKdeKwinBlur>>,
            wl::Owned<wl::XdgPopup>,
            wl::Owned<wl::XdgSurface>,
            wl::Owned<wl::WpViewport>,
            wl::Owned<wl::Surface>,
            Box<PopupState>,
        )>,
    >,
}
impl DragPreviewPopoverHandle {
    pub fn new(syslink: &SystemLink) -> Self {
        Self {
            display: &mut unsafe { &mut *syslink.display_server.context }.dp,
            wl_interfaces: &unsafe { &*syslink.display_server.context }.global_interfaces,
            root_window: core::cell::Cell::new(core::ptr::null_mut()),
            buf: unsafe { &*syslink.display_server.static_pixbufs }
                .create_drag_preview_popover_bufs(
                    &unsafe { &*syslink.display_server.context }.global_interfaces,
                ),
            popup: core::cell::UnsafeCell::new(None),
        }
    }

    pub fn bind_parent_window(&self, window: toplevel::Handle) {
        self.root_window
            .set(core::ptr::from_ref(window.xdg_surface()).cast_mut());
    }

    pub fn show(&self, pos: &Point<PointerInputUnit>, size: &Size<LogicalUnit>) {
        let wl_popup_surface = unsafe {
            (*self.wl_interfaces)
                .compositor
                .create_surface()
                .expect("wl_popup_surface.create")
        };
        let mut xdg_popup_surface = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .get_xdg_surface(&wl_popup_surface)
                .expect("xdg_popup_surface.create")
        };

        let positioner = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .create_positioner()
                .expect("pos.create")
        };
        positioner
            .set_size(size.width as _, size.height as _)
            .expect("pos.set_size");
        positioner
            .set_offset(pos.x as _, pos.y as _)
            .expect("pos.set_offset");
        positioner
            .set_anchor(wl::XdgPositionerAnchor::TopLeft)
            .expect("pos.set_anchor");
        positioner
            .set_anchor_rect(0, 0, 1, 1)
            .expect("pos.set_anchor_rect");
        positioner
            .set_gravity(wl::XdgPositionerGravity::BottomRight)
            .expect("pos.set_gravity");
        positioner
            .set_constraint_adjustment(wl::XdgPositionerConstraintAdjustment::None)
            .expect("pos.set_constraint_adjustment");
        let mut pp = unsafe {
            xdg_popup_surface
                .get_popup(Some(&*self.root_window.get()), &positioner)
                .expect("pop.create")
        };
        let mut popup_state = Box::new(PopupState {
            surface_ptr: wl_popup_surface.as_ptr(),
        });
        xdg_popup_surface
            .set_listener(&mut *popup_state)
            .into_result()
            .expect("xdg_popup_surface.set_listener");
        pp.set_listener(&mut *popup_state)
            .into_result()
            .expect("pop.set_listener");
        wl_popup_surface.commit().expect("wl_popup_surface.commit");
        unsafe {
            // process configure event...(Kwinとかはconfigureくるまえにattachするとエラーが出ておちる)
            (*self.display).roundtrip().expect("roundtrip");
        }

        wl_popup_surface
            .attach(Some(self.buf.buffer()), 0, 0)
            .expect("wl_popup_surface.attach");
        wl_popup_surface
            .damage(0, 0, -1, -1)
            .expect("wl_popup_surface.damage");
        let viewport = unsafe {
            (*self.wl_interfaces)
                .viewporter
                .get_viewport(&wl_popup_surface)
                .expect("popup_viewport.create")
        };
        viewport
            .set_source(
                wl::Fixed::ZERO,
                wl::Fixed::ZERO,
                wl::Fixed::ONE,
                wl::Fixed::ONE,
            )
            .expect("viewport.set_source");
        viewport
            .set_destination(size.width as _, size.height as _)
            .expect("viewport.set_destination");

        let blur = if let Some(bm) = unsafe { (*self.wl_interfaces).kde_blur_manager.as_ref() } {
            let blur = bm.create(&wl_popup_surface).expect("blur.create");
            blur.commit().expect("blur.commit");

            Some(blur)
        } else {
            None
        };

        wl_popup_surface.commit().expect("wl_popup_surface.commit");
        tracing::debug!("popup commit");

        unsafe {
            (*self.popup.get()) = Some((
                blur,
                pp,
                xdg_popup_surface,
                viewport,
                wl_popup_surface,
                popup_state,
            ));
        }
    }

    pub fn r#move(&self, p: &Point<PointerInputUnit>) {
        let Some((_, pp, _, _, _, _)) = (unsafe { &*self.popup.get() }) else {
            return;
        };

        let pos = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .create_positioner()
                .expect("pos.create")
        };
        pos.set_offset(p.x as _, p.y as _).expect("pos.set_offset");
        pp.reposition(&pos, 0).expect("pp.reposition");
    }

    pub fn hide(&self) {
        unsafe {
            (*self.popup.get()) = None;
        }
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
    pub global_messaging_ptr: *const GlobalMessaging,
}

impl crate::SystemLink<'_> {
    pub fn prelaunch(&self, _main_window: toplevel::Handle) {
        unsafe { &mut *self.display_server.context }
            .dp
            .roundtrip()
            .expect("roundtrip");
    }

    pub fn create_main_window(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) -> toplevel::Handle {
        toplevel::NativeWindow::new(
            WindowType::Main {
                #[cfg(target_os = "linux")]
                termination_event: self.terminate_event.clone(),
            },
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
            &self.rt_sender,
        )
        .into_handle()
    }

    pub fn open_window<'h>(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        hit_tree: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
        setup_contents: impl FnOnce(
            toplevel::Handle,
            &mut CompositeTree<SyncEvent>,
            &mut HitTestTreeManager,
            &mut KeyboardFocusTokenRegistry,
            &Self,
        ),
    ) -> toplevel::Handle {
        let w = toplevel::NativeWindow::new(
            WindowType::Sub,
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
            &self.rt_sender,
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
                    DragPreviewPopoverHandle::BG_COLOR
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
    ) -> DragPreviewPopoverBuffer {
        if let Some(ref spb) = interfaces.single_pixel_buffer_manager {
            let c = DragPreviewPopoverHandle::BG_COLOR.premultiplied();
            let b = spb
                .create_u32_rgba_buffer(c.r_u32(), c.g_u32(), c.b_u32(), c.a_u32())
                .expect("popup_buf.create.single_pixel_buffer");

            DragPreviewPopoverBuffer::SinglePixel(b)
        } else {
            // traditional shm-based single pixel buffer
            let (shm, _, _) = self.shm.as_ref().expect("no shm");

            let buf = shm
                .create_buffer(0, 1, 1, 4, wl::ShmFormat::ARGB8888)
                .expect("buf.create.popup");

            DragPreviewPopoverBuffer::Shm { buf }
        }
    }
}

struct PopupState {
    surface_ptr: *mut wl::Surface,
}
impl wl::XdgSurfaceEventListener for PopupState {
    #[tracing::instrument(name = "xdg_surface(Popup)::configure", skip(self, sender))]
    fn configure(&mut self, sender: &mut wl::XdgSurface, serial: u32) {
        event_trace!();

        sender.ack_configure(serial).expect("popup.ack_configure");
        unsafe {
            (*self.surface_ptr).commit().expect("popup.surface.commit");
        }
    }
}
impl wl::XdgPopupEventListener for PopupState {
    #[tracing::instrument(name = "xdg_popup::configure", skip(self, _sender))]
    fn configure(&mut self, _sender: &mut wl::XdgPopup, x: i32, y: i32, width: i32, height: i32) {
        event_trace!();
    }

    #[tracing::instrument(name = "xdg_popup::popup_done", skip(self, _sender))]
    fn popup_done(&mut self, _sender: &mut wl::XdgPopup) {
        event_trace!();
    }

    #[tracing::instrument(name = "xdg_popup::repositioned", skip(self, _sender))]
    fn repositioned(&mut self, _sender: &mut wl::XdgPopup, token: u32) {
        event_trace!();
    }
}

#[allow(dead_code)]
pub enum DragPreviewPopoverBuffer {
    SinglePixel(wl::Owned<wl::Buffer>),
    Shm { buf: wl::Owned<wl::Buffer> },
}
impl DragPreviewPopoverBuffer {
    #[inline(always)]
    pub fn buffer(&self) -> &wl::Buffer {
        match self {
            Self::SinglePixel(x) => x,
            Self::Shm { buf, .. } => buf,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PointerID(*mut wl::Pointer);

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
    committed_text: String,
    preedit_text: String,
}

pub struct GlobalMessaging {
    text_input_manager: NonNull<wl::ZwpTextInputManagerV3>,
    xkb_context: xkbcommon::Context,
    keyboard: Option<KeyboardState>,
    pointer: Option<PointerState>,
    cursor_shape_manager: Option<NonNull<wl::WpCursorShapeManagerV1>>,
    event_dispatcher: LogicFiberEventDispatcher,
    ime_pending_state: IMEPendingState,
    _pinned: core::marker::PhantomPinned,
}
impl GlobalMessaging {
    pub fn new(ctx: &DisplayServerContext, event_dispatcher: LogicFiberEventDispatcher) -> Self {
        Self {
            text_input_manager: unsafe { ctx.global_interfaces.text_input_manager.copy_ptr() },
            xkb_context: xkbcommon::Context::new(xkbcommon::ContextFlags::NO_FLAGS)
                .expect("xkb_context.create"),
            keyboard: None,
            pointer: None,
            cursor_shape_manager: ctx
                .global_interfaces
                .cursor_shape_manager
                .as_ref()
                .map(|x| unsafe { x.copy_ptr() }),
            event_dispatcher,
            ime_pending_state: IMEPendingState {
                committed_text: String::new(),
                preedit_text: String::new(),
            },
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
    #[tracing::instrument(skip(self, pointer, surface), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
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
        });
        state.pos = Point::new_logical(surface_x.to_f32(), surface_y.to_f32());

        let surface_state = unsafe { &*surface.user_data().cast::<SurfaceStateUntyped>() };
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
                self.event_dispatcher
                    .dispatch(Event::MenuPointerMove {
                        pointer_id: PointerID(pointer),
                        target: flyout_surface::Handle(NonNull::from_mut(surface)),
                        client_pos: state.pos,
                    });
            }
        }
    }

    #[tracing::instrument(skip(self, pointer, surface))]
    fn leave(&mut self, pointer: &mut wl::Pointer, serial: u32, surface: Option<&mut wl::Surface>) {
        event_trace!();

        let state = self.pointer.as_mut().expect("no pointer state initialized");

        if let Some(surface) = surface {
            let surface_state = unsafe { &*surface.user_data().cast::<SurfaceStateUntyped>() };
            match surface_state.tag {
                SurfaceStateTag::ToplevelWindow => {
                    self.event_dispatcher.dispatch(Event::PointerLeaveWindow {
                        pointer_id: PointerID(pointer),
                        window: toplevel::Handle::from_mut(surface),
                    });
                }
                SurfaceStateTag::FlyoutSurface => {
                    self.event_dispatcher
                        .dispatch(Event::MenuPointerLeave {
                            pointer_id: PointerID(pointer),
                            target: flyout_surface::Handle(NonNull::from_mut(surface)),
                        });
                }
                _ => (),
            }
        }

        state.enter_state = None;
    }

    #[tracing::instrument(skip(self, pointer), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
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
        let surface_state = unsafe {
            &*enter_state
                .surface
                .as_ref()
                .user_data()
                .cast::<SurfaceStateUntyped>()
        };
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
                self.event_dispatcher
                    .dispatch(Event::MenuPointerMove {
                        pointer_id: PointerID(pointer),
                        target: flyout_surface::Handle(enter_state.surface),
                        client_pos: state.pos,
                    });
            }
        }
    }

    #[tracing::instrument(skip(self, pointer), fields(state = state as u32))]
    fn button(
        &mut self,
        pointer: &mut wl::Pointer,
        serial: u32,
        time: u32,
        button: u32,
        state: wl::PointerButtonState,
    ) {
        event_trace!();

        let pointer_state = self.pointer.as_ref().expect("no pointer state initialized");
        let Some(ref enter_state) = pointer_state.enter_state else {
            return;
        };

        if state == wl::PointerButtonState::Pressed {
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
                    self.event_dispatcher
                        .dispatch(Event::MenuPointerDown {
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

    #[tracing::instrument(skip(self, _pointer))]
    fn axis(&mut self, _pointer: &mut wl::Pointer, time: u32, axis: u32, value: wl::Fixed) {
        event_trace!();
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn frame(&mut self, _pointer: &mut wl::Pointer) {
        event_trace!();
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_source(&mut self, _pointer: &mut wl::Pointer, axis_source: u32) {
        event_trace!();
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_stop(&mut self, _pointer: &mut wl::Pointer, time: u32, axis: u32) {
        event_trace!();
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_discrete(&mut self, _pointer: &mut wl::Pointer, axis: u32, discrete: i32) {
        event_trace!();
    }

    #[tracing::instrument(skip(self, _pointer))]
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

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_relative_direction(&mut self, _pointer: &mut wl::Pointer, axis: u32, direction: u32) {
        event_trace!();
    }
}
impl wl::KeyboardEventListener for GlobalMessaging {
    #[tracing::instrument(skip(self, _sender))]
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

    #[tracing::instrument(skip(self, _sender, surface))]
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

    #[tracing::instrument(skip(self, _sender, surface))]
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

    #[tracing::instrument(skip(self, _sender))]
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

    #[tracing::instrument(skip(self, _sender))]
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

    #[tracing::instrument(skip(self, _sender))]
    fn repeat_info(&mut self, _sender: &mut wl::Keyboard, rate: i32, delay: i32) {
        event_trace!();
    }
}
impl wl::ZwpTextInputV3EventListener for GlobalMessaging {
    #[tracing::instrument(skip(self, sender, _surface))]
    fn enter(&mut self, sender: &mut wl::ZwpTextInputV3, _surface: Option<&mut wl::Surface>) {
        event_trace!();

        sender.enable().expect("text_input.enable");
        sender.commit().expect("text_input.commit");
    }

    #[tracing::instrument(skip(self, sender, _surface))]
    fn leave(&mut self, sender: &mut wl::ZwpTextInputV3, _surface: Option<&mut wl::Surface>) {
        event_trace!();

        sender.disable().expect("text_input.disable");
        sender.commit().expect("text_input.commit");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn preedit_string(
        &mut self,
        _sender: &mut wl::ZwpTextInputV3,
        text: Option<&core::ffi::CStr>,
        cursor_begin: i32,
        cursor_end: i32,
    ) {
        event_trace!();

        self.ime_pending_state.preedit_text = text
            .map(|t| t.to_string_lossy().into_owned())
            .unwrap_or_default();
    }

    #[tracing::instrument(skip(self, _sender))]
    fn commit_string(&mut self, _sender: &mut wl::ZwpTextInputV3, text: Option<&core::ffi::CStr>) {
        event_trace!();

        self.ime_pending_state.committed_text = text
            .map(|t| t.to_string_lossy().into_owned())
            .unwrap_or_default();
    }

    #[tracing::instrument(skip(self, _sender))]
    fn delete_surrounding_text(
        &mut self,
        _sender: &mut wl::ZwpTextInputV3,
        before_length: u32,
        after_length: u32,
    ) {
        event_trace!();
    }

    #[tracing::instrument(skip(self, _sender))]
    fn done(&mut self, _sender: &mut wl::ZwpTextInputV3, serial: u32) {
        event_trace!();

        let k_state = self.keyboard.as_ref().expect("keyboard.uninit");
        let Some(ref k_enter_state) = k_state.enter_state else {
            return;
        };

        self.event_dispatcher.dispatch(Event::IMEStateChanges {
            window: toplevel::Handle(k_enter_state.surface),
            committed_string: core::mem::replace(
                &mut self.ime_pending_state.committed_text,
                String::new(),
            ),
            preedit_string: core::mem::replace(
                &mut self.ime_pending_state.preedit_text,
                String::new(),
            ),
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
    outputs: Vec<wl::Owned<wl::Output>>,
    compositor: wl::Owned<wl::Compositor>,
    subcompositor: wl::Owned<wl::Subcompositor>,
    xdg_wm_base: wl::Owned<wl::XdgWmBase>,
    seat: wl::Owned<wl::Seat>,
    shm: wl::Owned<wl::Shm>,
    viewporter: wl::Owned<wl::WpViewporter>,
    text_input_manager: wl::Owned<wl::ZwpTextInputManagerV3>,
    // optional requirements
    single_pixel_buffer_manager: Option<wl::Owned<wl::WpSinglePixelBufferManagerV1>>,
    kde_blur_manager: Option<wl::Owned<wl::OrgKdeKwinBlurManager>>,
    kde_appmenu_manager: Option<wl::Owned<wl::OrgKdeKwinAppmenuManager>>,
    kde_shadow_manager: Option<wl::Owned<wl::OrgKdeKwinShadowManager>>,
    zxdg_decoration_manager: Option<wl::Owned<wl::ZxdgDecorationManagerV1>>,
    cursor_shape_manager: Option<wl::Owned<wl::WpCursorShapeManagerV1>>,
    fractional_scale_manager: Option<wl::Owned<wl::WpFractionalScaleManagerV1>>,
    alpha_modifier: Option<wl::Owned<wl::WpAlphaModifierV1>>,
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
            shm: rl.shm.expect("no shm"),
            viewporter: rl.viewporter.expect("no viewporter"),
            text_input_manager: rl.text_input_manager.expect("no text-input"),
            single_pixel_buffer_manager: rl.single_pixel_buffer_manager,
            kde_blur_manager: rl.kde_blur_manager,
            kde_appmenu_manager: rl.kde_appmenu_manager,
            kde_shadow_manager: rl.kde_shadow_manager,
            zxdg_decoration_manager: rl.zxdg_decoration_manager,
            cursor_shape_manager: rl.cursor_shape_manager,
            fractional_scale_manager: rl.fractional_scale_manager,
            alpha_modifier: rl.alpha_modifier,
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
    outputs: Vec<wl::Owned<wl::Output>>,
    xdg_wm_base: Option<wl::Owned<wl::XdgWmBase>>,
    seat: Option<wl::Owned<wl::Seat>>,
    shm: Option<wl::Owned<wl::Shm>>,
    viewporter: Option<wl::Owned<wl::WpViewporter>>,
    text_input_manager: Option<wl::Owned<wl::ZwpTextInputManagerV3>>,
    single_pixel_buffer_manager: Option<wl::Owned<wl::WpSinglePixelBufferManagerV1>>,
    kde_blur_manager: Option<wl::Owned<wl::OrgKdeKwinBlurManager>>,
    kde_appmenu_manager: Option<wl::Owned<wl::OrgKdeKwinAppmenuManager>>,
    kde_shadow_manager: Option<wl::Owned<wl::OrgKdeKwinShadowManager>>,
    zxdg_decoration_manager: Option<wl::Owned<wl::ZxdgDecorationManagerV1>>,
    cursor_shape_manager: Option<wl::Owned<wl::WpCursorShapeManagerV1>>,
    fractional_scale_manager: Option<wl::Owned<wl::WpFractionalScaleManagerV1>>,
    alpha_modifier: Option<wl::Owned<wl::WpAlphaModifierV1>>,
    is_hyprland: bool,
}
impl wl::RegistryListener for RegistryListener {
    fn global(
        &mut self,
        registry: &mut peridot_tp_wayland::Registry,
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
            self.outputs
                .push(registry.bind(name, version).expect("bind output"));
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
        if interface == c"org_kde_kwin_shadow_manager" {
            self.kde_shadow_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind kde_shadow_manager"),
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
    }

    fn global_remove(&mut self, _registry: &mut peridot_tp_wayland::Registry, name: u32) {
        tracing::info!(target: "wl::diag", name, "wl interface remove");
    }
}
