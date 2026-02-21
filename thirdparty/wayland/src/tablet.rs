//! tablet_v2: Wayland protocol for graphics tablets
//!
//! This description provides a high-level overview of the interplay between
//! the interfaces defined this protocol. For details, see the protocol
//! specification.
//!
//! More than one tablet may exist, and device-specifics matter. Tablets are
//! not represented by a single virtual device like wl_pointer. A client
//! binds to the tablet manager object which is just a proxy object. From
//! that, the client requests wp_tablet_manager.get_tablet_seat(wl_seat)
//! and that returns the actual interface that has all the tablets. With
//! this indirection, we can avoid merging wp_tablet into the actual Wayland
//! protocol, a long-term benefit.
//!
//! The wp_tablet_seat sends a "tablet added" event for each tablet
//! connected. That event is followed by descriptive events about the
//! hardware; currently that includes events for name, vid/pid and
//! a wp_tablet.path event that describes a local path. This path can be
//! used to uniquely identify a tablet or get more information through
//! libwacom. Emulated or nested tablets can skip any of those, e.g. a
//! virtual tablet may not have a vid/pid. The sequence of descriptive
//! events is terminated by a wp_tablet.done event to signal that a client
//! may now finalize any initialization for that tablet.
//!
//! Events from tablets require a tool in proximity. Tools are also managed
//! by the tablet seat; a "tool added" event is sent whenever a tool is new
//! to the compositor. That event is followed by a number of descriptive
//! events about the hardware; currently that includes capabilities,
//! hardware id and serial number, and tool type. Similar to the tablet
//! interface, a wp_tablet_tool.done event is sent to terminate that initial
//! sequence.
//!
//! Any event from a tool happens on the wp_tablet_tool interface. When the
//! tool gets into proximity of the tablet, a proximity_in event is sent on
//! the wp_tablet_tool interface, listing the tablet and the surface. That
//! event is followed by a motion event with the coordinates. After that,
//! it's the usual motion, axis, button, etc. events. The protocol's
//! serialisation means events are grouped by wp_tablet_tool.frame events.
//!
//! Two special events (that don't exist in X) are down and up. They signal
//! "tip touching the surface". For tablets without real proximity
//! detection, the sequence is: proximity_in, motion, down, frame.
//!
//! When the tool leaves proximity, a proximity_out event is sent. If any
//! button is still down, a button release event is sent before this
//! proximity event. These button events are sent in the same frame as the
//! proximity event to signal to the client that the buttons were held when
//! the tool left proximity.
//!
//! If the tool moves out of the surface but stays in proximity (i.e.
//! between windows), compositor-specific grab policies apply. This usually
//! means that the proximity-out is delayed until all buttons are released.
//!
//! Moving a tool physically from one tablet to the other has no real effect
//! on the protocol, since we already have the tool object from the "tool
//! added" event. All the information is already there and the proximity
//! events on both tablets are all a client needs to reconstruct what
//! happened.
//!
//! Some extra axes are normalized, i.e. the client knows the range as
//! specified in the protocol (e.g. [0, 65535]), the granularity however is
//! unknown. The current normalized axes are pressure, distance, and slider.
//!
//! Other extra axes are in physical units as specified in the protocol.
//! The current extra axes with physical units are tilt, rotation and
//! wheel rotation.
//!
//! Since tablets work independently of the pointer controlled by the mouse,
//! the focus handling is independent too and controlled by proximity.
//! The wp_tablet_tool.set_cursor request sets a tool-specific cursor.
//! This cursor surface may be the same as the mouse cursor, and it may be
//! the same across tools but it is possible to be more fine-grained. For
//! example, a client may set different cursors for the pen and eraser.
//!
//! Tools are generally independent of tablets and it is
//! compositor-specific policy when a tool can be removed. Common approaches
//! will likely include some form of removing a tool when all tablets the
//! tool was used on are removed.
//!

use crate::{Interface, Proxy, ffi};

static ZWP_TABLET_MANAGER_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_tablet_manager_v2".as_ptr(),
    version: 2,
    method_count: 2,
    methods: const {
        [
            ffi::Message {
                name: c"get_tablet_seat".as_ptr(),
                signature: c"no".as_ptr(),
                types: const { [crate::ZwpTabletSeatV2::DEF, crate::Seat::DEF] }.as_ptr(),
            },
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 0,
    events: const { [] }.as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTabletManagerV2(pub(crate) Proxy);
unsafe impl Interface for ZwpTabletManagerV2 {
    const DEF: *const ffi::Interface = &ZWP_TABLET_MANAGER_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTabletManagerV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(1);
    }
}

impl ZwpTabletManagerV2 {
    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   

    #[inline]
    pub fn get_tablet_seat(
        &self,
        seat: &crate::Seat,
    ) -> crate::Result<crate::Owned<crate::ZwpTabletSeatV2>> {
        Ok(unsafe {
            crate::Owned::wrap_unchecked(
                self.0
                    .marshal_array_typed(0, &mut [crate::NEWID_ARG, seat.0.as_arg()])?,
            )
        })
    }
}

static ZWP_TABLET_SEAT_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_tablet_seat_v2".as_ptr(),
    version: 2,
    method_count: 1,
    methods: const {
        [ffi::Message {
            name: c"destroy".as_ptr(),
            signature: c"".as_ptr(),
            types: const { [] }.as_ptr(),
        }]
    }
    .as_ptr(),
    event_count: 3,
    events: const {
        [
            ffi::Message {
                name: c"tablet_added".as_ptr(),
                signature: c"n".as_ptr(),
                types: const { [crate::ZwpTabletV2::DEF] }.as_ptr(),
            },
            ffi::Message {
                name: c"tool_added".as_ptr(),
                signature: c"n".as_ptr(),
                types: const { [crate::ZwpTabletToolV2::DEF] }.as_ptr(),
            },
            ffi::Message {
                name: c"pad_added".as_ptr(),
                signature: c"n".as_ptr(),
                types: const { [crate::ZwpTabletPadV2::DEF] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTabletSeatV2(pub(crate) Proxy);
unsafe impl Interface for ZwpTabletSeatV2 {
    const DEF: *const ffi::Interface = &ZWP_TABLET_SEAT_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTabletSeatV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZwpTabletSeatV2 {
    pub fn set_listener<'l, L: ZwpTabletSeatV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn tablet_added<L: ZwpTabletSeatV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            id: *mut ffi::Proxy,
        ) {
            L::tablet_added(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe {
                    crate::Owned::from_untyped_unchecked(core::ptr::NonNull::new_unchecked(
                        crate::Proxy::cast_ffi_ptr(id),
                    ))
                },
            )
        }
        extern "C" fn tool_added<L: ZwpTabletSeatV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            id: *mut ffi::Proxy,
        ) {
            L::tool_added(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe {
                    crate::Owned::from_untyped_unchecked(core::ptr::NonNull::new_unchecked(
                        crate::Proxy::cast_ffi_ptr(id),
                    ))
                },
            )
        }
        extern "C" fn pad_added<L: ZwpTabletSeatV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            id: *mut ffi::Proxy,
        ) {
            L::pad_added(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe {
                    crate::Owned::from_untyped_unchecked(core::ptr::NonNull::new_unchecked(
                        crate::Proxy::cast_ffi_ptr(id),
                    ))
                },
            )
        }

        #[repr(C)]
        struct FPTable {
            tablet_added: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                id: *mut ffi::Proxy,
            ),
            tool_added: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                id: *mut ffi::Proxy,
            ),
            pad_added: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                id: *mut ffi::Proxy,
            ),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        tablet_added: tablet_added::<L>,
                        tool_added: tool_added::<L>,
                        pad_added: pad_added::<L>,
                    }
                } as &'static FPTable as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   
}

pub trait ZwpTabletSeatV2EventListener {
    fn tablet_added(&mut self, sender: &mut ZwpTabletSeatV2, id: crate::Owned<crate::ZwpTabletV2>);
    fn tool_added(
        &mut self,
        sender: &mut ZwpTabletSeatV2,
        id: crate::Owned<crate::ZwpTabletToolV2>,
    );
    fn pad_added(&mut self, sender: &mut ZwpTabletSeatV2, id: crate::Owned<crate::ZwpTabletPadV2>);
}

static ZWP_TABLET_TOOL_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_tablet_tool_v2".as_ptr(),
    version: 2,
    method_count: 2,
    methods: const {
        [
            ffi::Message {
                name: c"set_cursor".as_ptr(),
                signature: c"u?oii".as_ptr(),
                types: const {
                    [
                        core::ptr::null(),
                        crate::Surface::DEF,
                        core::ptr::null(),
                        core::ptr::null(),
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 19,
    events: const {
        [
            ffi::Message {
                name: c"type".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"hardware_serial".as_ptr(),
                signature: c"uu".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"hardware_id_wacom".as_ptr(),
                signature: c"uu".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"capability".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"done".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"removed".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"proximity_in".as_ptr(),
                signature: c"uoo".as_ptr(),
                types: const {
                    [
                        core::ptr::null(),
                        crate::ZwpTabletV2::DEF,
                        crate::Surface::DEF,
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"proximity_out".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"down".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"up".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"motion".as_ptr(),
                signature: c"ff".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"pressure".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"distance".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"tilt".as_ptr(),
                signature: c"ff".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"rotation".as_ptr(),
                signature: c"f".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"slider".as_ptr(),
                signature: c"i".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"wheel".as_ptr(),
                signature: c"fi".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"button".as_ptr(),
                signature: c"uuu".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"frame".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTabletToolV2(pub(crate) Proxy);
unsafe impl Interface for ZwpTabletToolV2 {
    const DEF: *const ffi::Interface = &ZWP_TABLET_TOOL_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTabletToolV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(1);
    }
}

impl ZwpTabletToolV2 {
    pub fn set_listener<'l, L: ZwpTabletToolV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn r#type<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            tool_type: u32,
        ) {
            L::r#type(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { core::mem::transmute(tool_type) },
            )
        }
        extern "C" fn hardware_serial<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            hardware_serial_hi: u32,
            hardware_serial_lo: u32,
        ) {
            L::hardware_serial(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                hardware_serial_hi,
                hardware_serial_lo,
            )
        }
        extern "C" fn hardware_id_wacom<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            hardware_id_hi: u32,
            hardware_id_lo: u32,
        ) {
            L::hardware_id_wacom(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                hardware_id_hi,
                hardware_id_lo,
            )
        }
        extern "C" fn capability<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            capability: u32,
        ) {
            L::capability(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { core::mem::transmute(capability) },
            )
        }
        extern "C" fn done<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::done(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn removed<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::removed(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn proximity_in<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            serial: u32,
            tablet: *mut ffi::Proxy,
            surface: *mut ffi::Proxy,
        ) {
            L::proximity_in(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                serial,
                unsafe { &mut *(tablet as *mut _) },
                unsafe { &mut *(surface as *mut _) },
            )
        }
        extern "C" fn proximity_out<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::proximity_out(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn down<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            serial: u32,
        ) {
            L::down(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                serial,
            )
        }
        extern "C" fn up<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::up(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn motion<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            x: ffi::Fixed,
            y: ffi::Fixed,
        ) {
            L::motion(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                x,
                y,
            )
        }
        extern "C" fn pressure<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            pressure: u32,
        ) {
            L::pressure(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                pressure,
            )
        }
        extern "C" fn distance<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            distance: u32,
        ) {
            L::distance(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                distance,
            )
        }
        extern "C" fn tilt<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            tilt_x: ffi::Fixed,
            tilt_y: ffi::Fixed,
        ) {
            L::tilt(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                tilt_x,
                tilt_y,
            )
        }
        extern "C" fn rotation<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            degrees: ffi::Fixed,
        ) {
            L::rotation(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                degrees,
            )
        }
        extern "C" fn slider<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            position: i32,
        ) {
            L::slider(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                position,
            )
        }
        extern "C" fn wheel<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            degrees: ffi::Fixed,
            clicks: i32,
        ) {
            L::wheel(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                degrees,
                clicks,
            )
        }
        extern "C" fn button<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            serial: u32,
            button: u32,
            state: u32,
        ) {
            L::button(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                serial,
                button,
                unsafe { core::mem::transmute(state) },
            )
        }
        extern "C" fn frame<L: ZwpTabletToolV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            time: u32,
        ) {
            L::frame(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                time,
            )
        }

        #[repr(C)]
        struct FPTable {
            r#type: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                tool_type: u32,
            ),
            hardware_serial: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                hardware_serial_hi: u32,
                hardware_serial_lo: u32,
            ),
            hardware_id_wacom: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                hardware_id_hi: u32,
                hardware_id_lo: u32,
            ),
            capability: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                capability: u32,
            ),
            done: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            removed: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            proximity_in: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                serial: u32,
                tablet: *mut ffi::Proxy,
                surface: *mut ffi::Proxy,
            ),
            proximity_out: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            down:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, serial: u32),
            up: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            motion: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                x: ffi::Fixed,
                y: ffi::Fixed,
            ),
            pressure: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                pressure: u32,
            ),
            distance: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                distance: u32,
            ),
            tilt: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                tilt_x: ffi::Fixed,
                tilt_y: ffi::Fixed,
            ),
            rotation: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                degrees: ffi::Fixed,
            ),
            slider: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                position: i32,
            ),
            wheel: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                degrees: ffi::Fixed,
                clicks: i32,
            ),
            button: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                serial: u32,
                button: u32,
                state: u32,
            ),
            frame:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, time: u32),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        r#type: r#type::<L>,
                        hardware_serial: hardware_serial::<L>,
                        hardware_id_wacom: hardware_id_wacom::<L>,
                        capability: capability::<L>,
                        done: done::<L>,
                        removed: removed::<L>,
                        proximity_in: proximity_in::<L>,
                        proximity_out: proximity_out::<L>,
                        down: down::<L>,
                        up: up::<L>,
                        motion: motion::<L>,
                        pressure: pressure::<L>,
                        distance: distance::<L>,
                        tilt: tilt::<L>,
                        rotation: rotation::<L>,
                        slider: slider::<L>,
                        wheel: wheel::<L>,
                        button: button::<L>,
                        frame: frame::<L>,
                    }
                } as &'static FPTable as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   

    #[inline]
    pub fn set_cursor(
        &self,
        serial: u32,
        surface: Option<&crate::Surface>,
        hotspot_x: i32,
        hotspot_y: i32,
    ) -> crate::Result<()> {
        self.0.marshal_array_void(
            0,
            &mut [
                ffi::Argument { u: serial },
                surface.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg()),
                ffi::Argument { i: hotspot_x },
                ffi::Argument { i: hotspot_y },
            ],
        )
    }
}

pub trait ZwpTabletToolV2EventListener {
    fn r#type(&mut self, sender: &mut ZwpTabletToolV2, tool_type: ZwpTabletToolV2Type);
    fn hardware_serial(
        &mut self,
        sender: &mut ZwpTabletToolV2,
        hardware_serial_hi: u32,
        hardware_serial_lo: u32,
    );
    fn hardware_id_wacom(
        &mut self,
        sender: &mut ZwpTabletToolV2,
        hardware_id_hi: u32,
        hardware_id_lo: u32,
    );
    fn capability(&mut self, sender: &mut ZwpTabletToolV2, capability: ZwpTabletToolV2Capability);
    fn done(&mut self, sender: &mut ZwpTabletToolV2);
    fn removed(&mut self, sender: &mut ZwpTabletToolV2);
    fn proximity_in(
        &mut self,
        sender: &mut ZwpTabletToolV2,
        serial: u32,
        tablet: &mut crate::ZwpTabletV2,
        surface: &mut crate::Surface,
    );
    fn proximity_out(&mut self, sender: &mut ZwpTabletToolV2);
    fn down(&mut self, sender: &mut ZwpTabletToolV2, serial: u32);
    fn up(&mut self, sender: &mut ZwpTabletToolV2);
    fn motion(&mut self, sender: &mut ZwpTabletToolV2, x: crate::Fixed, y: crate::Fixed);
    fn pressure(&mut self, sender: &mut ZwpTabletToolV2, pressure: u32);
    fn distance(&mut self, sender: &mut ZwpTabletToolV2, distance: u32);
    fn tilt(&mut self, sender: &mut ZwpTabletToolV2, tilt_x: crate::Fixed, tilt_y: crate::Fixed);
    fn rotation(&mut self, sender: &mut ZwpTabletToolV2, degrees: crate::Fixed);
    fn slider(&mut self, sender: &mut ZwpTabletToolV2, position: i32);
    fn wheel(&mut self, sender: &mut ZwpTabletToolV2, degrees: crate::Fixed, clicks: i32);
    fn button(
        &mut self,
        sender: &mut ZwpTabletToolV2,
        serial: u32,
        button: u32,
        state: ZwpTabletToolV2ButtonState,
    );
    fn frame(&mut self, sender: &mut ZwpTabletToolV2, time: u32);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZwpTabletToolV2Type {
    Pen = 320,
    Eraser = 321,
    Brush = 322,
    Pencil = 323,
    Airbrush = 324,
    Finger = 325,
    Mouse = 326,
    Lens = 327,
}
impl ZwpTabletToolV2Type {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZwpTabletToolV2Capability {
    Tilt = 1,
    Pressure = 2,
    Distance = 3,
    Rotation = 4,
    Slider = 5,
    Wheel = 6,
}
impl ZwpTabletToolV2Capability {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZwpTabletToolV2ButtonState {
    Released = 0,
    Pressed = 1,
}
impl ZwpTabletToolV2ButtonState {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZwpTabletToolV2Error {
    Role = 0,
}
impl ZwpTabletToolV2Error {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

static ZWP_TABLET_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_tablet_v2".as_ptr(),
    version: 2,
    method_count: 1,
    methods: const {
        [ffi::Message {
            name: c"destroy".as_ptr(),
            signature: c"".as_ptr(),
            types: const { [] }.as_ptr(),
        }]
    }
    .as_ptr(),
    event_count: 6,
    events: const {
        [
            ffi::Message {
                name: c"name".as_ptr(),
                signature: c"s".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"id".as_ptr(),
                signature: c"uu".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"path".as_ptr(),
                signature: c"s".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"done".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"removed".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"bustype".as_ptr(),
                signature: c"2u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTabletV2(pub(crate) Proxy);
unsafe impl Interface for ZwpTabletV2 {
    const DEF: *const ffi::Interface = &ZWP_TABLET_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTabletV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZwpTabletV2 {
    pub fn set_listener<'l, L: ZwpTabletV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn name<L: ZwpTabletV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            name: *const core::ffi::c_char,
        ) {
            L::name(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { core::ffi::CStr::from_ptr(name) },
            )
        }
        extern "C" fn id<L: ZwpTabletV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            vid: u32,
            pid: u32,
        ) {
            L::id(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                vid,
                pid,
            )
        }
        extern "C" fn path<L: ZwpTabletV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            path: *const core::ffi::c_char,
        ) {
            L::path(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { core::ffi::CStr::from_ptr(path) },
            )
        }
        extern "C" fn done<L: ZwpTabletV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::done(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn removed<L: ZwpTabletV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::removed(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn bustype<L: ZwpTabletV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            bustype: u32,
        ) {
            L::bustype(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { core::mem::transmute(bustype) },
            )
        }

        #[repr(C)]
        struct FPTable {
            name: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                name: *const core::ffi::c_char,
            ),
            id: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                vid: u32,
                pid: u32,
            ),
            path: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                path: *const core::ffi::c_char,
            ),
            done: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            removed: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            bustype: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                bustype: u32,
            ),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        name: name::<L>,
                        id: id::<L>,
                        path: path::<L>,
                        done: done::<L>,
                        removed: removed::<L>,
                        bustype: bustype::<L>,
                    }
                } as &'static FPTable as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   
}

pub trait ZwpTabletV2EventListener {
    fn name(&mut self, sender: &mut ZwpTabletV2, name: &core::ffi::CStr);
    fn id(&mut self, sender: &mut ZwpTabletV2, vid: u32, pid: u32);
    fn path(&mut self, sender: &mut ZwpTabletV2, path: &core::ffi::CStr);
    fn done(&mut self, sender: &mut ZwpTabletV2);
    fn removed(&mut self, sender: &mut ZwpTabletV2);
    fn bustype(&mut self, sender: &mut ZwpTabletV2, bustype: ZwpTabletV2Bustype);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZwpTabletV2Bustype {
    Usb = 3,
    Bluetooth = 5,
    Virtual = 6,
    Serial = 17,
    I2c = 24,
}
impl ZwpTabletV2Bustype {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

static ZWP_TABLET_PAD_RING_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_tablet_pad_ring_v2".as_ptr(),
    version: 2,
    method_count: 2,
    methods: const {
        [
            ffi::Message {
                name: c"set_feedback".as_ptr(),
                signature: c"su".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 4,
    events: const {
        [
            ffi::Message {
                name: c"source".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"angle".as_ptr(),
                signature: c"f".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"stop".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"frame".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTabletPadRingV2(pub(crate) Proxy);
unsafe impl Interface for ZwpTabletPadRingV2 {
    const DEF: *const ffi::Interface = &ZWP_TABLET_PAD_RING_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTabletPadRingV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(1);
    }
}

impl ZwpTabletPadRingV2 {
    pub fn set_listener<'l, L: ZwpTabletPadRingV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn source<L: ZwpTabletPadRingV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            source: u32,
        ) {
            L::source(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { core::mem::transmute(source) },
            )
        }
        extern "C" fn angle<L: ZwpTabletPadRingV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            degrees: ffi::Fixed,
        ) {
            L::angle(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                degrees,
            )
        }
        extern "C" fn stop<L: ZwpTabletPadRingV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::stop(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn frame<L: ZwpTabletPadRingV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            time: u32,
        ) {
            L::frame(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                time,
            )
        }

        #[repr(C)]
        struct FPTable {
            source:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, source: u32),
            angle: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                degrees: ffi::Fixed,
            ),
            stop: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            frame:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, time: u32),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        source: source::<L>,
                        angle: angle::<L>,
                        stop: stop::<L>,
                        frame: frame::<L>,
                    }
                } as &'static FPTable as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   

    #[inline]
    pub fn set_feedback(&self, description: &core::ffi::CStr, serial: u32) -> crate::Result<()> {
        self.0.marshal_array_void(
            0,
            &mut [
                ffi::Argument {
                    s: description.as_ptr(),
                },
                ffi::Argument { u: serial },
            ],
        )
    }
}

pub trait ZwpTabletPadRingV2EventListener {
    fn source(&mut self, sender: &mut ZwpTabletPadRingV2, source: ZwpTabletPadRingV2Source);
    fn angle(&mut self, sender: &mut ZwpTabletPadRingV2, degrees: crate::Fixed);
    fn stop(&mut self, sender: &mut ZwpTabletPadRingV2);
    fn frame(&mut self, sender: &mut ZwpTabletPadRingV2, time: u32);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZwpTabletPadRingV2Source {
    Finger = 1,
}
impl ZwpTabletPadRingV2Source {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

static ZWP_TABLET_PAD_STRIP_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_tablet_pad_strip_v2".as_ptr(),
    version: 2,
    method_count: 2,
    methods: const {
        [
            ffi::Message {
                name: c"set_feedback".as_ptr(),
                signature: c"su".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 4,
    events: const {
        [
            ffi::Message {
                name: c"source".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"position".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"stop".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"frame".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTabletPadStripV2(pub(crate) Proxy);
unsafe impl Interface for ZwpTabletPadStripV2 {
    const DEF: *const ffi::Interface = &ZWP_TABLET_PAD_STRIP_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTabletPadStripV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(1);
    }
}

impl ZwpTabletPadStripV2 {
    pub fn set_listener<'l, L: ZwpTabletPadStripV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn source<L: ZwpTabletPadStripV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            source: u32,
        ) {
            L::source(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { core::mem::transmute(source) },
            )
        }
        extern "C" fn position<L: ZwpTabletPadStripV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            position: u32,
        ) {
            L::position(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                position,
            )
        }
        extern "C" fn stop<L: ZwpTabletPadStripV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::stop(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn frame<L: ZwpTabletPadStripV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            time: u32,
        ) {
            L::frame(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                time,
            )
        }

        #[repr(C)]
        struct FPTable {
            source:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, source: u32),
            position: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                position: u32,
            ),
            stop: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            frame:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, time: u32),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        source: source::<L>,
                        position: position::<L>,
                        stop: stop::<L>,
                        frame: frame::<L>,
                    }
                } as &'static FPTable as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   

    #[inline]
    pub fn set_feedback(&self, description: &core::ffi::CStr, serial: u32) -> crate::Result<()> {
        self.0.marshal_array_void(
            0,
            &mut [
                ffi::Argument {
                    s: description.as_ptr(),
                },
                ffi::Argument { u: serial },
            ],
        )
    }
}

pub trait ZwpTabletPadStripV2EventListener {
    fn source(&mut self, sender: &mut ZwpTabletPadStripV2, source: ZwpTabletPadStripV2Source);
    fn position(&mut self, sender: &mut ZwpTabletPadStripV2, position: u32);
    fn stop(&mut self, sender: &mut ZwpTabletPadStripV2);
    fn frame(&mut self, sender: &mut ZwpTabletPadStripV2, time: u32);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZwpTabletPadStripV2Source {
    Finger = 1,
}
impl ZwpTabletPadStripV2Source {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

static ZWP_TABLET_PAD_GROUP_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_tablet_pad_group_v2".as_ptr(),
    version: 2,
    method_count: 1,
    methods: const {
        [ffi::Message {
            name: c"destroy".as_ptr(),
            signature: c"".as_ptr(),
            types: const { [] }.as_ptr(),
        }]
    }
    .as_ptr(),
    event_count: 7,
    events: const {
        [
            ffi::Message {
                name: c"buttons".as_ptr(),
                signature: c"a".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"ring".as_ptr(),
                signature: c"n".as_ptr(),
                types: const { [crate::ZwpTabletPadRingV2::DEF] }.as_ptr(),
            },
            ffi::Message {
                name: c"strip".as_ptr(),
                signature: c"n".as_ptr(),
                types: const { [crate::ZwpTabletPadStripV2::DEF] }.as_ptr(),
            },
            ffi::Message {
                name: c"modes".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"done".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"mode_switch".as_ptr(),
                signature: c"uuu".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"dial".as_ptr(),
                signature: c"2n".as_ptr(),
                types: const { [crate::ZwpTabletPadDialV2::DEF] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTabletPadGroupV2(pub(crate) Proxy);
unsafe impl Interface for ZwpTabletPadGroupV2 {
    const DEF: *const ffi::Interface = &ZWP_TABLET_PAD_GROUP_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTabletPadGroupV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZwpTabletPadGroupV2 {
    pub fn set_listener<'l, L: ZwpTabletPadGroupV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn buttons<L: ZwpTabletPadGroupV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            buttons: *mut ffi::Array,
        ) {
            L::buttons(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { &mut *buttons },
            )
        }
        extern "C" fn ring<L: ZwpTabletPadGroupV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            ring: *mut ffi::Proxy,
        ) {
            L::ring(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe {
                    crate::Owned::from_untyped_unchecked(core::ptr::NonNull::new_unchecked(
                        crate::Proxy::cast_ffi_ptr(ring),
                    ))
                },
            )
        }
        extern "C" fn strip<L: ZwpTabletPadGroupV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            strip: *mut ffi::Proxy,
        ) {
            L::strip(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe {
                    crate::Owned::from_untyped_unchecked(core::ptr::NonNull::new_unchecked(
                        crate::Proxy::cast_ffi_ptr(strip),
                    ))
                },
            )
        }
        extern "C" fn modes<L: ZwpTabletPadGroupV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            modes: u32,
        ) {
            L::modes(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                modes,
            )
        }
        extern "C" fn done<L: ZwpTabletPadGroupV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::done(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn mode_switch<L: ZwpTabletPadGroupV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            time: u32,
            serial: u32,
            mode: u32,
        ) {
            L::mode_switch(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                time,
                serial,
                mode,
            )
        }
        extern "C" fn dial<L: ZwpTabletPadGroupV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            dial: *mut ffi::Proxy,
        ) {
            L::dial(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe {
                    crate::Owned::from_untyped_unchecked(core::ptr::NonNull::new_unchecked(
                        crate::Proxy::cast_ffi_ptr(dial),
                    ))
                },
            )
        }

        #[repr(C)]
        struct FPTable {
            buttons: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                buttons: *mut ffi::Array,
            ),
            ring: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                ring: *mut ffi::Proxy,
            ),
            strip: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                strip: *mut ffi::Proxy,
            ),
            modes:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, modes: u32),
            done: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            mode_switch: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                time: u32,
                serial: u32,
                mode: u32,
            ),
            dial: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                dial: *mut ffi::Proxy,
            ),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        buttons: buttons::<L>,
                        ring: ring::<L>,
                        strip: strip::<L>,
                        modes: modes::<L>,
                        done: done::<L>,
                        mode_switch: mode_switch::<L>,
                        dial: dial::<L>,
                    }
                } as &'static FPTable as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   
}

pub trait ZwpTabletPadGroupV2EventListener {
    fn buttons(&mut self, sender: &mut ZwpTabletPadGroupV2, buttons: &mut ffi::Array);
    fn ring(
        &mut self,
        sender: &mut ZwpTabletPadGroupV2,
        ring: crate::Owned<crate::ZwpTabletPadRingV2>,
    );
    fn strip(
        &mut self,
        sender: &mut ZwpTabletPadGroupV2,
        strip: crate::Owned<crate::ZwpTabletPadStripV2>,
    );
    fn modes(&mut self, sender: &mut ZwpTabletPadGroupV2, modes: u32);
    fn done(&mut self, sender: &mut ZwpTabletPadGroupV2);
    fn mode_switch(&mut self, sender: &mut ZwpTabletPadGroupV2, time: u32, serial: u32, mode: u32);
    fn dial(
        &mut self,
        sender: &mut ZwpTabletPadGroupV2,
        dial: crate::Owned<crate::ZwpTabletPadDialV2>,
    );
}

static ZWP_TABLET_PAD_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_tablet_pad_v2".as_ptr(),
    version: 2,
    method_count: 2,
    methods: const {
        [
            ffi::Message {
                name: c"set_feedback".as_ptr(),
                signature: c"usu".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 8,
    events: const {
        [
            ffi::Message {
                name: c"group".as_ptr(),
                signature: c"n".as_ptr(),
                types: const { [crate::ZwpTabletPadGroupV2::DEF] }.as_ptr(),
            },
            ffi::Message {
                name: c"path".as_ptr(),
                signature: c"s".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"buttons".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"done".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"button".as_ptr(),
                signature: c"uuu".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"enter".as_ptr(),
                signature: c"uoo".as_ptr(),
                types: const {
                    [
                        core::ptr::null(),
                        crate::ZwpTabletV2::DEF,
                        crate::Surface::DEF,
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"leave".as_ptr(),
                signature: c"uo".as_ptr(),
                types: const { [core::ptr::null(), crate::Surface::DEF] }.as_ptr(),
            },
            ffi::Message {
                name: c"removed".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTabletPadV2(pub(crate) Proxy);
unsafe impl Interface for ZwpTabletPadV2 {
    const DEF: *const ffi::Interface = &ZWP_TABLET_PAD_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTabletPadV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(1);
    }
}

impl ZwpTabletPadV2 {
    pub fn set_listener<'l, L: ZwpTabletPadV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn group<L: ZwpTabletPadV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            pad_group: *mut ffi::Proxy,
        ) {
            L::group(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe {
                    crate::Owned::from_untyped_unchecked(core::ptr::NonNull::new_unchecked(
                        crate::Proxy::cast_ffi_ptr(pad_group),
                    ))
                },
            )
        }
        extern "C" fn path<L: ZwpTabletPadV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            path: *const core::ffi::c_char,
        ) {
            L::path(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { core::ffi::CStr::from_ptr(path) },
            )
        }
        extern "C" fn buttons<L: ZwpTabletPadV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            buttons: u32,
        ) {
            L::buttons(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                buttons,
            )
        }
        extern "C" fn done<L: ZwpTabletPadV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::done(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn button<L: ZwpTabletPadV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            time: u32,
            button: u32,
            state: u32,
        ) {
            L::button(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                time,
                button,
                unsafe { core::mem::transmute(state) },
            )
        }
        extern "C" fn enter<L: ZwpTabletPadV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            serial: u32,
            tablet: *mut ffi::Proxy,
            surface: *mut ffi::Proxy,
        ) {
            L::enter(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                serial,
                unsafe { &mut *(tablet as *mut _) },
                unsafe { &mut *(surface as *mut _) },
            )
        }
        extern "C" fn leave<L: ZwpTabletPadV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            serial: u32,
            surface: *mut ffi::Proxy,
        ) {
            L::leave(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                serial,
                unsafe { &mut *(surface as *mut _) },
            )
        }
        extern "C" fn removed<L: ZwpTabletPadV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::removed(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }

        #[repr(C)]
        struct FPTable {
            group: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                pad_group: *mut ffi::Proxy,
            ),
            path: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                path: *const core::ffi::c_char,
            ),
            buttons: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                buttons: u32,
            ),
            done: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            button: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                time: u32,
                button: u32,
                state: u32,
            ),
            enter: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                serial: u32,
                tablet: *mut ffi::Proxy,
                surface: *mut ffi::Proxy,
            ),
            leave: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                serial: u32,
                surface: *mut ffi::Proxy,
            ),
            removed: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        group: group::<L>,
                        path: path::<L>,
                        buttons: buttons::<L>,
                        done: done::<L>,
                        button: button::<L>,
                        enter: enter::<L>,
                        leave: leave::<L>,
                        removed: removed::<L>,
                    }
                } as &'static FPTable as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   

    #[inline]
    pub fn set_feedback(
        &self,
        button: u32,
        description: &core::ffi::CStr,
        serial: u32,
    ) -> crate::Result<()> {
        self.0.marshal_array_void(
            0,
            &mut [
                ffi::Argument { u: button },
                ffi::Argument {
                    s: description.as_ptr(),
                },
                ffi::Argument { u: serial },
            ],
        )
    }
}

pub trait ZwpTabletPadV2EventListener {
    fn group(
        &mut self,
        sender: &mut ZwpTabletPadV2,
        pad_group: crate::Owned<crate::ZwpTabletPadGroupV2>,
    );
    fn path(&mut self, sender: &mut ZwpTabletPadV2, path: &core::ffi::CStr);
    fn buttons(&mut self, sender: &mut ZwpTabletPadV2, buttons: u32);
    fn done(&mut self, sender: &mut ZwpTabletPadV2);
    fn button(
        &mut self,
        sender: &mut ZwpTabletPadV2,
        time: u32,
        button: u32,
        state: ZwpTabletPadV2ButtonState,
    );
    fn enter(
        &mut self,
        sender: &mut ZwpTabletPadV2,
        serial: u32,
        tablet: &mut crate::ZwpTabletV2,
        surface: &mut crate::Surface,
    );
    fn leave(&mut self, sender: &mut ZwpTabletPadV2, serial: u32, surface: &mut crate::Surface);
    fn removed(&mut self, sender: &mut ZwpTabletPadV2);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZwpTabletPadV2ButtonState {
    Released = 0,
    Pressed = 1,
}
impl ZwpTabletPadV2ButtonState {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

static ZWP_TABLET_PAD_DIAL_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_tablet_pad_dial_v2".as_ptr(),
    version: 2,
    method_count: 2,
    methods: const {
        [
            ffi::Message {
                name: c"set_feedback".as_ptr(),
                signature: c"su".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 2,
    events: const {
        [
            ffi::Message {
                name: c"delta".as_ptr(),
                signature: c"i".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"frame".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTabletPadDialV2(pub(crate) Proxy);
unsafe impl Interface for ZwpTabletPadDialV2 {
    const DEF: *const ffi::Interface = &ZWP_TABLET_PAD_DIAL_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTabletPadDialV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(1);
    }
}

impl ZwpTabletPadDialV2 {
    pub fn set_listener<'l, L: ZwpTabletPadDialV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn delta<L: ZwpTabletPadDialV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            value120: i32,
        ) {
            L::delta(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                value120,
            )
        }
        extern "C" fn frame<L: ZwpTabletPadDialV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            time: u32,
        ) {
            L::frame(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                time,
            )
        }

        #[repr(C)]
        struct FPTable {
            delta: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                value120: i32,
            ),
            frame:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, time: u32),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        delta: delta::<L>,
                        frame: frame::<L>,
                    }
                } as &'static FPTable as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   

    #[inline]
    pub fn set_feedback(&self, description: &core::ffi::CStr, serial: u32) -> crate::Result<()> {
        self.0.marshal_array_void(
            0,
            &mut [
                ffi::Argument {
                    s: description.as_ptr(),
                },
                ffi::Argument { u: serial },
            ],
        )
    }
}

pub trait ZwpTabletPadDialV2EventListener {
    fn delta(&mut self, sender: &mut ZwpTabletPadDialV2, value120: i32);
    fn frame(&mut self, sender: &mut ZwpTabletPadDialV2, time: u32);
}
