//! text_input_unstable_v3: Protocol for composing text
//!
//! This protocol allows compositors to act as input methods and to send text
//! to applications. A text input object is used to manage state of what are
//! typically text entry fields in the application.
//!
//! This document adheres to the RFC 2119 when using words like "must",
//! "should", "may", etc.
//!
//! Warning! The protocol described in this file is experimental and
//! backward incompatible changes may be made. Backward compatible changes
//! may be added together with the corresponding interface version bump.
//! Backward incompatible changes are done by bumping the version number in
//! the protocol and interface names and resetting the interface version.
//! Once the protocol is to be declared stable, the 'z' prefix and the
//! version number in the protocol and interface names are removed and the
//! interface version number is reset.
//!

use crate::{Interface, Proxy, ffi};

static ZWP_TEXT_INPUT_V3_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_text_input_v3".as_ptr(),
    version: 1,
    method_count: 8,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"enable".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"disable".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_surrounding_text".as_ptr(),
                signature: c"sii".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_text_change_cause".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_content_type".as_ptr(),
                signature: c"uu".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_cursor_rectangle".as_ptr(),
                signature: c"iiii".as_ptr(),
                types: const {
                    [
                        core::ptr::null(),
                        core::ptr::null(),
                        core::ptr::null(),
                        core::ptr::null(),
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"commit".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 6,
    events: const {
        [
            ffi::Message {
                name: c"enter".as_ptr(),
                signature: c"o".as_ptr(),
                types: const { [crate::Surface::DEF] }.as_ptr(),
            },
            ffi::Message {
                name: c"leave".as_ptr(),
                signature: c"o".as_ptr(),
                types: const { [crate::Surface::DEF] }.as_ptr(),
            },
            ffi::Message {
                name: c"preedit_string".as_ptr(),
                signature: c"?sii".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"commit_string".as_ptr(),
                signature: c"?s".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"delete_surrounding_text".as_ptr(),
                signature: c"uu".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"done".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTextInputV3(pub(crate) Proxy);
unsafe impl Interface for ZwpTextInputV3 {
    const DEF: *const ffi::Interface = &ZWP_TEXT_INPUT_V3_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTextInputV3 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZwpTextInputV3 {
    pub fn set_listener<'l, L: ZwpTextInputV3EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn enter<L: ZwpTextInputV3EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            surface: *mut ffi::Proxy,
        ) {
            L::enter(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { &mut *(surface as *mut _) },
            )
        }
        extern "C" fn leave<L: ZwpTextInputV3EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            surface: *mut ffi::Proxy,
        ) {
            L::leave(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { &mut *(surface as *mut _) },
            )
        }
        extern "C" fn preedit_string<L: ZwpTextInputV3EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            text: *const core::ffi::c_char,
            cursor_begin: i32,
            cursor_end: i32,
        ) {
            L::preedit_string(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                if text.is_null() {
                    None
                } else {
                    Some(unsafe { core::ffi::CStr::from_ptr(text) })
                },
                cursor_begin,
                cursor_end,
            )
        }
        extern "C" fn commit_string<L: ZwpTextInputV3EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            text: *const core::ffi::c_char,
        ) {
            L::commit_string(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                if text.is_null() {
                    None
                } else {
                    Some(unsafe { core::ffi::CStr::from_ptr(text) })
                },
            )
        }
        extern "C" fn delete_surrounding_text<L: ZwpTextInputV3EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            before_length: u32,
            after_length: u32,
        ) {
            L::delete_surrounding_text(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                before_length,
                after_length,
            )
        }
        extern "C" fn done<L: ZwpTextInputV3EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            serial: u32,
        ) {
            L::done(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                serial,
            )
        }

        #[repr(C)]
        struct FPTable {
            enter: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                surface: *mut ffi::Proxy,
            ),
            leave: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                surface: *mut ffi::Proxy,
            ),
            preedit_string: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                text: *const core::ffi::c_char,
                cursor_begin: i32,
                cursor_end: i32,
            ),
            commit_string: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                text: *const core::ffi::c_char,
            ),
            delete_surrounding_text: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                before_length: u32,
                after_length: u32,
            ),
            done:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, serial: u32),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        enter: enter::<L>,
                        leave: leave::<L>,
                        preedit_string: preedit_string::<L>,
                        commit_string: commit_string::<L>,
                        delete_surrounding_text: delete_surrounding_text::<L>,
                        done: done::<L>,
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
        unsafe { self.0.user_data() }
    }

    #[inline]
    pub fn enable(&self) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [])
    }

    #[inline]
    pub fn disable(&self) -> crate::Result<()> {
        self.0.marshal_array_void(2, &mut [])
    }

    #[inline]
    pub fn set_surrounding_text(
        &self,
        text: &core::ffi::CStr,
        cursor: i32,
        anchor: i32,
    ) -> crate::Result<()> {
        self.0.marshal_array_void(
            3,
            &mut [
                ffi::Argument { s: text.as_ptr() },
                ffi::Argument { i: cursor },
                ffi::Argument { i: anchor },
            ],
        )
    }

    #[inline]
    pub fn set_text_change_cause(&self, cause: ZwpTextInputV3ChangeCause) -> crate::Result<()> {
        self.0.marshal_array_void(4, &mut [cause.as_arg()])
    }

    #[inline]
    pub fn set_content_type(
        &self,
        hint: ZwpTextInputV3ContentHint,
        purpose: ZwpTextInputV3ContentPurpose,
    ) -> crate::Result<()> {
        self.0
            .marshal_array_void(5, &mut [hint.as_arg(), purpose.as_arg()])
    }

    #[inline]
    pub fn set_cursor_rectangle(
        &self,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
    ) -> crate::Result<()> {
        self.0.marshal_array_void(
            6,
            &mut [
                ffi::Argument { i: x },
                ffi::Argument { i: y },
                ffi::Argument { i: width },
                ffi::Argument { i: height },
            ],
        )
    }

    #[inline]
    pub fn commit(&self) -> crate::Result<()> {
        self.0.marshal_array_void(7, &mut [])
    }
}

pub trait ZwpTextInputV3EventListener {
    fn enter(&mut self, sender: &mut ZwpTextInputV3, surface: &mut crate::Surface);
    fn leave(&mut self, sender: &mut ZwpTextInputV3, surface: &mut crate::Surface);
    fn preedit_string(
        &mut self,
        sender: &mut ZwpTextInputV3,
        text: Option<&core::ffi::CStr>,
        cursor_begin: i32,
        cursor_end: i32,
    );
    fn commit_string(&mut self, sender: &mut ZwpTextInputV3, text: Option<&core::ffi::CStr>);
    fn delete_surrounding_text(
        &mut self,
        sender: &mut ZwpTextInputV3,
        before_length: u32,
        after_length: u32,
    );
    fn done(&mut self, sender: &mut ZwpTextInputV3, serial: u32);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZwpTextInputV3ChangeCause {
    InputMethod = 0,
    Other = 1,
}
impl ZwpTextInputV3ChangeCause {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

bitflags::bitflags! { #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub struct ZwpTextInputV3ContentHint : u32 {
    const None = 0;
    const Completion = 1;
    const Spellcheck = 2;
    const AutoCapitalization = 4;
    const Lowercase = 8;
    const Uppercase = 16;
    const Titlecase = 32;
    const HiddenText = 64;
    const SensitiveData = 128;
    const Latin = 256;
    const Multiline = 512;
} }
impl ZwpTextInputV3ContentHint {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: self.bits() }
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZwpTextInputV3ContentPurpose {
    Normal = 0,
    Alpha = 1,
    Digits = 2,
    Number = 3,
    Phone = 4,
    Url = 5,
    Email = 6,
    Name = 7,
    Password = 8,
    Pin = 9,
    Date = 10,
    Time = 11,
    Datetime = 12,
    Terminal = 13,
}
impl ZwpTextInputV3ContentPurpose {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

static ZWP_TEXT_INPUT_MANAGER_V3_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zwp_text_input_manager_v3".as_ptr(),
    version: 1,
    method_count: 2,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"get_text_input".as_ptr(),
                signature: c"no".as_ptr(),
                types: const { [crate::ZwpTextInputV3::DEF, crate::Seat::DEF] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 0,
    events: const { [] }.as_ptr(),
};

#[repr(transparent)]
pub struct ZwpTextInputManagerV3(pub(crate) Proxy);
unsafe impl Interface for ZwpTextInputManagerV3 {
    const DEF: *const ffi::Interface = &ZWP_TEXT_INPUT_MANAGER_V3_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwpTextInputManagerV3 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZwpTextInputManagerV3 {
    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() }
    }

    #[inline]
    pub fn get_text_input(
        &self,
        seat: &crate::Seat,
    ) -> crate::Result<crate::Owned<crate::ZwpTextInputV3>> {
        Ok(unsafe {
            crate::Owned::wrap_unchecked(
                self.0
                    .marshal_array_typed(1, &mut [crate::NEWID_ARG, seat.0.as_arg()])?,
            )
        })
    }
}
