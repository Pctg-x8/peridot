use crate::{EventFnTable, NEWID_ARG, Owned, Proxy, ffi};

#[repr(transparent)]
pub struct ZwlrOutputManagerV1(Proxy);
unsafe impl crate::Interface for ZwlrOutputManagerV1 {
    fn def() -> &'static crate::ffi::Interface {
        Self::INTERFACE
    }
}
impl ZwlrOutputManagerV1 {
    const INTERFACE: &'static crate::ffi::Interface = &crate::interface(
        c"zwlr_output_manager_v1",
        4,
        &[
            crate::message(
                c"create_configuration",
                c"nu",
                &[ZwlrOutputConfigurationV1::INTERFACE],
            ),
            crate::message(c"stop", c"", &[]),
        ],
        &[
            crate::message(c"head", c"n", &[ZwlrOutputHeadV1::INTERFACE]),
            crate::message(c"done", c"u", &[]),
            crate::message(c"finished", c"", &[]),
        ],
    );

    pub fn set_listener<'l, L: 'l + ZwlrOutputManagerV1EventListener>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: ZwlrOutputManagerV1EventListener {
                    head(head: *mut crate::ffi::Proxy => unsafe { Owned::from_raw_ptr_unchecked(head) }),
                    done(serial: u32 => serial),
                    finished()
                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline(always)]
    pub fn create_configuration(
        &self,
        serial: u32,
    ) -> Result<Owned<ZwlrOutputConfigurationV1>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                0,
                0,
                &mut [NEWID_ARG, ffi::Argument { u: serial }],
            )?)
        })
    }

    #[inline(always)]
    pub fn stop(&self) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(1, 0, &mut [])
    }
}

pub trait ZwlrOutputManagerV1EventListener {
    fn head(&mut self, sender: &mut ZwlrOutputManagerV1, head: Owned<ZwlrOutputHeadV1>);
    fn done(&mut self, sender: &mut ZwlrOutputManagerV1, serial: u32);
    fn finished(&mut self, sender: &mut ZwlrOutputManagerV1);
}

#[repr(transparent)]
pub struct ZwlrOutputHeadV1(Proxy);
unsafe impl crate::Interface for ZwlrOutputHeadV1 {
    fn def() -> &'static crate::ffi::Interface {
        Self::INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwlrOutputHeadV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        if self.0.version() < 3 {
            // no destructor before v3
            return;
        }

        self.0.call_simple_dtor(0);
    }
}
impl ZwlrOutputHeadV1 {
    const INTERFACE: &'static crate::ffi::Interface = &crate::interface(
        c"zwlr_output_head_v1",
        4,
        &[crate::message(c"release", c"3", &[])],
        &[
            crate::message(c"name", c"s", &[]),
            crate::message(c"description", c"s", &[]),
            crate::message(c"physical_size", c"ii", &[]),
            crate::message(c"mode", c"n", &[ZwlrOutputModeV1::INTERFACE]),
            crate::message(c"enabled", c"i", &[]),
            crate::message(c"current_mode", c"o", &[ZwlrOutputModeV1::INTERFACE]),
            crate::message(c"position", c"ii", &[]),
            crate::message(c"transform", c"i", &[]),
            crate::message(c"scale", c"f", &[]),
            crate::message(c"finished", c"", &[]),
            crate::message(c"make", c"2s", &[]),
            crate::message(c"model", c"2s", &[]),
            crate::message(c"serial_number", c"2s", &[]),
            crate::message(c"adaptive_sync", c"4u", &[]),
        ],
    );

    pub fn set_listener<'l, L: 'l + ZwlrOutputHeadV1EventListener>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: ZwlrOutputHeadV1EventListener {
                    name(name: *const core::ffi::c_char => unsafe { core::ffi::CStr::from_ptr(name) }),
                    description(
                        description: *const core::ffi::c_char => unsafe { core::ffi::CStr::from_ptr(description) }
                    ),
                    physical_size(width: i32 => width, height: i32 => height),
                    mode(mode: *mut crate::ffi::Proxy => unsafe { Owned::from_raw_ptr_unchecked(mode) }),
                    enabled(enabled: i32 => enabled != 0),
                    current_mode(mode: *mut crate::ffi::Proxy => unsafe { &mut *(mode as *mut ZwlrOutputModeV1) }),
                    position(x: i32 => x, y: i32 => y),
                    transform(transform: i32 => unsafe { core::mem::transmute(transform) }),
                    scale(scale: crate::Fixed => scale),
                    finished(),
                    // version 2 additions
                    make(make: *const core::ffi::c_char => unsafe { core::ffi::CStr::from_ptr(make) }),
                    model(model: *const core::ffi::c_char => unsafe { core::ffi::CStr::from_ptr(model) }),
                    serial_number(
                        serial_number: *const core::ffi::c_char => unsafe { core::ffi::CStr::from_ptr(serial_number) }
                    ),
                    // version 4 additions
                    adaptive_sync(state: u32 => unsafe { core::mem::transmute(state) })
                }) as *const _ as _,
                listener as *mut _ as _
            )
        }
    }
}

pub trait ZwlrOutputHeadV1EventListener {
    fn name(&mut self, sender: &mut ZwlrOutputHeadV1, name: &core::ffi::CStr);
    fn description(&mut self, sender: &mut ZwlrOutputHeadV1, description: &core::ffi::CStr);
    fn physical_size(&mut self, sender: &mut ZwlrOutputHeadV1, width: i32, height: i32);
    fn mode(&mut self, sender: &mut ZwlrOutputHeadV1, mode: Owned<ZwlrOutputModeV1>);
    fn enabled(&mut self, sender: &mut ZwlrOutputHeadV1, enabled: bool);
    fn current_mode(&mut self, sender: &mut ZwlrOutputHeadV1, mode: &mut ZwlrOutputModeV1);
    fn position(&mut self, sender: &mut ZwlrOutputHeadV1, x: i32, y: i32);
    fn transform(&mut self, sender: &mut ZwlrOutputHeadV1, transform: crate::OutputTransform);
    fn scale(&mut self, sender: &mut ZwlrOutputHeadV1, scale: crate::Fixed);
    fn finished(&mut self, sender: &mut ZwlrOutputHeadV1);
    // version 2 additions
    fn make(&mut self, sender: &mut ZwlrOutputHeadV1, make: &core::ffi::CStr);
    fn model(&mut self, sender: &mut ZwlrOutputHeadV1, model: &core::ffi::CStr);
    fn serial_number(&mut self, sender: &mut ZwlrOutputHeadV1, serial_number: &core::ffi::CStr);
    // version 4 additions
    fn adaptive_sync(
        &mut self,
        sender: &mut ZwlrOutputHeadV1,
        state: ZwlrOutputHeadV1AdaptiveSyncState,
    );
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ZwlrOutputHeadV1AdaptiveSyncState {
    Disabled = 0,
    Enabled = 1,
}

#[repr(transparent)]
pub struct ZwlrOutputModeV1(Proxy);
unsafe impl crate::Interface for ZwlrOutputModeV1 {
    fn def() -> &'static crate::ffi::Interface {
        Self::INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZwlrOutputModeV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        if self.0.version() < 3 {
            // no destructor before v3
            return;
        }

        self.0.call_simple_dtor(0);
    }
}
impl ZwlrOutputModeV1 {
    const INTERFACE: &'static crate::ffi::Interface = &crate::interface(
        c"zwlr_output_mode_v1",
        3,
        &[crate::message(c"release", c"3", &[])],
        &[
            crate::message(c"size", c"ii", &[]),
            crate::message(c"refresh", c"i", &[]),
            crate::message(c"preferred", c"", &[]),
            crate::message(c"finished", c"", &[]),
        ],
    );

    pub fn set_listener<'l, L: 'l + ZwlrOutputModeV1EventListener>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: ZwlrOutputModeV1EventListener {
                    size(width: i32 => width, height: i32 => height),
                    refresh(refresh: i32 => refresh),
                    preferred(),
                    finished()
                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }
}

pub trait ZwlrOutputModeV1EventListener {
    fn size(&mut self, sender: &mut ZwlrOutputModeV1, width: i32, height: i32);
    fn refresh(&mut self, sender: &mut ZwlrOutputModeV1, refresh: i32);
    fn preferred(&mut self, sender: &mut ZwlrOutputModeV1);
    fn finished(&mut self, sender: &mut ZwlrOutputModeV1);
}

#[repr(transparent)]
pub struct ZwlrOutputConfigurationV1(Proxy);
unsafe impl crate::Interface for ZwlrOutputConfigurationV1 {
    fn def() -> &'static crate::ffi::Interface {
        Self::INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(
            name = "<ZwlrOutputConfigurationV1 as Interface>::destruct",
            skip(self)
        )
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(4);
    }
}
impl ZwlrOutputConfigurationV1 {
    const INTERFACE: &'static crate::ffi::Interface = &crate::interface(
        c"zwlr_output_configuration_v1",
        4,
        &[
            crate::message(
                c"enable_head",
                c"no",
                &[
                    ZwlrOutputConfigurationHeadV1::INTERFACE,
                    ZwlrOutputHeadV1::INTERFACE,
                ],
            ),
            crate::message(c"disable_head", c"o", &[ZwlrOutputHeadV1::INTERFACE]),
            crate::message(c"apply", c"", &[]),
            crate::message(c"test", c"", &[]),
            crate::message(c"destroy", c"", &[]),
        ],
        &[
            crate::message(c"succeeded", c"", &[]),
            crate::message(c"failed", c"", &[]),
            crate::message(c"cancelled", c"", &[]),
        ],
    );

    pub fn set_listener<'l, L: 'l + ZwlrOutputConfigurationV1EventListener>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: ZwlrOutputConfigurationV1EventListener {
                    succeeded(),
                    failed(),
                    cancelled()
                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline(always)]
    pub fn enable_head(
        &self,
        head: &ZwlrOutputHeadV1,
    ) -> Result<Owned<ZwlrOutputConfigurationHeadV1>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                0,
                0,
                &mut [NEWID_ARG, head.0.as_arg()],
            )?)
        })
    }

    #[inline(always)]
    pub fn disable_head(&self, head: &ZwlrOutputHeadV1) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(1, 0, &mut [head.0.as_arg()])
    }

    #[inline(always)]
    pub fn apply(&self) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(2, 0, &mut [])
    }

    #[inline(always)]
    pub fn test(&self) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(3, 0, &mut [])
    }
}

pub trait ZwlrOutputConfigurationV1EventListener {
    fn succeeded(&mut self, sender: &mut ZwlrOutputConfigurationV1);
    fn failed(&mut self, sender: &mut ZwlrOutputConfigurationV1);
    fn cancelled(&mut self, sender: &mut ZwlrOutputConfigurationV1);
}

#[repr(transparent)]
pub struct ZwlrOutputConfigurationHeadV1(Proxy);
unsafe impl crate::Interface for ZwlrOutputConfigurationHeadV1 {
    fn def() -> &'static crate::ffi::Interface {
        Self::INTERFACE
    }
}
impl ZwlrOutputConfigurationHeadV1 {
    const INTERFACE: &'static crate::ffi::Interface = &crate::interface(
        c"zwlr_output_configuration_head_v1",
        4,
        &[
            crate::message(c"set_mode", c"o", &[ZwlrOutputModeV1::INTERFACE]),
            crate::message(c"set_custom_mode", c"iii", &[]),
            crate::message(c"set_position", c"ii", &[]),
            crate::message(c"set_transform", c"i", &[]),
            crate::message(c"set_scale", c"f", &[]),
            crate::message(c"set_adaptive_sync", c"4u", &[]),
        ],
        &[],
    );

    #[inline(always)]
    pub fn set_mode(&self, mode: &ZwlrOutputModeV1) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(0, 0, &mut [mode.0.as_arg()])
    }

    #[inline(always)]
    pub fn set_custom_mode(
        &self,
        width: i32,
        height: i32,
        refresh: i32,
    ) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            1,
            0,
            &mut [
                ffi::Argument { i: width },
                ffi::Argument { i: height },
                ffi::Argument { i: refresh },
            ],
        )
    }

    #[inline(always)]
    pub fn set_position(&self, x: i32, y: i32) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(2, 0, &mut [ffi::Argument { i: x }, ffi::Argument { i: y }])
    }

    #[inline(always)]
    pub fn set_transform(&self, transform: crate::OutputTransform) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(3, 0, &mut [ffi::Argument { i: transform as _ }])
    }

    #[inline(always)]
    pub fn set_scale(&self, scale: crate::Fixed) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(4, 0, &mut [ffi::Argument { f: scale }])
    }

    #[inline(always)]
    pub fn set_adaptive_sync(
        &self,
        state: ZwlrOutputHeadV1AdaptiveSyncState,
    ) -> Result<(), std::io::Error> {
        assert!(self.0.version() >= 4, "version 4 or later required");

        self.0
            .marshal_array_flags_void(5, 0, &mut [ffi::Argument { u: state as _ }])
    }
}
