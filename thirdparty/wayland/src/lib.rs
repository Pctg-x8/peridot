#![cfg(unix)]

use core::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};
use std::{cell::UnsafeCell, os::fd::AsRawFd};

use bitflags::bitflags;
use ffi::wl_proxy_destroy;

pub mod ffi;
mod local_macros;

pub use ffi::Fixed;

const NEWID_ARG: ffi::Argument = ffi::Argument { n: 0 };
const NULLOBJ_ARG: ffi::Argument = ffi::Argument {
    o: core::ptr::null_mut(),
};

#[repr(transparent)]
pub struct Proxy(UnsafeCell<ffi::Proxy>);
impl Proxy {
    #[inline(always)]
    pub const unsafe fn from_raw_ptr_unchecked<'a>(ptr: *mut ffi::Proxy) -> &'a mut Self {
        unsafe { Self::from_raw_ref_mut(&mut *ptr) }
    }

    #[inline(always)]
    pub const unsafe fn from_raw_ref_mut<'a>(r: &'a mut ffi::Proxy) -> &'a mut Self {
        unsafe { core::mem::transmute(UnsafeCell::from_mut(r)) }
    }

    #[inline(always)]
    pub(crate) const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument {
            o: self.0.get() as _,
        }
    }

    #[inline(always)]
    pub fn version(&self) -> u32 {
        unsafe { ffi::wl_proxy_get_version(self.0.get()) }
    }

    #[inline(always)]
    pub fn display(&self) -> *mut ffi::Display {
        unsafe { ffi::wl_proxy_get_display(self.0.get()) }
    }

    /// Set a proxy's listener
    #[inline(always)]
    pub unsafe fn set_listener(
        &mut self,
        function_table: *const core::ffi::c_void,
        user_data: *mut core::ffi::c_void,
    ) -> Result<(), ()> {
        match unsafe {
            ffi::wl_proxy_add_listener(self.0.get_mut() as _, function_table, user_data)
        } {
            -1 => Err(()),
            _ => Ok(()),
        }
    }

    #[inline]
    fn marshal_array_flags(
        &self,
        opcode: u32,
        interface: &ffi::Interface,
        version: u32,
        flags: u32,
        args: &mut [ffi::Argument],
    ) -> Result<NonNull<Proxy>, std::io::Error> {
        unsafe {
            NonNull::new(ffi::wl_proxy_marshal_array_flags(
                self.0.get(),
                opcode,
                interface as *const _,
                version,
                flags,
                args.as_mut_ptr(),
            ))
            .ok_or_else(std::io::Error::last_os_error)
            .map(NonNull::cast)
        }
    }

    #[inline]
    fn marshal_array_flags_typed<T: Interface>(
        &self,
        opcode: u32,
        version: u32,
        flags: u32,
        args: &mut [ffi::Argument],
    ) -> Result<NonNull<T>, std::io::Error> {
        self.marshal_array_flags(opcode, T::DEF, version, flags, args)
            .map(|x| unsafe { T::from_proxy_ptr_unchecked(x) })
    }

    fn marshal_array_flags_void(
        &self,
        opcode: u32,
        flags: u32,
        args: &mut [ffi::Argument],
    ) -> Result<(), std::io::Error> {
        unsafe {
            // wl_proxy_marshal_array_flags without any interface will returns NULL
            ffi::wl_proxy_marshal_array_flags(
                self.0.get(),
                opcode,
                core::ptr::null(),
                self.version(),
                flags,
                if args.is_empty() {
                    core::ptr::null_mut()
                } else {
                    args.as_mut_ptr()
                },
            )
        };

        let e = unsafe { ffi::wl_display_get_error(ffi::wl_proxy_get_display(self.0.get())) };
        if e != 0 {
            Err(std::io::Error::from_raw_os_error(e))
        } else {
            Ok(())
        }
    }

    /// Calls the destructor with no arguments
    ///
    /// If any errors occured, it will be reported via tracing if enabled.
    pub(crate) fn call_simple_dtor(&mut self, opcode: u32) {
        #[cfg(feature = "tracing")]
        if let Err(e) = self.marshal_array_flags_void(opcode, ffi::MARSHAL_FLAG_DESTROY, &mut []) {
            tracing::warn!(
                reason = ?e,
                display_error = unsafe { ffi::wl_display_get_error(self.display()) },
                "Failed to call destructor"
            );
        }
        #[cfg(not(feature = "tracing"))]
        let _ = self.marshal_array_flags_void(opcode, ffi::MARSHAL_FLAG_DESTROY, &mut []);
    }
}

/// must be transparent with ffi::Proxy(or Proxy wrapper newtype)
pub unsafe trait Interface {
    const DEF: &'static ffi::Interface;

    #[inline(always)]
    unsafe fn from_proxy_ptr_unchecked(p: NonNull<Proxy>) -> NonNull<Self>
    where
        Self: Sized,
    {
        unsafe { core::mem::transmute(p) }
    }

    unsafe fn destruct(&mut self) {
        unsafe {
            wl_proxy_destroy(self as *mut _ as _);
        }
    }
}

pub struct Owned<T: Interface>(NonNull<T>);
impl<T: Interface> Drop for Owned<T> {
    fn drop(&mut self) {
        #[cfg(feature = "tracing")]
        tracing::trace!(target: "wl_drop_log", type_name = core::any::type_name::<T>(), "drop wl owned");

        unsafe {
            self.0.as_mut().destruct();
        }
    }
}
impl<T: Interface> Deref for Owned<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: Interface> DerefMut for Owned<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T: Interface> Owned<T> {
    pub const unsafe fn from_untyped_unchecked(untyped: NonNull<Proxy>) -> Self {
        Self(untyped.cast())
    }

    /// # Safety
    /// passed pointer must be a valid typed proxy reference.
    pub const unsafe fn wrap_unchecked(p: NonNull<T>) -> Self {
        Self(p)
    }

    pub const unsafe fn copy_ptr(&self) -> NonNull<T> {
        self.0
    }

    pub fn ref_eq(&self, other: &T) -> bool {
        core::ptr::addr_eq(self.0.as_ptr(), other as *const _)
    }

    pub const fn leak(self) {
        core::mem::forget(self);
    }

    pub const fn unwrap(self) -> NonNull<T> {
        let ptr = unsafe { core::ptr::read(&self.0) };
        core::mem::forget(self);

        ptr
    }
}

pub struct Display {
    ffi: NonNull<ffi::Display>,
}
impl Drop for Display {
    fn drop(&mut self) {
        unsafe { ffi::wl_display_disconnect(self.ffi.as_ptr()) }
    }
}
impl AsRawFd for Display {
    #[inline(always)]
    fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
        unsafe { ffi::wl_display_get_fd(self.ffi.as_ptr()) }
    }
}
impl Display {
    #[inline]
    pub fn connect() -> Option<Self> {
        let ffi = NonNull::new(unsafe { ffi::wl_display_connect(core::ptr::null()) })?;

        Some(Self { ffi })
    }

    pub const fn as_raw(&self) -> *mut ffi::Display {
        self.ffi.as_ptr()
    }

    #[inline]
    pub fn get_registry(&self) -> Result<Owned<Registry>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(
                Proxy::from_raw_ptr_unchecked(self.ffi.as_ptr() as _).marshal_array_flags_typed(
                    1,
                    ffi::wl_proxy_get_version(self.ffi.as_ptr() as _),
                    0,
                    &mut [NEWID_ARG],
                )?,
            )
        })
    }

    #[inline]
    pub fn roundtrip(&self) -> Result<u32, std::io::Error> {
        match unsafe { ffi::wl_display_roundtrip(self.ffi.as_ptr()) } {
            -1 => Err(std::io::Error::last_os_error()),
            r => Ok(r.cast_unsigned()),
        }
    }

    #[inline]
    pub fn error(&self) -> Option<core::ffi::c_int> {
        let r = unsafe { ffi::wl_display_get_error(self.ffi.as_ptr()) };

        if r == 0 { None } else { Some(r) }
    }

    pub fn protocol_error(&self) -> (*const ffi::Interface, u32, u32) {
        let mut interface = core::mem::MaybeUninit::uninit();
        let mut id = core::mem::MaybeUninit::uninit();
        let code = unsafe {
            ffi::wl_display_get_protocol_error(
                self.ffi.as_ptr(),
                interface.as_mut_ptr(),
                id.as_mut_ptr(),
            )
        };

        (
            unsafe { interface.assume_init() },
            unsafe { id.assume_init() },
            code,
        )
    }

    #[inline]
    pub fn flush(&self) -> Result<u32, std::io::Error> {
        match unsafe { ffi::wl_display_flush(self.ffi.as_ptr()) } {
            -1 => Err(std::io::Error::last_os_error()),
            r => Ok(r.cast_unsigned()),
        }
    }

    #[inline]
    pub fn dispatch_pending(&self) -> Result<u32, std::io::Error> {
        match unsafe { ffi::wl_display_dispatch_pending(self.ffi.as_ptr()) } {
            -1 => Err(std::io::Error::last_os_error()),
            r => Ok(r.cast_unsigned()),
        }
    }

    #[inline]
    pub fn prepare_read(&self) -> Result<(), std::io::Error> {
        match unsafe { ffi::wl_display_prepare_read(self.ffi.as_ptr()) } {
            -1 => Err(std::io::Error::last_os_error()),
            _ => Ok(()),
        }
    }

    #[inline]
    pub fn cancel_read(&self) {
        unsafe { ffi::wl_display_cancel_read(self.ffi.as_ptr()) }
    }

    #[inline]
    pub fn read_events(&self) -> Result<(), std::io::Error> {
        match unsafe { ffi::wl_display_read_events(self.ffi.as_ptr()) } {
            -1 => Err(std::io::Error::last_os_error()),
            _ => Ok(()),
        }
    }
}

#[repr(transparent)]
pub struct Registry(Proxy);
unsafe impl Interface for Registry {
    const DEF: &'static ffi::Interface = unsafe { &wl_registry_interface };
}
impl Registry {
    pub fn set_listener<'l, L: RegistryListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: RegistryListener {
                    global(
                        name: u32 => name,
                        interface: *const core::ffi::c_char => unsafe { core::ffi::CStr::from_ptr(interface) },
                        version: u32 => version
                    ),
                    global_remove(name: u32 => name)
                }) as *const _ as _,
                listener as *mut _ as _
            )
        }
    }

    #[inline]
    pub fn bind<I: Interface>(&self, name: u32, version: u32) -> Result<Owned<I>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                0,
                version,
                0,
                &mut [
                    ffi::Argument { u: name },
                    // dynamically-typed new id
                    ffi::Argument { s: I::DEF.name },
                    ffi::Argument { u: version },
                    NEWID_ARG,
                ],
            )?)
        })
    }
}

pub trait RegistryListener {
    fn global(
        &mut self,
        registry: &mut Registry,
        name: u32,
        interface: &core::ffi::CStr,
        version: u32,
    );
    fn global_remove(&mut self, registry: &mut Registry, name: u32);
}

#[repr(transparent)]
pub struct Callback(Proxy);
unsafe impl Interface for Callback {
    const DEF: &'static ffi::Interface = unsafe { &wl_callback_interface };
}
impl Callback {
    pub fn set_listener<'l, L: CallbackEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: CallbackEventListener {
                    done(callback_data: u32 => callback_data)
                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }
}

pub trait CallbackEventListener {
    fn done(&mut self, callback: &mut Callback, data: u32);
}

#[repr(transparent)]
pub struct Compositor(Proxy);
unsafe impl Interface for Compositor {
    const DEF: &'static ffi::Interface = unsafe { &wl_compositor_interface };
}
impl Compositor {
    #[inline]
    pub fn create_surface(&self) -> Result<Owned<Surface>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                0,
                self.0.version(),
                0,
                &mut [NEWID_ARG],
            )?)
        })
    }

    #[inline]
    pub fn create_region(&self) -> Result<Owned<Region>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                1,
                self.0.version(),
                0,
                &mut [NEWID_ARG],
            )?)
        })
    }
}

#[repr(transparent)]
pub struct Surface(Proxy);
unsafe impl Interface for Surface {
    const DEF: &'static ffi::Interface = unsafe { &wl_surface_interface };
}
impl Surface {
    pub const fn as_raw(&mut self) -> *mut ffi::Proxy {
        &mut self.0 as *mut _ as _
    }

    #[inline]
    pub fn attach(&self, buffer: Option<&Buffer>, x: i32, y: i32) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            1,
            0,
            &mut [
                buffer.map_or(NULLOBJ_ARG, |x| x.0.as_arg()),
                ffi::Argument { i: x },
                ffi::Argument { i: y },
            ],
        )
    }

    #[inline]
    pub fn damage(&self, x: i32, y: i32, width: i32, height: i32) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            2,
            0,
            &mut [
                ffi::Argument { i: x },
                ffi::Argument { i: y },
                ffi::Argument { i: width },
                ffi::Argument { i: height },
            ],
        )
    }

    #[inline]
    pub fn frame(&self) -> Result<Owned<Callback>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                3,
                self.0.version(),
                0,
                &mut [NEWID_ARG],
            )?)
        })
    }

    #[inline]
    pub fn set_input_region(&self, region: Option<&Region>) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(5, 0, &mut [region.map_or(NULLOBJ_ARG, |x| x.0.as_arg())])
    }

    #[inline]
    pub fn commit(&self) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(6, 0, &mut [])
    }

    #[inline]
    pub fn set_buffer_transform(&self, transform: OutputTransform) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(7, 0, &mut [ffi::Argument { i: transform as _ }])
    }

    #[inline]
    pub fn set_buffer_scale(&self, scale: i32) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(8, 0, &mut [ffi::Argument { i: scale }])
    }

    pub fn set_listener<'l, L: SurfaceEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: SurfaceEventListener {
                    enter(output: *mut ffi::Proxy => unsafe { core::mem::transmute(&mut *output) }),
                    leave(output: *mut ffi::Proxy => unsafe { core::mem::transmute(&mut *output) }),
                    preferred_buffer_scale(factor: i32 => factor),
                    preferred_buffer_transform(transform: u32 => transform)
                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }
}

pub trait SurfaceEventListener {
    fn enter(&mut self, surface: &mut Surface, output: &mut Output);
    fn leave(&mut self, surface: &mut Surface, output: &mut Output);
    // --- version 6 additions ---
    fn preferred_buffer_scale(&mut self, surface: &mut Surface, factor: i32);
    fn preferred_buffer_transform(&mut self, surface: &mut Surface, transform: u32);
}

#[repr(transparent)]
pub struct Subcompositor(Proxy);
unsafe impl Interface for Subcompositor {
    const DEF: &'static ffi::Interface = unsafe { &wl_subcompositor_interface };

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<Subcompositor as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl Subcompositor {
    #[inline]
    pub fn get_subsurface(
        &self,
        surface: &Surface,
        parent: &Surface,
    ) -> Result<Owned<Subsurface>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                1,
                self.0.version(),
                0,
                &mut [NEWID_ARG, surface.0.as_arg(), parent.0.as_arg()],
            )?)
        })
    }
}

#[repr(transparent)]
pub struct Subsurface(Proxy);
unsafe impl Interface for Subsurface {
    const DEF: &'static ffi::Interface = unsafe { &wl_subsurface_interface };

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<Subsurface as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl Subsurface {
    #[inline]
    pub fn set_position(&self, x: i32, y: i32) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(1, 0, &mut [ffi::Argument { i: x }, ffi::Argument { i: y }])
    }

    #[inline]
    pub fn place_below(&self, sibling: &Surface) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(3, 0, &mut [sibling.0.as_arg()])
    }
}

#[repr(transparent)]
pub struct Shm(Proxy);
unsafe impl Interface for Shm {
    const DEF: &'static ffi::Interface = unsafe { &wl_shm_interface };

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<Shm as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(1);
    }
}
impl Shm {
    #[inline]
    pub fn create_pool_raw(
        &self,
        fd: std::os::fd::RawFd,
        size: i32,
    ) -> Result<Owned<ShmPool>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                0,
                self.0.version(),
                0,
                &mut [
                    NEWID_ARG,
                    ffi::Argument { h: fd.as_raw_fd() },
                    ffi::Argument { i: size },
                ],
            )?)
        })
    }

    #[inline(always)]
    pub fn create_pool(
        &self,
        fd: &impl AsRawFd,
        size: i32,
    ) -> Result<Owned<ShmPool>, std::io::Error> {
        self.create_pool_raw(fd.as_raw_fd(), size)
    }
}

#[repr(u32)]
#[derive(Clone, Copy)]
pub enum ShmFormat {
    ARGB8888 = 0,
    XRGB8888 = 1,
}

#[repr(transparent)]
pub struct ShmPool(Proxy);
unsafe impl Interface for ShmPool {
    const DEF: &'static ffi::Interface = unsafe { &wl_shm_pool_interface };

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ShmPool as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(1);
    }
}
impl ShmPool {
    #[inline]
    pub fn create_buffer(
        &self,
        offset: i32,
        width: i32,
        height: i32,
        stride: i32,
        format: ShmFormat,
    ) -> Result<Owned<Buffer>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                0,
                self.0.version(),
                0,
                &mut [
                    NEWID_ARG,
                    ffi::Argument { i: offset },
                    ffi::Argument { i: width },
                    ffi::Argument { i: height },
                    ffi::Argument { i: stride },
                    ffi::Argument { u: format as _ },
                ],
            )?)
        })
    }

    #[inline]
    pub fn resize(&self, size: i32) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(2, 0, &mut [ffi::Argument { i: size }])
    }
}

#[repr(transparent)]
pub struct Buffer(Proxy);
unsafe impl Interface for Buffer {
    const DEF: &'static ffi::Interface = unsafe { &wl_buffer_interface };

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<Buffer as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

#[repr(transparent)]
pub struct Region(Proxy);
unsafe impl Interface for Region {
    const DEF: &'static ffi::Interface = unsafe { &wl_region_interface };

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<Region as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl Region {
    #[inline]
    pub fn add(&self, x: i32, y: i32, width: i32, height: i32) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            1,
            0,
            &mut [
                ffi::Argument { i: x },
                ffi::Argument { i: y },
                ffi::Argument { i: width },
                ffi::Argument { i: height },
            ],
        )
    }
}

#[repr(transparent)]
pub struct Seat(Proxy);
unsafe impl Interface for Seat {
    const DEF: &'static ffi::Interface = unsafe { &wl_seat_interface };

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<Seat as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        if self.0.version() < 5 {
            // no destruction method implemented
            return;
        }

        self.0.call_simple_dtor(3);
    }
}
impl Seat {
    pub const fn as_raw(&mut self) -> *mut ffi::Proxy {
        &mut self.0 as *mut _ as _
    }

    #[inline]
    pub fn get_pointer(&self) -> Result<Owned<Pointer>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                0,
                self.0.version(),
                0,
                &mut [NEWID_ARG],
            )?)
        })
    }

    // v5
    #[inline]
    pub unsafe fn destroy(&self) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(3, 0, &mut [])
    }

    pub fn set_listener<'l, L: SeatEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: SeatEventListener {
                    capabilities(capabilities: u32 => capabilities),
                    name(name: *const core::ffi::c_char => unsafe { core::ffi::CStr::from_ptr(name) })
                }) as *const _ as _,
                listener as *mut _ as _
            )
        }
    }
}

pub trait SeatEventListener {
    fn capabilities(&mut self, seat: &mut Seat, capabilities: u32);
    // v2
    fn name(&mut self, seat: &mut Seat, name: &core::ffi::CStr);
}

#[repr(transparent)]
pub struct Pointer(Proxy);
unsafe impl Interface for Pointer {
    const DEF: &'static ffi::Interface = unsafe { &wl_pointer_interface };
}
impl Pointer {
    pub fn set_listener<'l, L: PointerEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: PointerEventListener {
                    enter(
                        serial: u32 => serial,
                        surface: *mut ffi::Proxy => unsafe { core::mem::transmute(&mut *surface) },
                        surface_x: Fixed => surface_x,
                        surface_y: Fixed => surface_y
                    ),
                    leave(
                        serial: u32 => serial,
                        surface: *mut ffi::Proxy => unsafe { core::mem::transmute(&mut *surface) }
                    ),
                    motion(time: u32 => time, surface_x: Fixed => surface_x, surface_y: Fixed => surface_y),
                    button(
                        serial: u32 => serial,
                        time: u32 => time,
                        button: u32 => button,
                        state: PointerButtonState => state
                    ),
                    axis(time: u32 => time, axis: u32 => axis, value: Fixed => value),
                    frame(),
                    axis_source(axis_source: u32 => axis_source),
                    axis_stop(time: u32 => time, axis: u32 => axis),
                    axis_discrete(axis: u32 => axis, discrete: i32 => discrete),
                    axis_value120(axis: u32 => axis, value120: i32 => value120),
                    axis_relative_direction(axis: u32 => axis, direction: u32 => direction)
                }) as *const _ as _,
                listener as *mut _ as _
            )
        }
    }
}

pub trait PointerEventListener {
    fn enter(
        &mut self,
        pointer: &mut Pointer,
        serial: u32,
        surface: &mut Surface,
        surface_x: Fixed,
        surface_y: Fixed,
    );
    fn leave(&mut self, pointer: &mut Pointer, serial: u32, surface: &mut Surface);
    fn motion(&mut self, pointer: &mut Pointer, time: u32, surface_x: Fixed, surface_y: Fixed);
    fn button(
        &mut self,
        pointer: &mut Pointer,
        serial: u32,
        time: u32,
        button: u32,
        state: PointerButtonState,
    );
    fn axis(&mut self, pointer: &mut Pointer, time: u32, axis: u32, value: Fixed);
    // v5
    fn frame(&mut self, pointer: &mut Pointer);
    fn axis_source(&mut self, pointer: &mut Pointer, axis_source: u32);
    fn axis_stop(&mut self, pointer: &mut Pointer, time: u32, axis: u32);
    fn axis_discrete(&mut self, pointer: &mut Pointer, axis: u32, discrete: i32);
    // v8
    fn axis_value120(&mut self, pointer: &mut Pointer, axis: u32, value120: i32);
    // v9
    fn axis_relative_direction(&mut self, pointer: &mut Pointer, axis: u32, direction: u32);
}

#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum PointerButtonState {
    Released = 0,
    Pressed = 1,
}

#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum OutputTransform {
    Normal = 0,
    Rot90 = 1,
    Rot180 = 2,
    Rot270 = 3,
    Flipped = 4,
    Flipped90 = 5,
    Flipped180 = 6,
    Flipped270 = 7,
}

#[repr(transparent)]
pub struct Output(Proxy);
unsafe impl Interface for Output {
    const DEF: &'static ffi::Interface = unsafe { &wl_output_interface };
}

// pub trait OutputEventListener {
//     fn geometry(&mut self, output: &mut Output, x: i32, y: i32, physical_width: i32, physical_height: i32, subpixel: i32, make: &core::ffi::CStr, model: &core::ffi::CStr, transform: i32);
//     fn mode(&mut self, output: &mut Output, flags: u32, width: i32, height: i32, refresh: i32);
//     // -- version 2 additions ---
//     fn done(&mut self, output: &mut Output);
//     fn scale(&mut self, output: &mut Output, factor: i32);
// }

#[repr(transparent)]
pub struct DataOffer(Proxy);
unsafe impl Interface for DataOffer {
    const DEF: &'static ffi::Interface = unsafe { &wl_data_offer_interface };

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<DataOffer as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(2);
    }
}
impl DataOffer {
    #[inline]
    pub fn accept(
        &self,
        serial: u32,
        mime_type: Option<&core::ffi::CStr>,
    ) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            0,
            0,
            &mut [
                ffi::Argument { u: serial },
                ffi::Argument {
                    s: mime_type.map_or_else(core::ptr::null, core::ffi::CStr::as_ptr),
                },
            ],
        )
    }

    #[inline]
    pub fn receive(
        &self,
        mime_type: &core::ffi::CStr,
        fd: &(impl AsRawFd + ?Sized),
    ) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            1,
            0,
            &mut [
                ffi::Argument {
                    s: mime_type.as_ptr(),
                },
                ffi::Argument { h: fd.as_raw_fd() },
            ],
        )
    }

    #[inline]
    pub fn finish(&self) -> Result<(), std::io::Error> {
        assert!(self.0.version() >= 3, "version 3 required");

        self.0.marshal_array_flags_void(3, 0, &mut [])
    }

    #[inline]
    pub fn set_actions(
        &self,
        dnd_actions: DataDeviceManagerDndAction,
        preferred_action: DataDeviceManagerDndAction,
    ) -> Result<(), std::io::Error> {
        assert!(self.0.version() >= 3, "version 3 required");

        self.0.marshal_array_flags_void(
            4,
            0,
            &mut [
                ffi::Argument {
                    u: dnd_actions.bits(),
                },
                ffi::Argument {
                    u: preferred_action.bits(),
                },
            ],
        )
    }

    pub fn set_listener<'l, L: DataOfferEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable! {
                    for L: DataOfferEventListener {
                        offer(
                            mime_type: *const core::ffi::c_char => unsafe { core::ffi::CStr::from_ptr(mime_type) }
                        ),
                        source_actions(
                            source_actions: u32 => DataDeviceManagerDndAction::from_bits_retain(source_actions)
                        ),
                        action(dnd_action: u32 => DataDeviceManagerDndAction::from_bits_retain(dnd_action))
                    }
                } as *const _ as _,
                listener as *mut _ as _
            )
        }
    }
}

pub trait DataOfferEventListener {
    fn offer(&mut self, sender: &mut DataOffer, mime_type: &core::ffi::CStr);
    /// since version 3
    fn source_actions(
        &mut self,
        sender: &mut DataOffer,
        source_actions: DataDeviceManagerDndAction,
    );
    /// since version 3
    fn action(&mut self, sender: &mut DataOffer, dnd_action: DataDeviceManagerDndAction);
}

#[repr(transparent)]
pub struct DataSource(Proxy);
unsafe impl Interface for DataSource {
    const DEF: &'static ffi::Interface = unsafe { &wl_data_source_interface };

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<DataSource as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(1);
    }
}
impl DataSource {
    #[inline]
    pub fn offer(&self, mime_type: &core::ffi::CStr) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            0,
            0,
            &mut [ffi::Argument {
                s: mime_type.as_ptr(),
            }],
        )
    }

    #[inline]
    pub fn set_actions(
        &self,
        dnd_actions: DataDeviceManagerDndAction,
    ) -> Result<(), std::io::Error> {
        assert!(self.0.version() >= 3, "version 3 required");

        self.0.marshal_array_flags_void(
            2,
            0,
            &mut [ffi::Argument {
                u: dnd_actions.bits(),
            }],
        )
    }

    pub fn add_listener<'l, L: DataSourceEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable! {
                    for L: DataSourceEventListener {
                        target(
                            mime_type: *const core::ffi::c_char => if mime_type.is_null() {
                                None
                            } else {
                                Some(unsafe { core::ffi::CStr::from_ptr(mime_type) })
                            }
                        ),
                        send(
                            mime_type: *const core::ffi::c_char => unsafe { core::ffi::CStr::from_ptr(mime_type) },
                            fd: core::ffi::c_int => fd
                        ),
                        cancelled(),
                        dnd_drop_performed(),
                        dnd_finished(),
                        action(
                            dnd_action: u32 => DataDeviceManagerDndAction::from_bits_retain(dnd_action)
                        )
                    }
                } as *const _ as _,
                listener as *mut _ as _
            )
        }
    }
}

pub trait DataSourceEventListener {
    fn target(&mut self, sender: &mut DataSource, mime_type: Option<&core::ffi::CStr>);
    fn send(
        &mut self,
        sender: &mut DataSource,
        mime_type: &core::ffi::CStr,
        fd: std::os::fd::RawFd,
    );
    fn cancelled(&mut self, sender: &mut DataSource);
    /// since version 3
    fn dnd_drop_performed(&mut self, sender: &mut DataSource);
    /// since version 3
    fn dnd_finished(&mut self, sender: &mut DataSource);
    /// since version 3
    fn action(&mut self, sender: &mut DataSource, dnd_action: DataDeviceManagerDndAction);
}

#[repr(transparent)]
pub struct DataDevice(Proxy);
unsafe impl Interface for DataDevice {
    const DEF: &'static ffi::Interface = unsafe { &wl_data_device_interface };

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<DataDevice as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        if self.0.version() < 2 {
            // no destructor
            return;
        }

        self.0.call_simple_dtor(2);
    }
}
impl DataDevice {
    pub fn display(&self) -> *mut ffi::Display {
        self.0.display()
    }

    #[inline]
    pub fn start_drag(
        &self,
        source: Option<&DataSource>,
        origin: &Surface,
        icon: Option<&Surface>,
        serial: u32,
    ) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            0,
            0,
            &mut [
                source.map_or(NULLOBJ_ARG, |x| x.0.as_arg()),
                origin.0.as_arg(),
                icon.map_or(NULLOBJ_ARG, |x| x.0.as_arg()),
                ffi::Argument { u: serial },
            ],
        )
    }

    #[inline]
    pub fn set_selection(
        &self,
        source: Option<&DataSource>,
        serial: u32,
    ) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            1,
            0,
            &mut [
                source.map_or(NULLOBJ_ARG, |x| x.0.as_arg()),
                ffi::Argument { u: serial },
            ],
        )
    }

    pub fn set_listener<'l, L: DataDeviceEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable! {
                    for L: DataDeviceEventListener {
                        data_offer(
                            id: *mut ffi::Proxy => unsafe {
                                Owned::from_untyped_unchecked(NonNull::new_unchecked(id as _))
                            }
                        ),
                        enter(
                            serial: u32 => serial,
                            surface: *mut ffi::Proxy => unsafe { &*(surface as *mut _) },
                            x: Fixed => x,
                            y: Fixed => y,
                            id: *mut ffi::Proxy => if id.is_null() { None } else { Some(unsafe { &*(id as *mut _) }) }
                        ),
                        leave(),
                        motion(time: u32 => time, x: Fixed => x, y: Fixed => y),
                        drop(),
                        selection(
                            id: *mut ffi::Proxy => if id.is_null() {
                                None
                            } else {
                                Some(unsafe { &*(id as *mut _) })
                            }
                        )
                    }
                } as *const _ as _,
                listener as *mut _ as _
            )
        }
    }
}

pub trait DataDeviceEventListener {
    fn data_offer(&mut self, sender: &mut DataDevice, id: Owned<DataOffer>);
    fn enter(
        &mut self,
        sender: &mut DataDevice,
        serial: u32,
        surface: &Surface,
        x: Fixed,
        y: Fixed,
        id: Option<&DataOffer>,
    );
    fn leave(&mut self, sender: &mut DataDevice);
    fn motion(&mut self, sender: &mut DataDevice, time: u32, x: Fixed, y: Fixed);
    fn drop(&mut self, sender: &mut DataDevice);
    fn selection(&mut self, sender: &mut DataDevice, id: Option<&DataOffer>);
}

#[repr(transparent)]
pub struct DataDeviceManager(Proxy);
unsafe impl Interface for DataDeviceManager {
    const DEF: &'static ffi::Interface = unsafe { &wl_data_device_manager_interface };
}
impl DataDeviceManager {
    #[inline]
    pub fn create_data_source(&self) -> Result<Owned<DataSource>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                0,
                self.0.version(),
                0,
                &mut [NEWID_ARG],
            )?)
        })
    }

    #[inline]
    pub fn get_data_device(&self, seat: &Seat) -> Result<Owned<DataDevice>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                1,
                self.0.version(),
                0,
                &mut [NEWID_ARG, seat.0.as_arg()],
            )?)
        })
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct DataDeviceManagerDndAction : u32 {
        const NONE = 0;
        const COPY = 1;
        const MOVE = 2;
        const ASK = 4;
    }
}

#[link(name = "wayland-client")]
unsafe extern "C" {
    static wl_registry_interface: ffi::Interface;
    static wl_compositor_interface: ffi::Interface;
    static wl_surface_interface: ffi::Interface;
    static wl_subcompositor_interface: ffi::Interface;
    static wl_subsurface_interface: ffi::Interface;
    static wl_shm_interface: ffi::Interface;
    static wl_shm_pool_interface: ffi::Interface;
    static wl_buffer_interface: ffi::Interface;
    static wl_region_interface: ffi::Interface;
    static wl_seat_interface: ffi::Interface;
    static wl_output_interface: ffi::Interface;
    static wl_callback_interface: ffi::Interface;
    static wl_pointer_interface: ffi::Interface;
    static wl_data_device_manager_interface: ffi::Interface;
    static wl_data_device_interface: ffi::Interface;
    static wl_data_source_interface: ffi::Interface;
    static wl_data_offer_interface: ffi::Interface;
}

#[allow(dead_code)]
const fn message(
    name: &'static core::ffi::CStr,
    signature: &'static core::ffi::CStr,
    types: &'static [*const ffi::Interface],
) -> ffi::Message {
    ffi::Message {
        name: name.as_ptr(),
        signature: signature.as_ptr(),
        types: types.as_ptr(),
    }
}

#[allow(dead_code)]
const fn interface(
    name: &'static core::ffi::CStr,
    version: core::ffi::c_int,
    methods: &'static [ffi::Message],
    events: &'static [ffi::Message],
) -> ffi::Interface {
    ffi::Interface {
        name: name.as_ptr(),
        version,
        method_count: methods.len() as _,
        methods: methods.as_ptr(),
        event_count: events.len() as _,
        events: events.as_ptr(),
    }
}

macro_rules! Ext {
    ($name: literal, $modname: ident) => {
        #[cfg(feature = $name)]
        mod $modname;
        #[cfg(feature = $name)]
        pub use self::$modname::*;
    };
}

// stable
Ext!("viewporter", viewporter);
Ext!("xdg-shell", xdg_shell);

// staging
Ext!("fractional-scale-v1", fractional_scale);
Ext!("cursor-shape-v1", cursor_shape);

// unstable
Ext!("xdg-decoration-unstable-v1", xdg_decoration);
Ext!("xdg-foreign-unstable-v2", xdg_foreign);

// external
Ext!("gtk-shell", gtk_shell);
