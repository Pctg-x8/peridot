use crate::raw::{
    PW_VERSION_CORE_EVENTS, PW_VERSION_REGISTRY_EVENTS, pw_core_events, pw_core_info,
    pw_registry_events, spa_dict,
};
use core::ffi::*;

pub mod raw;
pub mod spa;

pub trait CoreEventListener {
    #[allow(unused_variables)]
    fn info(&mut self, info: *const pw_core_info) {}
    #[allow(unused_variables)]
    fn done(&mut self, id: u32, seq: c_int) {}
    #[allow(unused_variables)]
    fn ping(&mut self, id: u32, seq: c_int) {}
    #[allow(unused_variables)]
    fn error(&mut self, id: u32, seq: c_int, res: c_int, mesasge: &CStr) {}
    #[allow(unused_variables)]
    fn remove_id(&mut self, id: u32) {}
    #[allow(unused_variables)]
    fn bound_id(&mut self, id: u32, global_id: u32) {}
    #[allow(unused_variables)]
    fn add_mem(&mut self, id: u32, r#type: u32, fd: c_int, flags: u32) {}
    #[allow(unused_variables)]
    fn remove_mem(&mut self, id: u32) {}
    #[allow(unused_variables)]
    fn bound_props(&mut self, id: u32, global_id: u32, props: *const spa_dict) {}
}
pub const fn core_event_fptbl<L: CoreEventListener + 'static>() -> &'static pw_core_events {
    extern "C" fn info<L: CoreEventListener + 'static>(
        data: *mut c_void,
        info: *const pw_core_info,
    ) {
        unsafe { L::info(&mut *data.cast(), info) }
    }
    extern "C" fn done<L: CoreEventListener + 'static>(data: *mut c_void, id: u32, seq: c_int) {
        unsafe { L::done(&mut *data.cast(), id, seq) }
    }
    extern "C" fn ping<L: CoreEventListener + 'static>(data: *mut c_void, id: u32, seq: c_int) {
        unsafe { L::ping(&mut *data.cast(), id, seq) }
    }
    extern "C" fn error<L: CoreEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        seq: c_int,
        res: c_int,
        message: *const c_char,
    ) {
        unsafe { L::error(&mut *data.cast(), id, seq, res, CStr::from_ptr(message)) }
    }
    extern "C" fn remove_id<L: CoreEventListener + 'static>(data: *mut c_void, id: u32) {
        unsafe { L::remove_id(&mut *data.cast(), id) }
    }
    extern "C" fn bound_id<L: CoreEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        global_id: u32,
    ) {
        unsafe { L::bound_id(&mut *data.cast(), id, global_id) }
    }
    extern "C" fn add_mem<L: CoreEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        r#type: u32,
        fd: c_int,
        flags: u32,
    ) {
        unsafe { L::add_mem(&mut *data.cast(), id, r#type, fd, flags) }
    }
    extern "C" fn remove_mem<L: CoreEventListener + 'static>(data: *mut c_void, id: u32) {
        unsafe { L::remove_mem(&mut *data.cast(), id) }
    }
    extern "C" fn bound_props<L: CoreEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        global_id: u32,
        props: *const spa_dict,
    ) {
        unsafe { L::bound_props(&mut *data.cast(), id, global_id, props) }
    }

    &pw_core_events {
        version: PW_VERSION_CORE_EVENTS,
        info: Some(info::<L>),
        done: Some(done::<L>),
        ping: Some(ping::<L>),
        error: Some(error::<L>),
        remove_id: Some(remove_id::<L>),
        bound_id: Some(bound_id::<L>),
        add_mem: Some(add_mem::<L>),
        remove_mem: Some(remove_mem::<L>),
        bound_props: Some(bound_props::<L>),
    }
}

pub trait RegistryEventListener {
    #[allow(unused_variables)]
    fn global(
        &mut self,
        id: u32,
        permissions: u32,
        r#type: &CStr,
        version: u32,
        props: *const spa_dict,
    ) {
    }
    #[allow(unused_variables)]
    fn global_remove(&mut self, id: u32) {}
}
pub const fn registry_event_fptbl<L: RegistryEventListener + 'static>()
-> &'static pw_registry_events {
    extern "C" fn global<L: RegistryEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        permissions: u32,
        r#type: *const c_char,
        version: u32,
        props: *const spa_dict,
    ) {
        unsafe {
            L::global(
                &mut *data.cast(),
                id,
                permissions,
                CStr::from_ptr(r#type),
                version,
                props,
            )
        }
    }
    extern "C" fn global_remove<L: RegistryEventListener + 'static>(data: *mut c_void, id: u32) {
        unsafe { L::global_remove(&mut *data.cast(), id) }
    }

    &pw_registry_events {
        version: PW_VERSION_REGISTRY_EVENTS,
        global: Some(global::<L>),
        global_remove: Some(global_remove::<L>),
    }
}
