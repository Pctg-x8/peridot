use pipewire::raw::*;

fn main() {
    unsafe {
        pw_init(core::ptr::null_mut(), core::ptr::null_mut());

        let ml = pw_main_loop_new(core::ptr::null());
        let context = pw_context_new(pw_main_loop_get_loop(ml), core::ptr::null_mut(), 0);
        let core = pw_context_connect(context, core::ptr::null_mut(), 0);
        let registry = ((*(*core.cast::<spa_interface>())
            .cb
            .funcs
            .cast::<pw_core_methods>())
        .get_registry
        .unwrap_unchecked())(
            (*core.cast::<spa_interface>()).cb.data,
            PW_VERSION_REGISTRY,
            0,
        );
        let mut registry_listener = core::mem::MaybeUninit::<spa_hook>::zeroed();
        ((*(*registry.cast::<spa_interface>())
            .cb
            .funcs
            .cast::<pw_registry_methods>())
        .add_listener
        .unwrap_unchecked())(
            (*registry.cast::<spa_interface>()).cb.data,
            registry_listener.as_mut_ptr(),
            REGISTRY_EVENTS,
            registry.cast(),
        );

        pw_main_loop_run(ml);
    }
}

static REGISTRY_EVENTS: &pw_registry_events = &pw_registry_events {
    version: PW_VERSION_REGISTRY_EVENTS,
    global: Some(registry_global),
    global_remove: Some(registry_global_remove),
};

extern "C" fn registry_global(
    data: *mut core::ffi::c_void,
    id: u32,
    permissions: u32,
    r#type: *const core::ffi::c_char,
    version: u32,
    props: *const spa_dict,
) {
    let r#type = unsafe { core::ffi::CStr::from_ptr(r#type) };

    println!("registry global: {id} {type:?} {version} {permissions:04o}");

    if r#type == c"PipeWire:Interface:Device" {
        unsafe {
            let registry = data.cast::<pw_registry>();
            let device = ((*(*registry.cast::<spa_interface>())
                .cb
                .funcs
                .cast::<pw_registry_methods>())
            .bind
            .unwrap_unchecked())(
                (*registry.cast::<spa_interface>()).cb.data,
                id,
                r#type.as_ptr(),
                PW_VERSION_DEVICE,
                0,
            )
            .cast::<pw_device>();
            let device_listener = Box::leak(Box::new(core::mem::MaybeUninit::zeroed()));
            ((*(*device.cast::<spa_interface>())
                .cb
                .funcs
                .cast::<pw_device_methods>())
            .add_listener
            .unwrap_unchecked())(
                (*device.cast::<spa_interface>()).cb.data,
                device_listener.as_mut_ptr(),
                DEVICE_EVENT,
                device.cast(),
            );
        }
    }
}
extern "C" fn registry_global_remove(data: *mut core::ffi::c_void, id: u32) {
    println!("global remove");
}

static DEVICE_EVENT: &pw_device_events = &pw_device_events {
    version: PW_VERSION_DEVICE_EVENTS,
    info: Some(device_info),
    param: Some(device_param),
};

extern "C" fn device_info(data: *mut core::ffi::c_void, info: *const pw_device_info) {
    let info = unsafe { &*info };

    println!("device info: {} {}", info.id, info.n_params);
    let mut reply_seq = 0;
    for n in 0..info.n_params {
        let param = unsafe { &*info.params.add(n as usize) };
        println!("* param {} 0x{:x}", param.id, param.flags);

        unsafe {
            let device = data.cast::<pw_device>();
            ((*(*device.cast::<spa_interface>())
                .cb
                .funcs
                .cast::<pw_device_methods>())
            .enum_params
            .unwrap_unchecked())(
                (*device.cast::<spa_interface>()).cb.data,
                reply_seq,
                param.id,
                0,
                u32::MAX,
                core::ptr::null(),
            );
        }

        reply_seq += 1;
    }
}

extern "C" fn device_param(
    data: *mut core::ffi::c_void,
    seq: core::ffi::c_int,
    id: u32,
    index: u32,
    next: u32,
    param: *const spa_pod,
) {
    println!("device param {seq} {id} {index} {next}");
    let first_pod = unsafe { &*param };
    if first_pod.r#type == SPA_TYPE_Object {
        let first_pod = unsafe { core::mem::transmute::<&spa_pod, &spa_pod_object>(first_pod) };

        if first_pod.body.r#type == SPA_TYPE_OBJECT_ParamProfile {
            for param in SPAPODObjectPropsIterator::new(first_pod) {
                if param.key == SPA_PARAM_PROFILE_index {
                    let index = spa_assert_int(&param.value).value;
                    println!("  * index = {index}");
                } else if param.key == SPA_PARAM_PROFILE_name {
                    let name = spa_pod_string_get_value(spa_assert_string(&param.value));
                    println!("  * name = {name:?}");
                } else if param.key == SPA_PARAM_PROFILE_description {
                    let description = spa_pod_string_get_value(spa_assert_string(&param.value));
                    println!("  * description = {description:?}");
                } else if param.key == SPA_PARAM_PROFILE_priority {
                    let priority = spa_assert_int(&param.value).value;
                    println!("  * Priority = {priority}");
                } else if param.key == SPA_PARAM_PROFILE_available {
                    let available = spa_assert_id(&param.value).value;
                    println!("  * available = {available}");
                } else if param.key == SPA_PARAM_PROFILE_classes {
                    assert_eq!(param.value.r#type, SPA_TYPE_Struct);
                    let mut members_iter = SPAPODStructMemberIterator::new(unsafe {
                        core::mem::transmute::<&spa_pod, &spa_pod_struct>(&param.value)
                    });

                    let item_count = spa_assert_int(members_iter.next().unwrap()).value;
                    for _ in 0..item_count {
                        let mut entry_members_iter = SPAPODStructMemberIterator::new(
                            spa_assert_struct(members_iter.next().unwrap()),
                        );

                        let class_name = spa_pod_string_get_value(spa_assert_string(
                            entry_members_iter.next().unwrap(),
                        ));
                        let node_count = spa_assert_int(entry_members_iter.next().unwrap()).value;
                        let property = spa_pod_string_get_value(spa_assert_string(
                            entry_members_iter.next().unwrap(),
                        ));
                        let device_indices = SPAPODArrayIterator::new(spa_assert_array(
                            entry_members_iter.next().unwrap(),
                        ))
                        .copied()
                        .collect::<Vec<i32>>();

                        println!(
                            "  * classes = {class_name:?} {node_count} {property:?} {device_indices:?}"
                        );
                    }
                } else if param.key == SPA_PARAM_PROFILE_save {
                    let save = spa_assert_bool(&param.value).value;
                    println!("  * save = {save}");
                } else {
                    println!(
                        "  * ParamProfile prop: {} {} {}",
                        param.key, param.value.r#type, param.value.size
                    );
                }
            }
        } else if first_pod.body.r#type == SPA_TYPE_OBJECT_ParamRoute {
            for param in SPAPODObjectPropsIterator::new(first_pod) {
                if param.key == SPA_PARAM_ROUTE_index {
                    let index = spa_assert_int(&param.value).value;
                    println!("  * index = {index}");
                } else if param.key == SPA_PARAM_ROUTE_direction {
                    let direction = spa_assert_id(&param.value).value;
                    println!("  * direction = {direction}");
                } else if param.key == SPA_PARAM_ROUTE_device {
                    let device_id = spa_assert_int(&param.value).value;
                    println!("  * device = {device_id}");
                } else if param.key == SPA_PARAM_ROUTE_name {
                    let name = spa_pod_string_get_value(spa_assert_string(&param.value));
                    println!("  * name = {name:?}");
                } else if param.key == SPA_PARAM_ROUTE_description {
                    let description = spa_pod_string_get_value(spa_assert_string(&param.value));
                    println!("  * description = {description:?}");
                } else if param.key == SPA_PARAM_ROUTE_priority {
                    let priority = spa_assert_int(&param.value).value;
                    println!("  * priority = {priority}");
                } else if param.key == SPA_PARAM_ROUTE_available {
                    let available = spa_assert_id(&param.value).value;
                    println!("  * available = {available}");
                } else if param.key == SPA_PARAM_ROUTE_profiles {
                    let values = SPAPODArrayIterator::new(spa_assert_array(&param.value))
                        .copied()
                        .collect::<Vec<i32>>();
                    println!("  * profiles = {values:?}");
                } else if param.key == SPA_PARAM_ROUTE_devices {
                    let values = SPAPODArrayIterator::new(spa_assert_array(&param.value))
                        .copied()
                        .collect::<Vec<i32>>();
                    println!("  * devices = {values:?}");
                } else if param.key == SPA_PARAM_ROUTE_profile {
                    let profile = spa_assert_int(&param.value).value;
                    println!("  * profile = {profile}");
                } else if param.key == SPA_PARAM_ROUTE_save {
                    let save = spa_assert_bool(&param.value).value;
                    println!("  * save = {save}");
                } else if param.key == SPA_PARAM_ROUTE_info {
                    let mut member_iter =
                        SPAPODStructMemberIterator::new(spa_assert_struct(&param.value));

                    let item_count = spa_assert_int(member_iter.next().unwrap()).value;
                    for _ in 0..item_count {
                        let key = spa_pod_string_get_value(spa_assert_string(
                            member_iter.next().unwrap(),
                        ));
                        let value = spa_pod_string_get_value(spa_assert_string(
                            member_iter.next().unwrap(),
                        ));

                        println!("  * info[{key:?}] = {value:?}");
                    }
                } else if param.key == SPA_PARAM_ROUTE_props {
                    for prop_pod in SPAPODObjectPropsIterator::new(spa_assert_object_of(
                        &param.value,
                        SPA_TYPE_OBJECT_Props,
                    )) {
                        println!("  * prop {} {}", prop_pod.key, prop_pod.value.r#type);
                    }
                } else {
                    println!(
                        "  * ParamProfile prop: {} {} {}",
                        param.key, param.value.r#type, param.value.size
                    );
                }
            }
        } else {
            println!(
                "* object pod? {} {}",
                first_pod.body.r#type, first_pod.body.id
            );
        }
    } else {
        println!("* first_pod? {} {}", first_pod.r#type, first_pod.size);
    }
}

#[inline(always)]
fn spa_assert_bool(v: &spa_pod) -> &spa_pod_bool {
    assert_eq!(v.r#type, SPA_TYPE_Bool);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_bool>(v) }
}

#[inline(always)]
fn spa_assert_id(v: &spa_pod) -> &spa_pod_id {
    assert_eq!(v.r#type, SPA_TYPE_Id);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_id>(v) }
}

#[inline(always)]
fn spa_assert_int(v: &spa_pod) -> &spa_pod_int {
    assert_eq!(v.r#type, SPA_TYPE_Int);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_int>(v) }
}

#[inline(always)]
fn spa_assert_string(v: &spa_pod) -> &spa_pod_string {
    assert_eq!(v.r#type, SPA_TYPE_String);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_string>(v) }
}

#[inline(always)]
fn spa_assert_array(v: &spa_pod) -> &spa_pod_array {
    assert_eq!(v.r#type, SPA_TYPE_Array);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_array>(v) }
}

#[inline(always)]
fn spa_assert_struct(v: &spa_pod) -> &spa_pod_struct {
    assert_eq!(v.r#type, SPA_TYPE_Struct);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_struct>(v) }
}

#[inline(always)]
fn spa_assert_object(v: &spa_pod) -> &spa_pod_object {
    assert_eq!(v.r#type, SPA_TYPE_Object);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_object>(v) }
}

#[inline(always)]
fn spa_assert_object_of(v: &spa_pod, object_type: spa_type) -> &spa_pod_object {
    let v = spa_assert_object(v);
    assert_eq!(v.body.r#type, object_type);
    v
}

#[inline(always)]
const fn spa_pod_string_get_value(v: &spa_pod_string) -> &core::ffi::CStr {
    unsafe { core::ffi::CStr::from_ptr(v.value.as_ptr() as _) }
}

pub struct SPAPODObjectPropsIterator<'a> {
    pod: &'a spa_pod_object,
    offset: usize,
}
impl<'a> SPAPODObjectPropsIterator<'a> {
    pub fn new(pod: &'a spa_pod_object) -> Self {
        Self { pod, offset: 0 }
    }
}
impl<'a> Iterator for SPAPODObjectPropsIterator<'a> {
    type Item = &'a spa_pod_prop;

    fn next(&mut self) -> Option<Self::Item> {
        // +8: spa_pod_object_bodyの頭のぶん
        if self.offset + 8 >= self.pod.pod.size as usize {
            return None;
        }

        let v = unsafe {
            &*self
                .pod
                .body
                .props
                .as_ptr()
                .add(self.offset)
                .cast::<spa_pod_prop>()
        };
        // round up to 8 bytes
        self.offset = (self.offset + v.total_size() + 7) & !7;
        Some(v)
    }
}

pub struct SPAPODStructMemberIterator<'a> {
    pod: &'a spa_pod_struct,
    offset: usize,
}
impl<'a> SPAPODStructMemberIterator<'a> {
    pub fn new(pod: &'a spa_pod_struct) -> Self {
        Self { pod, offset: 0 }
    }
}
impl<'a> Iterator for SPAPODStructMemberIterator<'a> {
    type Item = &'a spa_pod;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.pod.pod.size as usize {
            return None;
        }

        let v = unsafe { &*self.pod.values.as_ptr().add(self.offset).cast::<spa_pod>() };
        // round up to 8 bytes
        self.offset = (self.offset + v.total_size() + 7) & !7;
        Some(v)
    }
}

pub struct SPAPODArrayIterator<'a, T> {
    pod: &'a spa_pod_array,
    offset: usize,
    _marker: core::marker::PhantomData<*const T>,
}
impl<'a, T> SPAPODArrayIterator<'a, T> {
    pub fn new(pod: &'a spa_pod_array) -> Self {
        Self {
            pod,
            offset: 0,
            _marker: core::marker::PhantomData,
        }
    }
}
impl<'a, T: 'a> Iterator for SPAPODArrayIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pod.body.child.size > 0
            && self.offset + self.pod.body.child.size as usize >= self.pod.pod.size as usize
        {
            return None;
        }

        let v = unsafe { &*self.pod.body.values.as_ptr().add(self.offset).cast::<T>() };
        self.offset += self.pod.body.child.size as usize;
        Some(v)
    }
}
