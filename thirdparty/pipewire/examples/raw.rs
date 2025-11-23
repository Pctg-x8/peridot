use pipewire::raw::*;

fn main() {
    unsafe {
        pw_init(core::ptr::null_mut(), core::ptr::null_mut());

        let ml = pw_main_loop_new(core::ptr::null());
        let context = pw_context_new(pw_main_loop_get_loop(ml), core::ptr::null_mut(), 0);
        let core = pw_context_connect(context, core::ptr::null_mut(), 0);
        let mut event_ctx = PwCoreContext {
            rt_seq: None,
            mainloop_ptr: ml,
        };

        let mut core_event_listener = core::mem::MaybeUninit::zeroed();
        let r = pw_core::add_listener(
            core,
            core_event_listener.as_mut_ptr(),
            CORE_EVENTS,
            &mut event_ctx as *mut _ as _,
        );
        assert!(r >= 0, "pw_core::add_listener: {r}");

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
        let mut registry_listener = core::mem::MaybeUninit::zeroed();
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

        event_ctx.rt_seq = Some(pw_core::sync(core, PW_ID_CORE, 0));
        pw_main_loop_run(ml);

        let mut format_pod = pipewire::spa::pod::Builder::with_capacity(1024);
        format_pod
            .begin_object(SPA_TYPE_OBJECT_Format, SPA_PARAM_EnumFormat as _)
            .prop_heading(spa_format::mediaType as _, 0)
            .id(spa_media_type::audio as _)
            .prop_heading(spa_format::mediaSubtype as _, 0)
            .id(spa_media_subtype::raw as _)
            .prop_heading(spa_format::AUDIO_format as _, 0)
            .id(SPA_AUDIO_FORMAT_F32_LE as _)
            .prop_heading(spa_format::AUDIO_rate as _, 0)
            .int(44100)
            .prop_heading(spa_format::AUDIO_channels as _, 0)
            .int(2)
            .end_object();
        let format_pod = format_pod.into_bytes();

        let stream = pw_stream_new(
            core,
            c"test-audio-source".as_ptr(),
            pw_properties_new(
                c"media.type".as_ptr(),
                c"Audio".as_ptr(),
                c"media.category".as_ptr(),
                c"Playback".as_ptr(),
                c"media.role".as_ptr(),
                c"Game".as_ptr(),
                core::ptr::null::<core::ffi::c_char>(),
            ),
        );
        let mut stream_listener = core::mem::MaybeUninit::zeroed();
        pw_stream_add_listener(
            stream,
            stream_listener.as_mut_ptr(),
            STREAM_EVENT,
            stream.cast(),
        );
        pw_stream_connect(
            stream,
            PW_DIRECTION_OUTPUT,
            PW_ID_ANY,
            PW_STREAM_FLAG_RT_PROCESS | PW_STREAM_FLAG_MAP_BUFFERS | PW_STREAM_FLAG_AUTOCONNECT,
            [
                /*format_spa_buf.as_ptr().cast::<spa_pod>()*/ format_pod.as_ptr().cast(),
            ]
            .as_mut_ptr(),
            1,
        );

        pw_main_loop_run(ml);
    }
}

struct PwCoreContext {
    pub rt_seq: Option<core::ffi::c_int>,
    pub mainloop_ptr: *mut pw_main_loop,
}

static CORE_EVENTS: &pw_core_events = &pw_core_events {
    version: PW_VERSION_CORE_EVENTS,
    ping: None,
    info: None,
    error: None,
    bound_id: None,
    remove_id: None,
    add_mem: None,
    remove_mem: None,
    bound_props: None,
    done: Some(core_done),
};

extern "C" fn core_done(data: *mut core::ffi::c_void, id: u32, seq: core::ffi::c_int) {
    let data = unsafe { &mut *data.cast::<PwCoreContext>() };

    println!("pw core done: {id} {seq} 0x{seq:x}");
    if id == PW_ID_CORE && data.rt_seq.is_some_and(|x| x == seq) {
        // done roundtrip
        data.rt_seq = None;
        unsafe {
            pw_main_loop_quit(data.mainloop_ptr);
        }
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

    if r#type == c"PipeWire:Interface:Node" {
        unsafe {
            let registry = data.cast::<pw_registry>();
            let o = ((*(*registry.cast::<spa_interface>())
                .cb
                .funcs
                .cast::<pw_registry_methods>())
            .bind
            .unwrap_unchecked())(
                (*registry.cast::<spa_interface>()).cb.data,
                id,
                r#type.as_ptr(),
                PW_VERSION_NODE,
                0,
            )
            .cast::<pw_node>();
            let node_listener = Box::leak(Box::new(core::mem::MaybeUninit::zeroed()));
            ((*(*o.cast::<spa_interface>())
                .cb
                .funcs
                .cast::<pw_node_methods>())
            .add_listener
            .unwrap_unchecked())(
                (*o.cast::<spa_interface>()).cb.data,
                node_listener.as_mut_ptr(),
                NODE_EVENTS,
                o.cast(),
            );
        }
    }

    if r#type == c"PipeWire:Interface:Port" {
        unsafe {
            let registry = data.cast::<pw_registry>();
            let o = ((*(*registry.cast::<spa_interface>())
                .cb
                .funcs
                .cast::<pw_registry_methods>())
            .bind
            .unwrap_unchecked())(
                (*registry.cast::<spa_interface>()).cb.data,
                id,
                r#type.as_ptr(),
                PW_VERSION_PORT,
                0,
            )
            .cast::<pw_port>();
            let node_listener = Box::leak(Box::new(core::mem::MaybeUninit::zeroed()));
            ((*(*o.cast::<spa_interface>())
                .cb
                .funcs
                .cast::<pw_port_methods>())
            .add_listener
            .unwrap_unchecked())(
                (*o.cast::<spa_interface>()).cb.data,
                node_listener.as_mut_ptr(),
                PORT_EVENTS,
                o.cast(),
            );
        }
    }
}
extern "C" fn registry_global_remove(data: *mut core::ffi::c_void, id: u32) {
    println!("global remove {id}");
}

static STREAM_EVENT: &pw_stream_events = &pw_stream_events {
    version: PW_VERSION_STREAM_EVENTS,
    destroy: None,
    state_changed: Some(stream_state_changed),
    control_info: Some(stream_control_info),
    io_changed: None,
    param_changed: None,
    add_buffer: None,
    remove_buffer: None,
    process: Some(stream_process),
    drained: None,
    command: None,
    trigger_done: None,
};

extern "C" fn stream_state_changed(
    data: *mut core::ffi::c_void,
    old: pw_stream_state,
    state: pw_stream_state,
    error: *const core::ffi::c_char,
) {
    println!("state changed: {old} {state}");
}

extern "C" fn stream_control_info(
    data: *mut core::ffi::c_void,
    id: u32,
    control: *const pw_stream_control,
) {
    println!("control info: {id} {:?}", unsafe {
        core::ffi::CStr::from_ptr((*control).name)
    });
}

static mut SMP: usize = 0;

extern "C" fn stream_process(data: *mut core::ffi::c_void) {
    println!("stream process");

    unsafe {
        let buf = pw_stream_dequeue_buffer(data.cast::<pw_stream>());
        if buf.is_null() {
            eprintln!("out of buffer");
            return;
        }

        let mut frames = (*(*(*buf).buffer).datas).maxsize as u64 / (4 * 2);
        if (*buf).requested != 0 {
            frames = frames.min((*buf).requested);
        }

        for dst in core::slice::from_raw_parts_mut(
            (*(*(*buf).buffer).datas).data.cast::<[f32; 2]>(),
            frames as _,
        ) {
            let v = (core::f32::consts::TAU * 440.0 * SMP as f32 / 44100.0).sin() * 0.5;
            *dst = [v, v];
            SMP += 1;
        }

        (*(*(*(*buf).buffer).datas).chunk).offset = 0;
        (*(*(*(*buf).buffer).datas).chunk).stride = 4 * 2;
        (*(*(*(*buf).buffer).datas).chunk).size = (frames * 4 * 2) as _;

        pw_stream_queue_buffer(data.cast::<pw_stream>(), buf);
    }
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
            println!("  * type: ParamProfile *");
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
                    let mut members_iter =
                        SPAPODStructMemberIterator::new(spa_assert_struct(&param.value));

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
            println!("  * type: ParamRoute *");
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

static NODE_EVENTS: &pw_node_events = &pw_node_events {
    version: PW_VERSION_NODE_EVENTS,
    /*info: Some(node_info),
    param: Some(node_params),*/
    info: None,
    param: None,
};

extern "C" fn node_info(data: *mut core::ffi::c_void, info: *const pw_node_info) {
    let info = unsafe { &*info };
    let params = info.params().iter().map(|x| x.id).collect::<Vec<_>>();

    println!("node info {} {params:?}", info.id);
    for p in info.params() {
        unsafe {
            ((*(*data.cast::<spa_interface>())
                .cb
                .funcs
                .cast::<pw_node_methods>())
            .enum_params
            .unwrap_unchecked())(
                (*data.cast::<spa_interface>()).cb.data,
                p.seq,
                p.id,
                0,
                u32::MAX,
                core::ptr::null(),
            );
        }
    }
}

extern "C" fn node_params(
    data: *mut core::ffi::c_void,
    seq: core::ffi::c_int,
    id: u32,
    index: u32,
    next: u32,
    param: *const spa_pod,
) {
    println!("node params {data:p} {seq} {id} {index} {next}");

    let Some(pod) = (unsafe { param.as_ref() }) else {
        return;
    };
    if pod.r#type == SPA_TYPE_Object {
        let pod = unsafe { core::mem::transmute::<&spa_pod, &spa_pod_object>(pod) };
        if pod.body.r#type == SPA_TYPE_OBJECT_PropInfo {
            for prop in SPAPODObjectPropsIterator::new(pod) {
                if prop.key == spa_prop_info::id as u32 {
                    let value = spa_assert_id(&prop.value).value;
                    println!("  * id = {value} (spa_prop)");
                } else if prop.key == spa_prop_info::name as u32 {
                    let value = spa_pod_string_get_value(spa_assert_string(&prop.value));
                    println!("  * name = {value:?}");
                } else if prop.key == spa_prop_info::r#type as u32 {
                    if prop.value.r#type == SPA_TYPE_Id {
                        let value = unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_id>(&prop.value).value
                        };
                        println!("  * type<id> = {value:?}");
                    } else if prop.value.r#type == SPA_TYPE_Int {
                        let value = unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_int>(&prop.value).value
                        };
                        println!("  * type<int> = {value:?}");
                    } else if prop.value.r#type == SPA_TYPE_String {
                        let value = spa_pod_string_get_value(unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_string>(&prop.value)
                        });
                        println!("  * type<string> = {value:?}");
                    } else if prop.value.r#type == SPA_TYPE_Choice {
                        let value = unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_choice>(&prop.value)
                        };

                        if value.body.r#type == SPA_CHOICE_Range {
                            if value.element_pod().r#type == SPA_TYPE_Id {
                                let r = unsafe { SPAPODChoiceRange::<u32>::read_unchecked(value) };
                                println!("  * type = range<id> {r:?}");
                            } else if value.element_pod().r#type == SPA_TYPE_Int {
                                let r = unsafe { SPAPODChoiceRange::<i32>::read_unchecked(value) };
                                println!("  * type = range<int> {r:?}");
                            } else if value.element_pod().r#type == SPA_TYPE_Long {
                                let r = unsafe { SPAPODChoiceRange::<i64>::read_unchecked(value) };
                                println!("  * type = range<long> {r:?}");
                            } else if value.element_pod().r#type == SPA_TYPE_Float {
                                let r = unsafe { SPAPODChoiceRange::<f32>::read_unchecked(value) };
                                println!("  * type = range<float> {r:?}");
                            } else if value.element_pod().r#type == SPA_TYPE_Double {
                                let r = unsafe { SPAPODChoiceRange::<f64>::read_unchecked(value) };
                                println!("  * type = range<double> {r:?}");
                            } else {
                                panic!("?type[choice.range] = t:{}", value.element_pod().r#type);
                            }
                        } else if value.body.r#type == SPA_CHOICE_Enum {
                            if value.element_pod().r#type == SPA_TYPE_Bool {
                                let values =
                                    unsafe { SPAPODChoiceEnum::<i32>::read_unchecked(value) };
                                println!("  * type = enum<bool> {values:?}");
                            } else {
                                panic!("?type[choice.enum] = t:{}", value.element_pod().r#type);
                            }
                        } else {
                            panic!("?type[choice] = t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("?type = t:{}", prop.value.r#type);
                    }
                } else if prop.key == spa_prop_info::labels as u32 {
                    println!("  * labels = t:{}", prop.value.r#type);
                } else if prop.key == spa_prop_info::container as u32 {
                    let value = spa_assert_id(&prop.value).value;
                    println!("  * container = {value}");
                } else if prop.key == spa_prop_info::params as u32 {
                    let value = spa_assert_bool(&prop.value).as_bool();
                    println!("  * params = {value}");
                } else if prop.key == spa_prop_info::description as u32 {
                    let value = spa_pod_string_get_value(spa_assert_string(&prop.value));
                    println!("  * description = {value:?}");
                } else {
                    println!("  * prop_info: {} {}", prop.key, prop.value.r#type);
                }
            }
        } else if pod.body.r#type == SPA_TYPE_OBJECT_Props {
            for prop in SPAPODObjectPropsIterator::new(pod) {
                if prop.key == spa_prop::device as u32 {
                    let value = spa_pod_string_get_value(spa_assert_string(&prop.value));
                    println!("  * device = {value:?}");
                } else if prop.key == spa_prop::deviceName as u32 {
                    let value = spa_pod_string_get_value(spa_assert_string(&prop.value));
                    println!("  * device name = {value:?}");
                } else if prop.key == spa_prop::cardName as u32 {
                    let value = spa_pod_string_get_value(spa_assert_string(&prop.value));
                    println!("  * card name = {value:?}");
                } else if prop.key == spa_prop::volume as u32 {
                    let value = spa_assert_float(&prop.value).value;
                    println!("  * volume = {value}");
                } else if prop.key == spa_prop::mute as u32 {
                    let value = spa_assert_bool(&prop.value).as_bool();
                    println!("  * mute = {value}");
                } else if prop.key == spa_prop::channelVolumes as u32 {
                    let values = SPAPODArrayIterator::new(spa_assert_array(&prop.value))
                        .copied()
                        .collect::<Vec<f32>>();
                    println!("  * channel volumes = {values:?}");
                } else if prop.key == spa_prop::channelMap as u32 {
                    let channel_ids = SPAPODArrayIterator::new(spa_assert_array(&prop.value))
                        .copied()
                        .collect::<Vec<u32>>();
                    println!("  * channel map = {channel_ids:?}");
                } else if prop.key == spa_prop::monitorMute as u32 {
                    let value = spa_assert_bool(&prop.value).as_bool();
                    println!("  * monitor mute = {value}");
                } else if prop.key == spa_prop::monitorVolumes as u32 {
                    let values = SPAPODArrayIterator::new(spa_assert_array(&prop.value))
                        .copied()
                        .collect::<Vec<f32>>();
                    println!("  * monitor volumes = {values:?}");
                } else if prop.key == spa_prop::latencyOffsetNsec as u32 {
                    let value = spa_assert_long(&prop.value).value;
                    println!("  * latency offset ns = {value}");
                } else if prop.key == spa_prop::softMute as u32 {
                    let value = spa_assert_bool(&prop.value).as_bool();
                    println!("  * soft mute = {value}");
                } else if prop.key == spa_prop::softVolumes as u32 {
                    let values = SPAPODArrayIterator::new(spa_assert_array(&prop.value))
                        .copied()
                        .collect::<Vec<f32>>();
                    println!("  * soft volumes = {values:?}");
                } else if prop.key == spa_prop::params as u32 {
                    let mut members_iter =
                        SPAPODStructMemberIterator::new(spa_assert_struct(&prop.value));
                    while let Some(k) = members_iter.next() {
                        let k = spa_assert_string(k);
                        let v = members_iter.next().unwrap();

                        println!(
                            "  * params[{:?}] = t:{} s:{}",
                            spa_pod_string_get_value(k),
                            v.r#type,
                            v.size
                        );
                    }
                } else {
                    println!("  * prop: {} {}", prop.key, prop.value.r#type);
                }
            }
        } else if pod.body.r#type == SPA_TYPE_OBJECT_Format {
            for prop in SPAPODObjectPropsIterator::new(pod) {
                if prop.key == spa_format::mediaType as u32 {
                    if prop.value.r#type == SPA_TYPE_Id {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_id>(&prop.value) };
                        println!("  * media type = {}", value.value);
                    } else if prop.value.r#type == SPA_TYPE_Choice {
                        let value = unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_choice>(&prop.value)
                        };

                        if value.body.r#type == SPA_CHOICE_None {
                            if value.body.child.r#type == SPA_TYPE_Id {
                                let current_value = unsafe {
                                    core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                                };
                                println!("  * media type = choice.none<u32> {current_value}");
                            } else {
                                panic!("media type.choice.none t:{}", value.body.child.r#type)
                            }
                        } else {
                            panic!("media type.choice = t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("  * media type ?= t:{}", prop.value.r#type);
                    }
                } else if prop.key == spa_format::mediaSubtype as u32 {
                    if prop.value.r#type == SPA_TYPE_Id {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_id>(&prop.value) };
                        println!("  * media subtype = {}", value.value);
                    } else if prop.value.r#type == SPA_TYPE_Choice {
                        let value = unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_choice>(&prop.value)
                        };

                        if value.body.r#type == SPA_CHOICE_None {
                            if value.body.child.r#type == SPA_TYPE_Id {
                                let current_value = unsafe {
                                    core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                                };
                                println!("  * media subtype = choice.none<u32> {current_value}");
                            } else {
                                panic!("media subtype.choice.none t:{}", value.body.child.r#type)
                            }
                        } else {
                            panic!("media subtype.choice = t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("  * media subtype ?= t:{}", prop.value.r#type);
                    }
                } else if prop.key == spa_format::AUDIO_format as u32 {
                    if prop.value.r#type == SPA_TYPE_Id {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_id>(&prop.value) };
                        println!("  * audio format = {}", value.value);
                    } else if prop.value.r#type == SPA_TYPE_Choice {
                        let value = unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_choice>(&prop.value)
                        };

                        if value.body.r#type == SPA_CHOICE_None {
                            if value.body.child.r#type == SPA_TYPE_Id {
                                let current_value = unsafe {
                                    core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                                };
                                println!("  * audio format = choice.none<id> {current_value}");
                            } else {
                                panic!("audio format.choice.none t:{}", value.body.child.r#type)
                            }
                        } else if value.body.r#type == SPA_CHOICE_Enum {
                            if value.body.child.r#type == SPA_TYPE_Id {
                                let current_value = unsafe {
                                    core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                                };
                                println!("  * audio format = choice.enum<id> {current_value}");
                            } else {
                                panic!("audio format.choice.enum t:{}", value.body.child.r#type)
                            }
                        } else {
                            panic!("audio format.choice = t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("  * audio format ?= t:{}", prop.value.r#type);
                    }
                } else if prop.key == spa_format::AUDIO_rate as u32 {
                    if prop.value.r#type == SPA_TYPE_Int {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_int>(&prop.value) };
                        println!("  * audio rate = {}", value.value);
                    } else if prop.value.r#type == SPA_TYPE_Choice {
                        let value = unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_choice>(&prop.value)
                        };

                        if value.body.r#type == SPA_CHOICE_None {
                            if value.body.child.r#type == SPA_TYPE_Int {
                                let current_value = unsafe {
                                    core::ptr::read(value.body.values.as_ptr().cast::<i32>())
                                };
                                println!("  * audio rate = choice.none<int> {current_value}");
                            } else {
                                panic!("audio rate.choice.none t:{}", value.body.child.r#type)
                            }
                        } else if value.body.r#type == SPA_CHOICE_Range {
                            if value.body.child.r#type == SPA_TYPE_Int {
                                let range =
                                    unsafe { SPAPODChoiceRange::<i32>::read_unchecked(value) };

                                println!("  * audio rate = choice.range<int> {range:?}");
                            } else {
                                panic!("audio rate.choice.range t:{}", value.body.child.r#type)
                            }
                        } else if value.body.r#type == SPA_CHOICE_Enum {
                            if value.body.child.r#type == SPA_TYPE_Id {
                                let current_value = unsafe {
                                    core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                                };
                                println!("  * audio rate = choice.enum<id> {current_value}");
                            } else {
                                panic!("audio rate.choice.enum t:{}", value.body.child.r#type)
                            }
                        } else {
                            panic!("audio rate.choice = t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("  * audio rate ?= t:{}", prop.value.r#type);
                    }
                } else if prop.key == spa_format::AUDIO_channels as u32 {
                    if prop.value.r#type == SPA_TYPE_Int {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_int>(&prop.value) };
                        println!("  * audio channels = {}", value.value);
                    } else if prop.value.r#type == SPA_TYPE_Choice {
                        let value = unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_choice>(&prop.value)
                        };

                        if value.body.r#type == SPA_CHOICE_None {
                            if value.body.child.r#type == SPA_TYPE_Int {
                                let current_value = unsafe {
                                    core::ptr::read(value.body.values.as_ptr().add(0).cast::<i32>())
                                };
                                println!("  * audio channels = choice.none<int> {current_value}");
                            } else {
                                panic!("audio channels.choice.none t:{}", value.body.child.r#type)
                            }
                        } else if value.body.r#type == SPA_CHOICE_Enum {
                            if value.body.child.r#type == SPA_TYPE_Id {
                                let current_value = unsafe {
                                    core::ptr::read(value.body.values.as_ptr().add(0).cast::<u32>())
                                };
                                println!("  * audio channels = choice.enum<id> {current_value}");
                            } else {
                                panic!("audio channels.choice.enum t:{}", value.body.child.r#type)
                            }
                        } else {
                            panic!("audio channels.choice = t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("  * audio channels ?= t:{}", prop.value.r#type);
                    }
                } else if prop.key == spa_format::AUDIO_position as u32 {
                    if prop.value.r#type == SPA_TYPE_Id {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_id>(&prop.value) };
                        println!("  * audio position = {}", value.value);
                    } else if prop.value.r#type == SPA_TYPE_Array {
                        let values = SPAPODArrayIterator::new(unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_array>(&prop.value)
                        })
                        .copied()
                        .collect::<Vec<u32>>();
                        println!("  * audio position = {values:?}");
                    } else if prop.value.r#type == SPA_TYPE_Choice {
                        let value = unsafe {
                            core::mem::transmute::<&spa_pod, &spa_pod_choice>(&prop.value)
                        };

                        if value.body.r#type == SPA_CHOICE_None {
                            if value.body.child.r#type == SPA_TYPE_Id {
                                let current_value = unsafe {
                                    core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                                };
                                println!("  * audio position = choice.none<id> {current_value}");
                            } else if value.body.child.r#type == SPA_TYPE_Array {
                                let mut o = 0;
                                let mut values = Vec::new();
                                while o < value.body.child.size as usize {
                                    values.push(unsafe {
                                        core::ptr::read(
                                            value.body.values.as_ptr().add(o).cast::<u32>(),
                                        )
                                    });
                                    o += 4;
                                }

                                println!("  * audio position = choice.none<array<id>> {values:?}");
                            } else {
                                panic!("audio position.choice.none t:{}", value.body.child.r#type)
                            }
                        } else if value.body.r#type == SPA_CHOICE_Enum {
                            if value.body.child.r#type == SPA_TYPE_Id {
                                let current_value = unsafe {
                                    core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                                };
                                println!("  * audio position = choice.enum<id> {current_value}");
                            } else {
                                panic!("audio position.choice.enum t:{}", value.body.child.r#type)
                            }
                        } else {
                            panic!("audio position.choice = t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("  * audio position ?= t:{}", prop.value.r#type);
                    }
                } else {
                    println!("  * format: {} {}", prop.key, prop.value.r#type);
                }
            }
        } else if pod.body.r#type == SPA_TYPE_OBJECT_ParamPortConfig {
            for p in SPAPODObjectPropsIterator::new(pod) {
                if p.key == spa_param_port_config::direction as u32 {
                    if p.value.r#type == SPA_TYPE_Id {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_id>(&p.value) };
                        println!("  * direction = {}", value.value);
                    } else if p.value.r#type == SPA_TYPE_Choice {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_choice>(&p.value) };
                        assert_eq!(value.element_pod().r#type, SPA_TYPE_Id);

                        if value.body.r#type == SPA_CHOICE_None {
                            let value = unsafe {
                                core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                            };
                            println!("  * direction = choice.none<id> {value}");
                        } else {
                            panic!("direction.choice: t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("direction: t:{}", p.value.r#type);
                    }
                } else if p.key == spa_param_port_config::mode as u32 {
                    if p.value.r#type == SPA_TYPE_Id {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_id>(&p.value) };
                        println!("  * mode = {}", value.value);
                    } else if p.value.r#type == SPA_TYPE_Choice {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_choice>(&p.value) };
                        assert_eq!(value.element_pod().r#type, SPA_TYPE_Id);

                        if value.body.r#type == SPA_CHOICE_None {
                            let value = unsafe {
                                core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                            };
                            println!("  * mode = choice.none<id> {value}");
                        } else if value.body.r#type == SPA_CHOICE_Enum {
                            let values = unsafe { SPAPODChoiceEnum::<u32>::read_unchecked(value) };

                            println!("  * mode = choice.enum<id> {values:?}");
                        } else {
                            panic!("mode.choice: t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("mode: t:{}", p.value.r#type);
                    }
                } else if p.key == spa_param_port_config::monitor as u32 {
                    if p.value.r#type == SPA_TYPE_Bool {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_id>(&p.value) };
                        println!("  * monitor = {}", value.value);
                    } else if p.value.r#type == SPA_TYPE_Choice {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_choice>(&p.value) };
                        assert_eq!(value.body.child.r#type, SPA_TYPE_Bool);

                        if value.body.r#type == SPA_CHOICE_None {
                            let value = unsafe {
                                core::ptr::read(value.body.values.as_ptr().cast::<i32>())
                            };
                            println!("  * monitor = choice.none<bool> {value}");
                        } else if value.body.r#type == SPA_CHOICE_Enum {
                            let values = unsafe { SPAPODChoiceEnum::<i32>::read_unchecked(value) };

                            println!("  * monitor = choice.enum<bool> {values:?}");
                        } else {
                            panic!("monitor.choice: t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("monitor: t:{}", p.value.r#type);
                    }
                } else if p.key == spa_param_port_config::control as u32 {
                    if p.value.r#type == SPA_TYPE_Bool {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_id>(&p.value) };
                        println!("  * control = {}", value.value);
                    } else if p.value.r#type == SPA_TYPE_Choice {
                        let value =
                            unsafe { core::mem::transmute::<&spa_pod, &spa_pod_choice>(&p.value) };
                        assert_eq!(value.body.child.r#type, SPA_TYPE_Bool);

                        if value.body.r#type == SPA_CHOICE_None {
                            let value = unsafe {
                                core::ptr::read(value.body.values.as_ptr().cast::<i32>())
                            };
                            println!("  * control = choice.none<bool> {value}");
                        } else if value.body.r#type == SPA_CHOICE_Enum {
                            let values = unsafe { SPAPODChoiceEnum::<i32>::read_unchecked(value) };

                            println!("  * control = choice.enum<bool> {values:?}");
                        } else {
                            panic!("control.choice: t:{}", value.body.r#type);
                        }
                    } else {
                        panic!("control: t:{}", p.value.r#type);
                    }
                } else if p.key == spa_param_port_config::format as u32 {
                    for p in SPAPODObjectPropsIterator::new(spa_assert_object_of(
                        &p.value,
                        SPA_TYPE_OBJECT_Format,
                    )) {
                        if p.key == spa_format::mediaType as u32 {
                            if p.value.r#type == SPA_TYPE_Id {
                                let value = unsafe {
                                    core::mem::transmute::<&spa_pod, &spa_pod_id>(&p.value)
                                };
                                println!("  * format.media type = {}", value.value);
                            } else if p.value.r#type == SPA_TYPE_Choice {
                                let value = unsafe {
                                    core::mem::transmute::<&spa_pod, &spa_pod_choice>(&p.value)
                                };
                                assert_eq!(value.element_pod().r#type, SPA_TYPE_Id);

                                if value.body.r#type == SPA_CHOICE_None {
                                    let current_value = unsafe {
                                        core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                                    };
                                    println!(
                                        "  * format.media type = choice.none<u32> {current_value}"
                                    );
                                } else {
                                    panic!("format.media type.choice = t:{}", value.body.r#type);
                                }
                            } else {
                                panic!("  * format.media type ?= t:{}", p.value.r#type);
                            }
                        } else if p.key == spa_format::mediaSubtype as u32 {
                            if p.value.r#type == SPA_TYPE_Id {
                                let value = unsafe {
                                    core::mem::transmute::<&spa_pod, &spa_pod_id>(&p.value).value
                                };
                                println!("  * format.media subtype = {value}");
                            } else if p.value.r#type == SPA_TYPE_Choice {
                                let value = unsafe {
                                    core::mem::transmute::<&spa_pod, &spa_pod_choice>(&p.value)
                                };
                                assert_eq!(value.element_pod().r#type, SPA_TYPE_Id);

                                if value.body.r#type == SPA_CHOICE_None {
                                    let current_value = unsafe {
                                        core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                                    };
                                    println!(
                                        "  * format.media subtype = choice.none<u32> {current_value}"
                                    );
                                } else {
                                    panic!("format.media subtype.choice = t:{}", value.body.r#type);
                                }
                            } else {
                                panic!("  * format.media subtype ?= t:{}", p.value.r#type);
                            }
                        } else if p.key == spa_format::AUDIO_format as u32 {
                            if p.value.r#type == SPA_TYPE_Id {
                                let value = unsafe {
                                    core::mem::transmute::<&spa_pod, &spa_pod_id>(&p.value).value
                                };
                                println!("  * format.audio format = {value}");
                            } else if p.value.r#type == SPA_TYPE_Choice {
                                let value = unsafe {
                                    core::mem::transmute::<&spa_pod, &spa_pod_choice>(&p.value)
                                };
                                assert_eq!(value.element_pod().r#type, SPA_TYPE_Id);

                                if value.body.r#type == SPA_CHOICE_None {
                                    let current_value = unsafe {
                                        core::ptr::read(value.body.values.as_ptr().cast::<u32>())
                                    };
                                    println!(
                                        "  * format.audio format = choice.none<id> {current_value}"
                                    );
                                } else if value.body.r#type == SPA_CHOICE_Enum {
                                    let values =
                                        unsafe { SPAPODChoiceEnum::<u32>::read_unchecked(value) };

                                    println!(
                                        "  * format.audio format = choice.enum<id> {values:?}"
                                    );
                                } else {
                                    panic!("format.audio format.choice = t:{}", value.body.r#type);
                                }
                            } else {
                                panic!("  * format.audio format ?= t:{}", p.value.r#type);
                            }
                        } else if p.key == spa_format::AUDIO_rate as u32 {
                            if p.value.r#type == SPA_TYPE_Int {
                                let value = unsafe {
                                    core::mem::transmute::<&spa_pod, &spa_pod_int>(&p.value).value
                                };
                                println!("  * audio rate = {value}");
                            } else if p.value.r#type == SPA_TYPE_Choice {
                                let value = unsafe {
                                    core::mem::transmute::<&spa_pod, &spa_pod_choice>(&p.value)
                                };
                                assert_eq!(value.body.child.r#type, SPA_TYPE_Int);

                                if value.body.r#type == SPA_CHOICE_None {
                                    let current_value = unsafe {
                                        core::ptr::read(value.body.values.as_ptr().cast::<i32>())
                                    };
                                    println!(
                                        "  * format.audio rate = choice.none<int> {current_value}"
                                    );
                                } else if value.body.r#type == SPA_CHOICE_Range {
                                    let range =
                                        unsafe { SPAPODChoiceRange::<i32>::read_unchecked(value) };

                                    println!("  * format.audio rate = choice.range<int> {range:?}");
                                } else if value.body.r#type == SPA_CHOICE_Enum {
                                    let values =
                                        unsafe { SPAPODChoiceEnum::<i32>::read_unchecked(value) };

                                    println!("  * format.audio rate = choice.enum<id> {values:?}");
                                } else {
                                    panic!("format.audio rate.choice = t:{}", value.body.r#type);
                                }
                            } else {
                                panic!("  * format.audio rate ?= t:{}", p.value.r#type);
                            }
                        } else if p.key == spa_format::AUDIO_channels as u32 {
                            if p.value.r#type == SPA_TYPE_Int {
                                let value = unsafe {
                                    core::mem::transmute::<&spa_pod, &spa_pod_int>(&p.value)
                                };
                                println!("  * format.audio channels = {}", value.value);
                            } else if p.value.r#type == SPA_TYPE_Choice {
                                let value = unsafe {
                                    core::mem::transmute::<&spa_pod, &spa_pod_choice>(&p.value)
                                };
                                assert_eq!(value.element_pod().r#type, SPA_TYPE_Int);

                                if value.body.r#type == SPA_CHOICE_None {
                                    let current_value = unsafe {
                                        core::ptr::read(value.body.values.as_ptr().cast::<i32>())
                                    };
                                    println!(
                                        "  * format.audio channels = choice.none<int> {current_value}"
                                    );
                                } else if value.body.r#type == SPA_CHOICE_Enum {
                                    let values =
                                        unsafe { SPAPODChoiceEnum::<i32>::read_unchecked(value) };

                                    println!(
                                        "  * format.audio channels = choice.enum<id> {values:?}"
                                    );
                                } else {
                                    panic!(
                                        "format.audio channels.choice = t:{}",
                                        value.body.r#type
                                    );
                                }
                            } else {
                                panic!("  * format.audio channels ?= t:{}", p.value.r#type);
                            }
                        } else if p.key == spa_format::AUDIO_position as u32 {
                            let values = SPAPODArrayIterator::new(spa_assert_array(&p.value))
                                .copied()
                                .collect::<Vec<u32>>();
                            println!("  * format.audio position = {values:?}");
                        } else {
                            println!("  * format.{} {}", p.key, p.value.r#type);
                        }
                    }
                } else {
                    println!("  * param port config: {} t:{}", p.key, p.value.r#type);
                }
            }
        } else if pod.body.r#type == SPA_TYPE_OBJECT_ParamLatency {
            let mut direction = None;
            let mut min_quantum = None;
            let mut max_quantum = None;
            let mut min_rate = None;
            let mut max_rate = None;
            let mut min_ns = None;
            let mut max_ns = None;
            for p in SPAPODObjectPropsIterator::new(pod) {
                if p.key == spa_param_latency::direction as u32 {
                    assert!(direction.is_none(), "multiprop: direction");
                    direction = Some(spa_assert_id(&p.value).value);
                } else if p.key == spa_param_latency::minQuantum as u32 {
                    assert!(min_quantum.is_none(), "multiprop: minQuantum");
                    min_quantum = Some(spa_assert_float(&p.value).value);
                } else if p.key == spa_param_latency::maxQuantum as u32 {
                    assert!(max_quantum.is_none(), "multiprop: maxQuantum");
                    max_quantum = Some(spa_assert_float(&p.value).value);
                } else if p.key == spa_param_latency::minRate as u32 {
                    assert!(min_rate.is_none(), "multiprop: minRate");
                    min_rate = Some(spa_assert_int(&p.value).value);
                } else if p.key == spa_param_latency::maxRate as u32 {
                    assert!(max_rate.is_none(), "multiprop: maxRate");
                    max_rate = Some(spa_assert_int(&p.value).value);
                } else if p.key == spa_param_latency::minNs as u32 {
                    assert!(min_ns.is_none(), "multiprop: minNs");
                    min_ns = Some(spa_assert_long(&p.value).value);
                } else if p.key == spa_param_latency::maxNs as u32 {
                    assert!(max_ns.is_none(), "multiprop: maxNs");
                    max_ns = Some(spa_assert_long(&p.value).value);
                } else {
                    panic!("param latency: {} t:{}", p.key, p.value.r#type);
                }
            }

            #[derive(Debug)]
            enum Direction {
                In,
                Out,
            }

            let direction = match direction.expect("missing: direction") {
                v if v == SPA_DIRECTION_INPUT as u32 => Direction::In,
                v if v == SPA_DIRECTION_OUTPUT as u32 => Direction::Out,
                v => panic!("invalid direction id: {v}"),
            };
            let min_quantum = min_quantum.expect("missing: minQuantum");
            let max_quantum = max_quantum.expect("missing: maxQuantum");
            let min_rate = min_rate.expect("missing: minRate");
            let max_rate = max_rate.expect("missing: maxRate");
            let min_ns = min_ns.expect("missing: minNs");
            let max_ns = max_ns.expect("missing: maxNs");
            println!(
                "  * param latency: {direction:?} {min_quantum}..{max_quantum} {min_rate}..{max_rate} {min_ns}..{max_ns}"
            );
        } else {
            println!("  * object param: {}", pod.body.r#type);
        }
    } else {
        println!("  * param type: {}", pod.r#type);
    }
}

static PORT_EVENTS: &pw_port_events = &pw_port_events {
    version: PW_VERSION_NODE_EVENTS,
    info: Some(port_info),
    param: Some(port_params),
};

extern "C" fn port_info(data: *mut core::ffi::c_void, info: *const pw_port_info) {
    let info = unsafe { &*info };
    let params = info.params().iter().map(|x| x.id).collect::<Vec<_>>();

    println!("port info {} {params:?}", info.id);
    for p in info.params() {
        unsafe {
            ((*(*data.cast::<spa_interface>())
                .cb
                .funcs
                .cast::<pw_port_methods>())
            .enum_params
            .unwrap_unchecked())(
                (*data.cast::<spa_interface>()).cb.data,
                p.seq,
                p.id,
                0,
                u32::MAX,
                core::ptr::null(),
            );
        }
    }
}

extern "C" fn port_params(
    data: *mut core::ffi::c_void,
    seq: core::ffi::c_int,
    id: u32,
    index: u32,
    next: u32,
    param: *const spa_pod,
) {
    println!("node params {seq} {id} {index} {next}");
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
fn spa_assert_long(v: &spa_pod) -> &spa_pod_long {
    assert_eq!(v.r#type, SPA_TYPE_Long);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_long>(v) }
}

#[inline(always)]
fn spa_assert_float(v: &spa_pod) -> &spa_pod_float {
    assert_eq!(v.r#type, SPA_TYPE_Float);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_float>(v) }
}

#[inline(always)]
fn spa_assert_double(v: &spa_pod) -> &spa_pod_double {
    assert_eq!(v.r#type, SPA_TYPE_Double);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_double>(v) }
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
fn spa_assert_choice(v: &spa_pod) -> &spa_pod_choice {
    assert_eq!(v.r#type, SPA_TYPE_Choice);
    unsafe { core::mem::transmute::<&spa_pod, &spa_pod_choice>(v) }
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

#[derive(Debug, Clone)]
pub struct SPAPODChoiceRange<T> {
    pub default: T,
    pub min: T,
    pub max: T,
}
impl<T> SPAPODChoiceRange<T> {
    pub unsafe fn read_unchecked(pod: &spa_pod_choice) -> Self {
        debug_assert!(pod.pod.size >= 16 + pod.element_pod().size * 3);

        Self {
            default: unsafe { core::ptr::read(pod.body.values.as_ptr().cast::<T>()) },
            min: unsafe {
                core::ptr::read(
                    pod.body
                        .values
                        .as_ptr()
                        .add(pod.element_pod().size as usize)
                        .cast::<T>(),
                )
            },
            max: unsafe {
                core::ptr::read(
                    pod.body
                        .values
                        .as_ptr()
                        .add((pod.element_pod().size * 2) as usize)
                        .cast::<T>(),
                )
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct SPAPODChoiceEnum<T> {
    pub default: T,
    pub alternatives: Vec<T>,
}
impl<T> SPAPODChoiceEnum<T> {
    pub unsafe fn read_unchecked(pod: &spa_pod_choice) -> Self {
        debug_assert!(pod.pod.size >= 16 + pod.element_pod().size);

        let default = unsafe { core::ptr::read(pod.body.values.as_ptr().cast::<T>()) };
        let mut alternatives = Vec::new();
        let mut o = pod.element_pod().size as usize;
        while o + 16 + pod.element_pod().size as usize <= pod.pod.size as usize {
            alternatives
                .push(unsafe { core::ptr::read(pod.body.values.as_ptr().add(o).cast::<T>()) });
            o += pod.element_pod().size as usize;
        }

        Self {
            default,
            alternatives,
        }
    }
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
        // 8: spa_pod_array_body::valuesのオフセット
        if self.pod.body.child.size > 0
            && self.offset + 8 + self.pod.body.child.size as usize >= self.pod.pod.size as usize
        {
            return None;
        }

        let v = unsafe { &*self.pod.body.values.as_ptr().add(self.offset).cast::<T>() };
        self.offset += self.pod.body.child.size as usize;
        Some(v)
    }
}
