use core::ffi::*;
use pipewire::{
    CoreEventListener, RegistryEventListener, core_event_fptbl, raw::*, registry_event_fptbl,
};

fn main() {
    unsafe {
        pw_init(core::ptr::null_mut(), core::ptr::null_mut());

        let ml = pw_main_loop_new(core::ptr::null());
        let context = pw_context_new(pw_main_loop_get_loop(ml), core::ptr::null_mut(), 0);
        let core = pw_context_connect(context, core::ptr::null_mut(), 0);
        let mut event_ctx = PwCoreContext {
            rt_seq: None,
            mainloop_ptr: ml,
            registry_ptr: core::ptr::null_mut(),
        };

        let mut core_event_listener = core::mem::MaybeUninit::zeroed();
        let r = pw_core::add_listener(
            core,
            core_event_listener.as_mut_ptr(),
            core_event_fptbl::<PwCoreContext>(),
            &mut event_ctx as *mut _ as _,
        );
        assert!(r >= 0, "pw_core::add_listener: {r}");

        let registry = pw_core::get_registry(core, PW_VERSION_REGISTRY, 0);
        event_ctx.registry_ptr = registry;
        let mut registry_listener = core::mem::MaybeUninit::zeroed();
        pw_registry::add_listener(
            registry,
            registry_listener.as_mut_ptr(),
            registry_event_fptbl::<PwCoreContext>(),
            &mut event_ctx as *mut _ as _,
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
            [format_pod.as_ptr().cast()].as_mut_ptr(),
            1,
        );

        pw_main_loop_run(ml);
    }
}

struct PwCoreContext {
    pub rt_seq: Option<core::ffi::c_int>,
    pub mainloop_ptr: *mut pw_main_loop,
    pub registry_ptr: *mut pw_registry,
}
impl CoreEventListener for PwCoreContext {
    fn done(&mut self, id: u32, seq: c_int) {
        if id == PW_ID_CORE && self.rt_seq.is_some_and(|x| x == seq) {
            // done roundtrip
            self.rt_seq = None;
            unsafe {
                pw_main_loop_quit(self.mainloop_ptr);
            }
        }
    }
}
impl RegistryEventListener for PwCoreContext {
    fn global(
        &mut self,
        id: u32,
        permissions: u32,
        r#type: &CStr,
        version: u32,
        props: *const spa_dict,
    ) {
        println!("registry global: {id} {type:?} {version} {permissions:04o}");

        if r#type == c"PipeWire:Interface:Device" {
            unsafe {
                let device =
                    pw_registry::bind(self.registry_ptr, id, r#type.as_ptr(), PW_VERSION_DEVICE, 0)
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
                let o =
                    pw_registry::bind(self.registry_ptr, id, r#type.as_ptr(), PW_VERSION_NODE, 0)
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
                let o =
                    pw_registry::bind(self.registry_ptr, id, r#type.as_ptr(), PW_VERSION_PORT, 0)
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

    fn global_remove(&mut self, id: u32) {
        println!("global remove {id}");
    }
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
    let param = pipewire::spa::pod::Parser::new(unsafe { &*param });
    if let Some(object_parser) = param.try_as_object() {
        if object_parser.object_type() == SPA_TYPE_OBJECT_ParamProfile {
            println!("  * type: ParamProfile *");
            for param in object_parser.iter_props() {
                if param.key() == SPA_PARAM_PROFILE_index {
                    let index = param.value().try_as_int().unwrap().value();
                    println!("  * index = {index}");
                } else if param.key() == SPA_PARAM_PROFILE_name {
                    let name = param.value().try_as_string().unwrap().value();
                    println!("  * name = {name:?}");
                } else if param.key() == SPA_PARAM_PROFILE_description {
                    let description = param.value().try_as_string().unwrap().value();
                    println!("  * description = {description:?}");
                } else if param.key() == SPA_PARAM_PROFILE_priority {
                    let priority = param.value().try_as_int().unwrap().value();
                    println!("  * Priority = {priority}");
                } else if param.key() == SPA_PARAM_PROFILE_available {
                    let available = param.value().try_as_id().unwrap().value();
                    println!("  * available = {available}");
                } else if param.key() == SPA_PARAM_PROFILE_classes {
                    let mut members_iter = param.value().try_as_struct().unwrap().iter_members();

                    let item_count = members_iter.next().unwrap().try_as_int().unwrap().value();
                    for _ in 0..item_count {
                        let mut entry_members_iter = members_iter
                            .next()
                            .unwrap()
                            .try_as_struct()
                            .unwrap()
                            .iter_members();

                        let class_name = entry_members_iter
                            .next()
                            .unwrap()
                            .try_as_string()
                            .unwrap()
                            .value();
                        let node_count = entry_members_iter
                            .next()
                            .unwrap()
                            .try_as_int()
                            .unwrap()
                            .value();
                        let property = entry_members_iter
                            .next()
                            .unwrap()
                            .try_as_string()
                            .unwrap()
                            .value();
                        let device_indices = unsafe {
                            entry_members_iter
                                .next()
                                .unwrap()
                                .try_as_array()
                                .unwrap()
                                .values_unchecked::<i32>()
                        };

                        println!(
                            "  * classes = {class_name:?} {node_count} {property:?} {device_indices:?}"
                        );
                    }
                } else if param.key() == SPA_PARAM_PROFILE_save {
                    let save = param.value().try_as_bool().unwrap().value();
                    println!("  * save = {save}");
                } else {
                    println!(
                        "  * ParamProfile prop: {} {:?} {}",
                        param.key(),
                        param.value().r#type(),
                        param.value().size()
                    );
                }
            }
        } else if object_parser.object_type() == SPA_TYPE_OBJECT_ParamRoute {
            println!("  * type: ParamRoute *");
            for param in object_parser.iter_props() {
                if param.key() == SPA_PARAM_ROUTE_index {
                    let index = param.value().try_as_int().unwrap().value();
                    println!("  * index = {index}");
                } else if param.key() == SPA_PARAM_ROUTE_direction {
                    let direction = param.value().try_as_id().unwrap().value();
                    println!("  * direction = {direction}");
                } else if param.key() == SPA_PARAM_ROUTE_device {
                    let device_id = param.value().try_as_int().unwrap().value();
                    println!("  * device = {device_id}");
                } else if param.key() == SPA_PARAM_ROUTE_name {
                    let name = param.value().try_as_string().unwrap().value();
                    println!("  * name = {name:?}");
                } else if param.key() == SPA_PARAM_ROUTE_description {
                    let description = param.value().try_as_string().unwrap().value();
                    println!("  * description = {description:?}");
                } else if param.key() == SPA_PARAM_ROUTE_priority {
                    let priority = param.value().try_as_int().unwrap().value();
                    println!("  * priority = {priority}");
                } else if param.key() == SPA_PARAM_ROUTE_available {
                    let available = param.value().try_as_id().unwrap().value();
                    println!("  * available = {available}");
                } else if param.key() == SPA_PARAM_ROUTE_profiles {
                    let values = unsafe {
                        param
                            .value()
                            .try_as_array()
                            .unwrap()
                            .values_unchecked::<i32>()
                    };
                    println!("  * profiles = {values:?}");
                } else if param.key() == SPA_PARAM_ROUTE_devices {
                    let values = unsafe {
                        param
                            .value()
                            .try_as_array()
                            .unwrap()
                            .values_unchecked::<i32>()
                    };
                    println!("  * devices = {values:?}");
                } else if param.key() == SPA_PARAM_ROUTE_profile {
                    let profile = param.value().try_as_int().unwrap().value();
                    println!("  * profile = {profile}");
                } else if param.key() == SPA_PARAM_ROUTE_save {
                    let save = param.value().try_as_bool().unwrap().value();
                    println!("  * save = {save}");
                } else if param.key() == SPA_PARAM_ROUTE_info {
                    let mut member_iter = param.value().try_as_struct().unwrap().iter_members();

                    let item_count = member_iter.next().unwrap().try_as_int().unwrap().value();
                    for _ in 0..item_count {
                        let key = member_iter.next().unwrap().try_as_string().unwrap().value();
                        let value = member_iter.next().unwrap().try_as_string().unwrap().value();

                        println!("  * info[{key:?}] = {value:?}");
                    }
                } else if param.key() == SPA_PARAM_ROUTE_props {
                    let o = param.value().try_as_object().unwrap();
                    assert_eq!(o.object_type(), SPA_TYPE_OBJECT_Props);
                    for prop_pod in o.iter_props() {
                        println!(
                            "  * prop {} {:?}",
                            prop_pod.key(),
                            prop_pod.value().r#type()
                        );
                    }
                } else {
                    println!(
                        "  * ParamProfile prop: {} {:?} {}",
                        param.key(),
                        param.value().r#type(),
                        param.value().size()
                    );
                }
            }
        } else {
            println!(
                "* object pod? {} {}",
                object_parser.object_type(),
                object_parser.object_id()
            );
        }
    } else {
        println!("* first_pod? {:?} {}", param.r#type(), param.size());
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
    let pod = pipewire::spa::pod::Parser::new(pod);
    if let Some(pod) = pod.try_as_object() {
        if pod.object_type() == SPA_TYPE_OBJECT_PropInfo {
            for prop in pod.iter_props() {
                if prop.key() == spa_prop_info::id as u32 {
                    let value = prop.value().try_as_id().unwrap().value();
                    println!("  * id = {value} (spa_prop)");
                } else if prop.key() == spa_prop_info::name as u32 {
                    let value = prop.value().try_as_string().unwrap().value();
                    println!("  * name = {value:?}");
                } else if prop.key() == spa_prop_info::r#type as u32 {
                    if let Some(v) = prop.value().try_as_id() {
                        let value = v.value();
                        println!("  * type<id> = {value:?}");
                    } else if let Some(v) = prop.value().try_as_int() {
                        let value = v.value();
                        println!("  * type<int> = {value:?}");
                    } else if let Some(v) = prop.value().try_as_string() {
                        let value = v.value();
                        println!("  * type<string> = {value:?}");
                    } else if let Some(v) = prop.value().try_as_choice() {
                        if let Some(v) = v.try_as_range() {
                            match v.child_type() {
                                Ok(pipewire::spa::pod::Type::Id) => {
                                    let r = unsafe { v.default_unchecked::<u32>() };
                                    println!("  * type = range<id> {r}");
                                }
                                Ok(pipewire::spa::pod::Type::Int) => {
                                    let r = unsafe { v.default_unchecked::<i32>() };
                                    println!("  * type = range<int> {r}");
                                }
                                Ok(pipewire::spa::pod::Type::Long) => {
                                    let r = unsafe { v.default_unchecked::<i64>() };
                                    println!("  * type = range<long> {r}");
                                }
                                Ok(pipewire::spa::pod::Type::Float) => {
                                    let r = unsafe { v.default_unchecked::<f32>() };
                                    println!("  * type = range<float> {r}");
                                }
                                Ok(pipewire::spa::pod::Type::Double) => {
                                    let r = unsafe { v.default_unchecked::<f64>() };
                                    println!("  * type = range<double> {r}");
                                }
                                t => panic!("?type[choice.range] = t:{t:?}"),
                            }
                        } else if let Some(v) = v.try_as_enum() {
                            match v.child_type() {
                                Ok(pipewire::spa::pod::Type::Bool) => {
                                    let default = unsafe {
                                        v.default_unchecked::<pipewire::spa::pod::ArrayValueBool>()
                                            .value()
                                    };
                                    let alternatives = unsafe {
                                        v.alternatives_unchecked::<pipewire::spa::pod::ArrayValueBool>()
                                    };

                                    println!("  * type = enum<bool> {default} {alternatives:?}");
                                }
                                t => panic!("?type[choice.enum] = t:{t:?}"),
                            }
                        } else {
                            panic!("?type[choice] = t:{:?}", v.choice_type());
                        }
                    } else {
                        panic!("?type = t:{:?}", prop.value().r#type());
                    }
                } else if prop.key() == spa_prop_info::labels as u32 {
                    println!("  * labels = t:{:?}", prop.value().r#type());
                } else if prop.key() == spa_prop_info::container as u32 {
                    let value = prop.value().try_as_id().unwrap().value();
                    println!("  * container = {value}");
                } else if prop.key() == spa_prop_info::params as u32 {
                    let value = prop.value().try_as_bool().unwrap().value();
                    println!("  * params = {value}");
                } else if prop.key() == spa_prop_info::description as u32 {
                    let value = prop.value().try_as_string().unwrap().value();
                    println!("  * description = {value:?}");
                } else {
                    println!("  * prop_info: {} {:?}", prop.key(), prop.value().r#type());
                }
            }
        } else if pod.object_type() == SPA_TYPE_OBJECT_Props {
            for prop in pod.iter_props() {
                if prop.key() == spa_prop::device as u32 {
                    let value = prop.value().try_as_string().unwrap().value();
                    println!("  * device = {value:?}");
                } else if prop.key() == spa_prop::deviceName as u32 {
                    let value = prop.value().try_as_string().unwrap().value();
                    println!("  * device name = {value:?}");
                } else if prop.key() == spa_prop::cardName as u32 {
                    let value = prop.value().try_as_string().unwrap().value();
                    println!("  * card name = {value:?}");
                } else if prop.key() == spa_prop::volume as u32 {
                    let value = prop.value().try_as_float().unwrap().value();
                    println!("  * volume = {value}");
                } else if prop.key() == spa_prop::mute as u32 {
                    let value = prop.value().try_as_bool().unwrap().value();
                    println!("  * mute = {value}");
                } else if prop.key() == spa_prop::channelVolumes as u32 {
                    let values = unsafe {
                        prop.value()
                            .try_as_array()
                            .unwrap()
                            .values_unchecked::<f32>()
                    };
                    println!("  * channel volumes = {values:?}");
                } else if prop.key() == spa_prop::channelMap as u32 {
                    let channel_ids = unsafe {
                        prop.value()
                            .try_as_array()
                            .unwrap()
                            .values_unchecked::<u32>()
                    };
                    println!("  * channel map = {channel_ids:?}");
                } else if prop.key() == spa_prop::monitorMute as u32 {
                    let value = prop.value().try_as_bool().unwrap().value();
                    println!("  * monitor mute = {value}");
                } else if prop.key() == spa_prop::monitorVolumes as u32 {
                    let values = unsafe {
                        prop.value()
                            .try_as_array()
                            .unwrap()
                            .values_unchecked::<f32>()
                    };
                    println!("  * monitor volumes = {values:?}");
                } else if prop.key() == spa_prop::latencyOffsetNsec as u32 {
                    let value = prop.value().try_as_long().unwrap().value();
                    println!("  * latency offset ns = {value}");
                } else if prop.key() == spa_prop::softMute as u32 {
                    let value = prop.value().try_as_bool().unwrap().value();
                    println!("  * soft mute = {value}");
                } else if prop.key() == spa_prop::softVolumes as u32 {
                    let values = unsafe {
                        prop.value()
                            .try_as_array()
                            .unwrap()
                            .values_unchecked::<f32>()
                    };
                    println!("  * soft volumes = {values:?}");
                } else if prop.key() == spa_prop::params as u32 {
                    let mut members_iter = prop.value().try_as_struct().unwrap().iter_members();
                    while let Some(k) = members_iter.next() {
                        let k = k.try_as_string().unwrap().value();
                        let v = members_iter.next().unwrap();

                        println!("  * params[{k:?}] = t:{:?} s:{}", v.r#type(), v.size());
                    }
                } else {
                    println!("  * prop: {} {:?}", prop.key(), prop.value().r#type());
                }
            }
        } else if pod.object_type() == SPA_TYPE_OBJECT_Format {
            for prop in pod.iter_props() {
                if prop.key() == spa_format::mediaType as u32 {
                    if let Some(v) = prop.value().try_as_id() {
                        let value = v.value();
                        println!("  * media type = {value}");
                    } else if let Some(v) = prop.value().try_as_choice() {
                        if let Some(v) = v.try_as_none() {
                            match v.child_type() {
                                Ok(pipewire::spa::pod::Type::Id) => {
                                    let current_value = unsafe { v.current_unchecked::<u32>() };
                                    println!("  * media type = choice.none<u32> {current_value}");
                                }
                                t => panic!("media type.choice.none t:{t:?}"),
                            }
                        } else {
                            panic!("media type.choice = t:{:?}", v.choice_type());
                        }
                    } else {
                        panic!("  * media type ?= t:{:?}", prop.value().r#type());
                    }
                } else if prop.key() == spa_format::mediaSubtype as u32 {
                    if let Some(v) = prop.value().try_as_id() {
                        let value = v.value();
                        println!("  * media subtype = {value}");
                    } else if let Some(v) = prop.value().try_as_choice() {
                        if let Some(v) = v.try_as_none() {
                            match v.child_type() {
                                Ok(pipewire::spa::pod::Type::Id) => {
                                    let current_value = unsafe { v.current_unchecked::<u32>() };
                                    println!(
                                        "  * media subtype = choice.none<u32> {current_value}"
                                    );
                                }
                                t => panic!("media subtype.choice.none t:{t:?}"),
                            }
                        } else {
                            panic!("media subtype.choice = t:{:?}", v.choice_type());
                        }
                    } else {
                        panic!("  * media subtype ?= t:{:?}", prop.value().r#type());
                    }
                } else if prop.key() == spa_format::AUDIO_format as u32 {
                    if let Some(v) = prop.value().try_as_id() {
                        let value = v.value();
                        println!("  * audio format = {value}");
                    } else if let Some(v) = prop.value().try_as_choice() {
                        if let Some(v) = v.try_as_none() {
                            match v.child_type() {
                                Ok(pipewire::spa::pod::Type::Id) => {
                                    let current_value = unsafe { v.current_unchecked::<u32>() };
                                    println!("  * audio format = choice.none<u32> {current_value}");
                                }
                                t => panic!("audio format.choice.none t:{t:?}"),
                            }
                        } else if let Some(v) = v.try_as_enum() {
                            match v.child_type() {
                                Ok(pipewire::spa::pod::Type::Id) => {
                                    let default = unsafe { v.default_unchecked::<u32>() };

                                    println!("  * audio format = choice.enum<u32> {default}");
                                }
                                t => panic!("audio format.choice.enum t:{t:?}"),
                            }
                        } else {
                            panic!("audio format.choice = t:{:?}", v.choice_type());
                        }
                    } else {
                        panic!("  * audio format ?= t:{:?}", prop.value().r#type());
                    }
                } else if prop.key() == spa_format::AUDIO_rate as u32 {
                    if let Some(v) = prop.value().try_as_int() {
                        let value = v.value();
                        println!("  * audio rate = {value}");
                    } else if let Some(v) = prop.value().try_as_choice() {
                        if let Some(v) = v.try_as_none() {
                            match v.child_type() {
                                Ok(pipewire::spa::pod::Type::Int) => {
                                    let current_value = unsafe { v.current_unchecked::<i32>() };
                                    println!("  * audio rate = choice.none<int> {current_value}");
                                }
                                t => panic!("audio rate.choice.none t:{t:?}"),
                            }
                        } else if let Some(v) = v.try_as_range() {
                            match v.child_type() {
                                Ok(pipewire::spa::pod::Type::Int) => {
                                    let range = unsafe { v.values_unchecked::<i32>() };
                                    println!("  * audio rate = choice.range<int> {range:?}");
                                }
                                t => panic!("audio rate.choice.range t:{t:?}"),
                            }
                        } else if let Some(v) = v.try_as_enum() {
                            match v.child_type() {
                                Ok(pipewire::spa::pod::Type::Id) => {
                                    let default = unsafe { v.default_unchecked::<u32>() };
                                    println!("  * audio rate = choice.enum<id> {default}");
                                }
                                t => panic!("audio rate.choice.enum t:{t:?}"),
                            }
                        } else {
                            panic!("audio rate.choice = t:{:?}", v.choice_type());
                        }
                    } else {
                        panic!("  * audio rate ?= t:{:?}", prop.value().r#type());
                    }
                } else if prop.key() == spa_format::AUDIO_channels as u32 {
                    println!("  * audio channels ?= t:{:?}", prop.value().r#type());
                } else if prop.key() == spa_format::AUDIO_position as u32 {
                    println!("  * audio position ?= t:{:?}", prop.value().r#type());
                } else {
                    println!("  * format: {} {:?}", prop.key(), prop.value().r#type());
                }
            }
        } else {
            println!("  * object param: {}", pod.object_type());
        }
    } else {
        println!("  * param type: {:?}", pod.r#type());
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
    println!("port params {seq} {id} {index} {next}");
}
