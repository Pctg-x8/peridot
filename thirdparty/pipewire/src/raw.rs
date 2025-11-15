#![allow(non_camel_case_types)]

use core::ffi::*;

mod spa;
pub use self::spa::*;

pub type pw_direction = spa_direction;
pub const PW_DIRECTION_INPUT: pw_direction = SPA_DIRECTION_INPUT;
pub const PW_DIRECTION_OUTPUT: pw_direction = SPA_DIRECTION_OUTPUT;

#[repr(C)]
pub struct pw_properties {
    pub dict: spa_dict,
    pub flags: u32,
}

pub const PW_PROPERTIES_FLAG_NL: u32 = 1 << 0;
pub const PW_PROPERTIES_FLAG_RECURSE: u32 = 1 << 1;
pub const PW_PROPERTIES_FLAG_ENCLOSE: u32 = 1 << 2;
pub const PW_PROPERTIES_FLAG_ARRAY: u32 = 1 << 3;
pub const PW_PROPERTIES_FLAG_COLORS: u32 = 1 << 4;

#[repr(C)]
pub struct pw_loop {
    pub system: *mut spa_system,
    pub r#loop: *mut spa_loop,
    pub control: *mut spa_loop_control,
    pub utils: *mut spa_loop_utils,
    pub name: *const c_char,
}

#[repr(C)]
pub struct pw_main_loop(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct pw_main_loop_events {
    pub version: u32,
    pub destroy: Option<extern "C" fn(data: *mut c_void)>,
}

#[repr(C)]
pub struct pw_context(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct pw_global(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct pw_impl_client(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct pw_impl_node(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct pw_context_events {
    pub version: u32,
    pub destroy: Option<extern "C" fn(data: *mut c_void)>,
    pub free: Option<extern "C" fn(data: *mut c_void)>,
    pub check_access: Option<extern "C" fn(data: *mut c_void, client: *mut pw_impl_client)>,
    pub global_added: Option<extern "C" fn(data: *mut c_void, global: *mut pw_global)>,
    pub global_removed: Option<extern "C" fn(data: *mut c_void, global: *mut pw_global)>,
    pub driver_added: Option<extern "C" fn(data: *mut c_void, node: *mut pw_impl_node)>,
    pub driver_removed: Option<extern "C" fn(data: *mut c_void, node: *mut pw_impl_node)>,
}

#[repr(C)]
pub struct pw_export_type {
    pub link: spa_list,
    pub r#type: *const c_char,
    pub func: Option<
        extern "C" fn(
            core: *mut pw_core,
            r#type: *const c_char,
            props: *const spa_dict,
            object: *mut c_void,
            user_data_size: usize,
        ) -> *mut pw_proxy,
    >,
}

#[repr(C)]
pub struct pw_core(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

pub const PW_VERSION_CORE: u32 = 4;

#[repr(C)]
pub struct pw_registry(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

pub const PW_VERSION_REGISTRY: u32 = 3;

pub const PW_DEFAULT_REMOTE: &str = "pipewire-0";

pub const PW_ID_CORE: u32 = 0;
pub const PW_ID_ANY: u32 = 0xffff_ffff;

#[repr(C)]
pub struct pw_core_info {
    pub id: u32,
    pub cookie: u32,
    pub user_name: *const c_char,
    pub host_name: *const c_char,
    pub version: *const c_char,
    pub name: *const c_char,
    pub change_mask: u64,
    pub props: *mut spa_dict,
}

pub const PW_CORE_CHANGE_MASK_PROPS: u64 = 1 << 0;
pub const PW_CORE_CHANGE_MASK_ALL: u64 = (1 << 1) - 1;

#[repr(C)]
pub struct pw_proxy(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct pw_protocol(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct pw_protocol_client {
    pub link: spa_list,
    pub protocol: *mut pw_protocol,
    pub core: *mut pw_core,
    pub connect: Option<
        extern "C" fn(
            client: *mut pw_protocol_client,
            props: *const spa_dict,
            done_callback: Option<extern "C" fn(data: *mut c_void, result: c_int)>,
            data: *mut c_void,
        ) -> c_int,
    >,
    pub connect_fd:
        Option<extern "C" fn(client: *mut pw_protocol_client, fd: c_int, close: bool) -> c_int>,
    pub steal_fd: Option<extern "C" fn(client: *mut pw_protocol_client) -> c_int>,
    pub disconnect: Option<extern "C" fn(client: *mut pw_protocol_client)>,
    pub destroy: Option<extern "C" fn(client: *mut pw_protocol_client)>,
    pub set_paused: Option<extern "C" fn(client: *mut pw_protocol_client, paused: bool) -> c_int>,
}

#[repr(C)]
pub struct pw_protocol_marshal {
    pub r#type: *const c_char,
    pub version: u32,
    pub flags: u32,
    pub n_client_methods: u32,
    pub n_server_methods: u32,
    pub client_marshal: *const c_void,
    pub server_demarshal: *const c_void,
    pub server_marshal: *const c_void,
    pub client_demarshal: *const c_void,
}

pub const PW_PROTOCOL_MARSHAL_FLAG_IMPL: u32 = 1 << 0;

#[repr(C)]
pub struct pw_protocol_events {
    pub version: u32,
    pub destroy: Option<extern "C" fn(data: *mut c_void)>,
}

#[repr(C)]
pub struct pw_proxy_events {
    pub version: u32,
    pub destroy: Option<extern "C" fn(data: *mut c_void)>,
    pub bound: Option<extern "C" fn(data: *mut c_void, global_id: u32)>,
    pub removed: Option<extern "C" fn(data: *mut c_void)>,
    pub done: Option<extern "C" fn(data: *mut c_void, seq: c_int)>,
    pub error:
        Option<extern "C" fn(data: *mut c_void, seq: c_int, res: c_int, message: *const c_char)>,
    pub bound_props:
        Option<extern "C" fn(data: *mut c_void, global_id: u32, props: *const spa_dict)>,
}

#[repr(C)]
pub struct pw_core_events {
    pub version: u32,
    pub info: Option<extern "C" fn(data: *mut c_void, info: *const pw_core_info)>,
    pub done: Option<extern "C" fn(data: *mut c_void, id: u32, seq: c_int)>,
    pub ping: Option<extern "C" fn(data: *mut c_void, id: u32, seq: c_int)>,
    pub error: Option<
        extern "C" fn(data: *mut c_void, id: u32, seq: c_int, res: c_int, message: *const c_char),
    >,
    pub remove_id: Option<extern "C" fn(data: *mut c_void, id: u32)>,
    pub bound_id: Option<extern "C" fn(data: *mut c_void, id: u32, global_id: u32)>,
    pub add_mem:
        Option<extern "C" fn(data: *mut c_void, id: u32, r#type: u32, fd: c_int, flags: u32)>,
    pub remove_mem: Option<extern "C" fn(data: *mut c_void, id: u32)>,
    pub bound_props:
        Option<extern "C" fn(data: *mut c_void, id: u32, global_id: u32, props: *const spa_dict)>,
}

pub const PW_VERSION_CORE_EVENTS: u32 = 1;

#[repr(C)]
pub struct pw_core_methods {
    pub version: u32,
    pub add_listener: Option<
        extern "C" fn(
            object: *mut c_void,
            listener: *mut spa_hook,
            events: *const pw_core_events,
            data: *mut c_void,
        ) -> c_int,
    >,
    pub hello: Option<extern "C" fn(object: *mut c_void, version: u32) -> c_int>,
    pub sync: Option<extern "C" fn(object: *mut c_void, id: u32, seq: c_int) -> c_int>,
    pub pong: Option<extern "C" fn(object: *mut c_void, id: u32, seq: c_int) -> c_int>,
    pub error: Option<
        extern "C" fn(
            object: *mut c_void,
            id: u32,
            seq: c_int,
            res: c_int,
            message: *const c_char,
        ) -> c_int,
    >,
    pub get_registry: Option<
        extern "C" fn(object: *mut c_void, version: u32, user_data_size: usize) -> *mut pw_registry,
    >,
    pub create_object: Option<
        extern "C" fn(
            object: *mut c_void,
            factory_name: *const c_char,
            r#type: *const c_char,
            version: u32,
            props: *const spa_dict,
            user_data_size: usize,
        ) -> *mut c_void,
    >,
    pub destroy: Option<extern "C" fn(object: *mut c_void, proxy: *mut c_void) -> c_int>,
}

pub const PW_VERSION_CORE_METHODS: u32 = 0;

#[repr(C)]
pub struct pw_registry_events {
    pub version: u32,
    pub global: Option<
        extern "C" fn(
            data: *mut c_void,
            id: u32,
            permissions: u32,
            r#type: *const c_char,
            version: u32,
            props: *const spa_dict,
        ),
    >,
    pub global_remove: Option<extern "C" fn(data: *mut c_void, id: u32)>,
}

pub const PW_VERSION_REGISTRY_EVENTS: u32 = 0;

#[repr(C)]
pub struct pw_registry_methods {
    pub version: u32,
    pub add_listener: Option<
        extern "C" fn(
            object: *mut c_void,
            listener: *mut spa_hook,
            events: *const pw_registry_events,
            data: *mut c_void,
        ) -> c_int,
    >,
    pub bind: Option<
        extern "C" fn(
            object: *mut c_void,
            id: u32,
            r#type: *const c_char,
            version: u32,
            user_data_size: usize,
        ) -> *mut c_void,
    >,
    pub destroy: Option<extern "C" fn(object: *mut c_void, id: u32) -> c_int>,
}

pub const PW_VERSION_REGISTRY_METHOD: u32 = 0;

#[repr(C)]
pub struct pw_client(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct pw_client_info {
    pub id: u32,
    pub change_mask: u64,
    pub props: *mut spa_dict,
}

pub const PW_CLIENT_CHANGE_MASK_PROPS: u64 = 1 << 0;
pub const PW_CLIENT_CHANGE_MASK_ALL: u64 = (1 << 1) - 1;

#[repr(C)]
pub struct pw_client_events {
    pub version: u32,
    pub info: Option<extern "C" fn(data: *mut c_void, info: *const pw_client_info)>,
    pub permissions: Option<
        extern "C" fn(
            data: *mut c_void,
            index: u32,
            n_permissions: u32,
            permissions: *const pw_permission,
        ),
    >,
}

#[repr(C)]
pub struct pw_client_methods {
    pub version: u32,
    pub add_listener: Option<
        extern "C" fn(
            object: *mut c_void,
            listener: *mut spa_hook,
            events: *const pw_client_events,
            data: *mut c_void,
        ) -> c_int,
    >,
    pub error: Option<
        extern "C" fn(object: *mut c_void, id: u32, res: c_int, message: *const c_char) -> c_int,
    >,
    pub update_properties:
        Option<extern "C" fn(object: *mut c_void, props: *const spa_dict) -> c_int>,
    pub get_permissions: Option<extern "C" fn(object: *mut c_void, index: u32, num: u32) -> c_int>,
    pub update_permissions: Option<
        extern "C" fn(
            object: *mut c_void,
            n_permissions: u32,
            permissions: *const pw_permission,
        ) -> c_int,
    >,
}

#[repr(C)]
pub struct pw_permission {
    pub id: u32,
    pub permissions: u32,
}

pub const PW_PERM_R: u32 = 0o0400;
pub const PW_PERM_W: u32 = 0o0200;
pub const PW_PERM_X: u32 = 0o0100;
pub const PW_PERM_M: u32 = 0o0010;
pub const PW_PERM_L: u32 = 0o0020;
pub const PW_PERM_INVALID: u32 = 0xffff_ffff;

#[repr(C)]
pub struct pw_data_loop(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct pw_data_loop_events {
    pub version: u32,
    pub destroy: Option<extern "C" fn(data: *mut c_void)>,
}

pub type pw_memblock_flags = u32;
pub const PW_MEMBLOCK_FLAG_NONE: pw_memblock_flags = 0;
pub const PW_MEMBLOCK_FLAG_READABLE: pw_memblock_flags = 1 << 0;
pub const PW_MEMBLOCK_FLAG_WRITABLE: pw_memblock_flags = 1 << 1;
pub const PW_MEMBLOCK_FLAG_SEAL: pw_memblock_flags = 1 << 2;
pub const PW_MEMBLOCK_FLAG_MAP: pw_memblock_flags = 1 << 3;
pub const PW_MEMBLOCK_FLAG_DONT_CLOSE: pw_memblock_flags = 1 << 4;
pub const PW_MEMBLOCK_FLAG_DONT_NOTIFY: pw_memblock_flags = 1 << 5;
pub const PW_MEMBLOCK_FLAG_UNMAPPABLE: pw_memblock_flags = 1 << 6;
pub const PW_MEMBLOCK_FLAG_READWRITE: pw_memblock_flags =
    PW_MEMBLOCK_FLAG_READABLE | PW_MEMBLOCK_FLAG_WRITABLE;

pub type pw_memmap_flags = u32;
pub const PW_MEMMAP_FLAG_NONE: pw_memmap_flags = 0;
pub const PW_MEMMAP_FLAG_READ: pw_memmap_flags = 1 << 0;
pub const PW_MEMMAP_FLAG_WRITE: pw_memmap_flags = 1 << 1;
pub const PW_MEMMAP_FLAG_TWICE: pw_memmap_flags = 1 << 2;
pub const PW_MEMMAP_FLAG_PRIVATE: pw_memmap_flags = 1 << 3;
pub const PW_MEMMAP_FLAG_LOCKED: pw_memmap_flags = 1 << 4;
pub const PW_MEMMAP_FLAG_READWRITE: pw_memmap_flags = PW_MEMMAP_FLAG_READ | PW_MEMMAP_FLAG_WRITE;

#[repr(C)]
pub struct pw_memchunk(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct pw_mempool {
    pub props: *mut pw_properties,
}

#[repr(C)]
pub struct pw_memblock {
    pub pool: *mut pw_mempool,
    pub id: u32,
    pub r#ref: c_int,
    pub flags: u32,
    pub r#type: u32,
    pub fd: c_int,
    pub size: u32,
    pub map: *mut pw_memmap,
}

#[repr(C)]
pub struct pw_memmap {
    pub block: *mut pw_memblock,
    pub ptr: *mut c_void,
    pub flags: u32,
    pub offset: u32,
    pub size: u32,
    pub tag: [u32; 5],
}

#[repr(C)]
pub struct pw_mempool_events {
    pub version: u32,
    pub destroy: Option<extern "C" fn(data: *mut c_void)>,
    pub added: Option<extern "C" fn(data: *mut c_void, block: *mut pw_memblock)>,
    pub removed: Option<extern "C" fn(data: *mut c_void, block: *mut pw_memblock)>,
}

#[repr(C)]
pub struct pw_map_range {
    pub start: u32,
    pub offset: u32,
    pub size: u32,
}

#[repr(C)]
pub struct pw_work_queue(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

pub type pw_work_func_t = extern "C" fn(obj: *mut c_void, data: *mut c_void, res: c_int, id: u32);

#[repr(C)]
pub struct pw_device(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

pub const PW_VERSION_DEVICE: u32 = 3;

#[repr(C)]
pub struct pw_device_info {
    pub id: u32,
    pub chnage_mask: u64,
    pub props: *mut spa_dict,
    pub params: *mut spa_param_info,
    pub n_params: u32,
}

pub const PW_DEVICE_CHANGE_MASK_PROPS: u64 = 1 << 0;
pub const PW_DEVICE_CHANGE_MASK_PARAMS: u64 = 1 << 1;
pub const PW_DEVICE_CHANGE_MASK_ALL: u64 = (1 << 2) - 1;

#[repr(C)]
pub struct pw_device_events {
    pub version: u32,
    pub info: Option<extern "C" fn(data: *mut c_void, info: *const pw_device_info)>,
    pub param: Option<
        extern "C" fn(
            data: *mut c_void,
            seq: c_int,
            id: u32,
            index: u32,
            next: u32,
            param: *const spa_pod,
        ),
    >,
}

pub const PW_VERSION_DEVICE_EVENTS: u32 = 0;

#[repr(C)]
pub struct pw_device_methods {
    pub version: u32,
    pub add_listener: Option<
        extern "C" fn(
            object: *mut c_void,
            listener: *mut spa_hook,
            events: *const pw_device_events,
            data: *mut c_void,
        ) -> c_int,
    >,
    pub subscribe_params:
        Option<extern "C" fn(object: *mut c_void, ids: *mut u32, n_ids: u32) -> c_int>,
    pub enum_params: Option<
        extern "C" fn(
            object: *mut c_void,
            seq: c_int,
            id: u32,
            start: u32,
            num: u32,
            filter: *const spa_pod,
        ) -> c_int,
    >,
    pub set_param: Option<
        extern "C" fn(objet: *mut c_void, id: u32, flags: u32, param: *const spa_pod) -> c_int,
    >,
}

unsafe extern "C" {
    pub fn pw_init(argc: *mut c_int, argv: *mut *mut *mut c_char);
    pub fn pw_deinit();

    pub fn pw_debug_is_category_enabled(name: *const c_char) -> bool;
    pub fn pw_get_application_name() -> *const c_char;
    pub fn pw_get_prgname() -> *const c_char;
    pub fn pw_get_user_name() -> *const c_char;
    pub fn pw_get_host_name() -> *const c_char;
    pub fn pw_get_client_name() -> *const c_char;
    pub fn pw_check_option(option: *const c_char, value: *const c_char) -> bool;
    pub fn pw_direction_reverse(direction: pw_direction) -> pw_direction;
    pub fn pw_set_domain(domain: *const c_char) -> c_int;
    pub fn pw_get_domain() -> *const c_char;
    pub fn pw_get_support(support: *mut spa_support, max_support: u32) -> u32;
    pub fn pw_load_spa_handle(
        lib: *const c_char,
        factory_name: *const c_char,
        info: *const spa_dict,
        n_support: u32,
        support: *const spa_support,
    ) -> *mut spa_handle;
    pub fn pw_unload_spa_handle(handle: *mut spa_handle) -> c_int;

    pub fn pw_properties_new(key: *const c_char, ...) -> *mut pw_properties;
    pub fn pw_properties_new_dict(dict: *const spa_dict) -> *mut pw_properties;
    pub fn pw_properties_new_string(args: *const c_char) -> *mut pw_properties;
    pub fn pw_properties_new_string_checked(
        args: *const c_char,
        size: usize,
        loc: *mut spa_error_location,
    ) -> *mut pw_properties;
    pub fn pw_properties_copy(properties: *const pw_properties) -> *mut pw_properties;
    pub fn pw_properties_update_keys(
        props: *mut pw_properties,
        dict: *const spa_dict,
        keys: *const *const c_char,
    ) -> c_int;
    pub fn pw_properties_update_ignore(
        props: *mut pw_properties,
        dict: *const spa_dict,
        ignore: *const *const c_char,
    ) -> c_int;
    pub fn pw_properties_update(props: *mut pw_properties, dict: *const spa_dict) -> c_int;
    pub fn pw_properties_update_string(
        props: *mut pw_properties,
        str: *const c_char,
        size: usize,
    ) -> c_int;
    pub fn pw_properties_update_string_checked(
        props: *mut pw_properties,
        str: *const c_char,
        size: usize,
        loc: *mut spa_error_location,
    ) -> c_int;
    pub fn pw_properties_add(oldprops: *mut pw_properties, dict: *const spa_dict) -> c_int;
    pub fn pw_properties_add_keys(
        oldprops: *mut pw_properties,
        dict: *const spa_dict,
        keys: *const *const c_char,
    ) -> c_int;
    pub fn pw_properties_clear(properties: *mut pw_properties);
    pub fn pw_properties_free(properties: *mut pw_properties);
    pub fn pw_properties_str(
        properties: *mut pw_properties,
        key: *const c_char,
        value: *const c_char,
    ) -> c_int;
    pub fn pw_properties_get(properties: *const pw_properties, key: *const c_char)
    -> *const c_char;
    pub fn pw_properties_fetch_uint32(
        properties: *const pw_properties,
        key: *const c_char,
        value: *mut u32,
    ) -> c_int;
    pub fn pw_properties_fetch_int32(
        properties: *const pw_properties,
        key: *const c_char,
        value: *mut i32,
    ) -> c_int;
    pub fn pw_properties_fetch_uint64(
        properties: *const pw_properties,
        key: *const c_char,
        value: *mut u64,
    ) -> c_int;
    pub fn pw_properties_fetch_int64(
        properties: *const pw_properties,
        key: *const c_char,
        value: *mut i64,
    ) -> c_int;
    pub fn pw_properties_fetch_bool(
        properties: *const pw_properties,
        key: *const c_char,
        value: *mut bool,
    ) -> c_int;
    pub fn pw_properties_iterate(
        properties: *const pw_properties,
        state: *mut *mut c_void,
    ) -> *const c_char;

    pub fn pw_loop_new(props: *const spa_dict) -> *mut pw_loop;
    pub fn pw_loop_destroy(r#loop: *mut pw_loop);
    pub fn pw_loop_set_name(r#loop: *mut pw_loop, name: *const c_char) -> c_int;

    pub fn pw_main_loop_new(props: *const spa_dict) -> *mut pw_main_loop;
    pub fn pw_main_loop_add_listener(
        r#loop: *mut pw_main_loop,
        listener: *mut spa_hook,
        events: *const pw_main_loop_events,
        data: *mut c_void,
    );
    pub fn pw_main_loop_get_loop(r#loop: *mut pw_main_loop) -> *mut pw_loop;
    pub fn pw_main_loop_destroy(r#loop: *mut pw_main_loop);
    pub fn pw_main_loop_run(r#loop: *mut pw_main_loop) -> c_int;
    pub fn pw_main_loop_quit(r#loop: *mut pw_main_loop) -> c_int;

    pub fn pw_context_new(
        main_loop: *mut pw_loop,
        props: *mut pw_properties,
        user_data_size: usize,
    ) -> *mut pw_context;
    pub fn pw_context_destroy(context: *mut pw_context);
    pub fn pw_context_get_user_data(context: *mut pw_context) -> *mut c_void;
    pub fn pw_context_add_listener(
        context: *mut pw_context,
        listener: *mut spa_hook,
        events: *const pw_context_events,
        data: *mut c_void,
    );
    pub fn pw_context_get_properties(context: *mut pw_context) -> *const pw_properties;
    pub fn pw_context_update_properties(context: *mut pw_context, dict: *const spa_dict) -> c_int;
    pub fn pw_context_get_conf_section(
        contenxt: *mut pw_context,
        section: *const c_char,
    ) -> *const c_char;
    pub fn pw_context_parse_conf_section(
        context: *mut pw_context,
        conf: *mut pw_properties,
        section: *const c_char,
    ) -> c_int;
    pub fn pw_context_conf_update_props(
        context: *mut pw_context,
        section: *const c_char,
        props: *mut pw_properties,
    ) -> c_int;
    pub fn pw_context_conf_section_for_each(
        context: *mut pw_context,
        section: *const c_char,
        callback: Option<
            extern "C" fn(
                data: *mut c_void,
                location: *const c_char,
                section: *const c_char,
                str: *const c_char,
                len: usize,
            ) -> c_int,
        >,
        data: *mut c_void,
    ) -> c_int;
    pub fn pw_context_conf_section_match_rules(
        context: *mut pw_context,
        section: *const c_char,
        props: *const spa_dict,
        callback: Option<
            extern "C" fn(
                data: *mut c_void,
                location: *const c_char,
                action: *const c_char,
                str: *const c_char,
                len: usize,
            ) -> c_int,
        >,
        data: *mut c_void,
    ) -> c_int;
    pub fn pw_context_get_support(
        context: *mut pw_context,
        n_support: *mut u32,
    ) -> *const spa_support;
    pub fn pw_context_get_main_loop(context: *mut pw_context) -> *mut pw_loop;
    pub fn pw_context_get_data_loop(context: *mut pw_context) -> *mut pw_data_loop;
    pub fn pw_context_acquire_loop(
        context: *mut pw_context,
        props: *const spa_dict,
    ) -> *mut pw_loop;
    pub fn pw_context_release_loop(context: *mut pw_context, r#loop: *mut pw_loop);
    pub fn pw_context_get_work_queue(context: *mut pw_context) -> *mut pw_work_queue;
    pub fn pw_context_get_mempool(context: *mut pw_context) -> *mut pw_mempool;
    pub fn pw_context_for_each_global(
        context: *mut pw_context,
        callback: Option<extern "C" fn(data: *mut c_void, global: *mut pw_global) -> c_int>,
        data: *mut c_void,
    ) -> c_int;
    pub fn pw_context_find_global(context: *mut pw_context, id: u32) -> *mut pw_global;
    pub fn pw_context_add_spa_lib(
        context: *mut pw_context,
        factory_name: *const c_char,
    ) -> *const c_char;
    pub fn pw_context_load_spa_handle(
        context: *mut pw_context,
        factory_name: *const c_char,
        info: *const spa_dict,
    ) -> *mut spa_handle;
    pub fn pw_context_register_export_type(
        context: *mut pw_context,
        r#type: *mut pw_export_type,
    ) -> c_int;
    pub fn pw_context_find_export_type(
        context: *mut pw_context,
        r#type: *const c_char,
    ) -> *const pw_export_type;
    pub fn pw_context_set_object(
        context: *mut pw_context,
        r#type: *const c_char,
        value: *mut c_void,
    ) -> c_int;
    pub fn pw_context_get_object(context: *mut pw_context, r#type: *const c_char) -> *mut c_void;

    pub fn pw_protocol_new(
        context: *mut pw_context,
        name: *const c_char,
        user_data_size: usize,
    ) -> *mut pw_protocol;
    pub fn pw_protocol_destroy(protocol: *mut pw_protocol);
    pub fn pw_protocol_get_context(protocol: *mut pw_protocol) -> *mut pw_context;
    pub fn pw_protocol_get_user_data(protocol: *mut pw_protocol) -> *mut c_void;
    // pub fn pw_protocol_get_implementation(...)
    pub fn pw_protocol_get_extension(protocol: *mut pw_protocol) -> *const c_void;
    pub fn pw_protocol_add_listener(
        protocol: *mut pw_protocol,
        listener: *mut spa_hook,
        events: *const pw_protocol_events,
        data: *mut c_void,
    );
    pub fn pw_protocol_add_marshal(
        protocol: *mut pw_protocol,
        marshal: *const pw_protocol_marshal,
    ) -> c_int;
    pub fn pw_protocol_get_marshal(
        protocol: *mut pw_protocol,
        r#type: *const c_char,
        version: u32,
        flags: u32,
    ) -> *const pw_protocol_marshal;
    pub fn pw_context_find_protocol(
        context: *mut pw_context,
        name: *const c_char,
    ) -> *mut pw_protocol;

    pub fn pw_proxy_new(
        factory: *mut pw_proxy,
        r#type: *const c_char,
        version: u32,
        user_data_size: usize,
    ) -> *mut pw_proxy;
    pub fn pw_proxy_add_listener(
        proxy: *mut pw_proxy,
        listener: *mut spa_hook,
        events: *const pw_proxy_events,
        data: *mut c_void,
    );
    pub fn pw_proxy_add_object_listener(
        proxy: *mut pw_proxy,
        listener: *mut spa_hook,
        funcs: *const c_void,
        data: *mut c_void,
    );
    pub fn pw_proxy_destroy(proxy: *mut pw_proxy);
    pub fn pw_proxy_ref(proxy: *mut pw_proxy);
    pub fn pw_proxy_unref(proxy: *mut pw_proxy);
    pub fn pw_proxy_get_user_data(proxy: *mut pw_proxy) -> *mut c_void;
    pub fn pw_proxy_get_id(proxy: *mut pw_proxy) -> u32;
    pub fn pw_proxy_get_type(proxy: *mut pw_proxy, version: *mut u32) -> *const c_char;
    pub fn pw_proxy_get_protocol(proxy: *mut pw_proxy) -> *mut pw_protocol;
    pub fn pw_proxy_sync(proxy: *mut pw_proxy, seq: c_int) -> c_int;
    pub fn pw_proxy_set_bound_id(proxy: *mut pw_proxy, global_id: u32) -> c_int;
    pub fn pw_proxy_get_bound_id(proxy: *mut pw_proxy) -> u32;
    pub fn pw_proxy_error(proxy: *mut pw_proxy, error: *const c_char) -> c_int;
    pub fn pw_proxy_get_object_listeners(proxy: *mut pw_proxy) -> *mut spa_hook_list;
    pub fn pw_proxy_get_marshal(proxy: *mut pw_proxy) -> *const pw_protocol_marshal;
    pub fn pw_proxy_install_marshal(proxy: *mut pw_proxy, implementor: bool) -> c_int;

    pub fn pw_core_info_update(
        info: *mut pw_core_info,
        update: *const pw_core_info,
    ) -> *mut pw_core_info;
    pub fn pw_core_info_merge(
        info: *mut pw_core_info,
        update: *const pw_core_info,
        reset: bool,
    ) -> *mut pw_core_info;
    pub fn pw_core_info_free(info: *mut pw_core_info);

    pub fn pw_context_connect(
        context: *mut pw_context,
        properties: *mut pw_properties,
        user_data_size: usize,
    ) -> *mut pw_core;
    pub fn pw_context_connect_fd(
        context: *mut pw_context,
        fd: c_int,
        properties: *mut pw_properties,
        user_data_size: usize,
    ) -> *mut pw_core;
    pub fn pw_context_connect_self(
        context: *mut pw_context,
        properties: *mut pw_properties,
        user_data_size: usize,
    ) -> *mut pw_core;
    pub fn pw_core_steal_fd(core: *mut pw_core) -> c_int;
    pub fn pw_core_set_paused(core: *mut pw_core, paused: bool) -> c_int;
    pub fn pw_core_disconnect(core: *mut pw_core) -> c_int;
    pub fn pw_core_get_user_data(core: *mut pw_core) -> *mut c_void;
    pub fn pw_core_get_client(core: *mut pw_core) -> *mut pw_client;
    pub fn pw_core_get_context(core: *mut pw_core) -> *mut pw_context;
    pub fn pw_core_get_properties(core: *mut pw_core) -> *const pw_properties;
    pub fn pw_core_update_properties(core: *mut pw_core, dict: *const spa_dict) -> c_int;
    pub fn pw_core_get_mempool(core: *mut pw_core) -> *mut pw_mempool;
    pub fn pw_core_find_proxy(core: *mut pw_core, id: u32) -> *mut pw_proxy;
    pub fn pw_core_export(
        core: *mut pw_core,
        r#type: *const c_char,
        props: *const spa_dict,
        object: *mut c_void,
        user_data_size: usize,
    ) -> *mut pw_proxy;

    pub fn pw_client_info_update(
        info: *mut pw_client_info,
        update: *const pw_client_info,
    ) -> *mut pw_client_info;
    pub fn pw_client_info_merge(
        info: *mut pw_client_info,
        update: *const pw_client_info,
        reset: bool,
    ) -> *mut pw_client_info;
    pub fn pw_client_info_free(info: *mut pw_client_info);

    pub fn pw_data_loop_new(props: *const spa_dict) -> *mut pw_data_loop;
    pub fn pw_data_loop_add_listener(
        r#loop: *mut pw_data_loop,
        listener: *mut spa_hook,
        events: *const pw_data_loop_events,
        data: *mut c_void,
    );
    pub fn pw_data_loop_wait(r#loop: *mut pw_data_loop, timeout: c_int) -> c_int;
    pub fn pw_data_loop_exit(r#loop: *mut pw_data_loop);
    pub fn pw_data_loop_get_loop(r#loop: *mut pw_data_loop) -> *mut pw_loop;
    pub fn pw_data_loop_get_name(r#loop: *mut pw_data_loop) -> *const c_char;
    pub fn pw_data_loop_get_class(r#loop: *mut pw_data_loop) -> *const c_char;
    pub fn pw_data_loop_destroy(r#loop: *mut pw_data_loop);
    pub fn pw_data_loop_start(r#loop: *mut pw_data_loop) -> c_int;
    pub fn pw_data_loop_stop(r#loop: *mut pw_data_loop) -> c_int;
    pub fn pw_data_loop_in_thread(r#loop: *mut pw_data_loop) -> bool;
    pub fn pw_data_loop_get_thread(r#loop: *mut pw_data_loop) -> *mut spa_thread;
    pub fn pw_data_loop_invoke(
        r#loop: *mut pw_data_loop,
        func: Option<spa_invoke_func_t>,
        seq: u32,
        data: *const c_void,
        size: usize,
        block: bool,
        user_data: *mut c_void,
    ) -> c_int;
    pub fn pw_data_loop_set_thread_utils(r#loop: *mut pw_data_loop, r#impl: *mut spa_thread_utils);

    pub fn pw_mempool_new(props: *mut pw_properties) -> *mut pw_mempool;
    pub fn pw_mempool_add_listener(
        pool: *mut pw_mempool,
        listener: *mut spa_hook,
        events: *const pw_mempool_events,
        data: *mut c_void,
    );
    pub fn pw_mempool_clear(pool: *mut pw_mempool);
    pub fn pw_mempool_destroy(pool: *mut pw_mempool);
    pub fn pw_mempool_alloc(
        pool: *mut pw_mempool,
        flags: pw_memblock_flags,
        r#type: u32,
        size: usize,
    ) -> *mut pw_memblock;
    pub fn pw_mempool_import_block(
        pool: *mut pw_mempool,
        mem: *mut pw_memblock,
    ) -> *mut pw_memblock;
    pub fn pw_mempool_import(
        pool: *mut pw_mempool,
        flags: pw_memblock_flags,
        r#type: u32,
        fd: c_int,
    ) -> *mut pw_memblock;
    pub fn pw_memblock_free(mem: *mut pw_memblock);
    pub fn pw_mempool_remove_id(pool: *mut pw_mempool, id: u32) -> c_int;
    pub fn pw_mempool_find_ptr(pool: *mut pw_mempool, ptr: *const c_void) -> *mut pw_memblock;
    pub fn pw_mempool_find_id(pool: *mut pw_mempool, id: u32) -> *mut pw_memblock;
    pub fn pw_mempool_find_fd(pool: *mut pw_mempool, fd: c_int) -> *mut pw_memblock;
    pub fn pw_memblock_map(
        block: *mut pw_memblock,
        flags: pw_memmap_flags,
        offset: u32,
        size: u32,
        tag: *mut u32,
    ) -> *mut pw_memmap;
    pub fn pw_mempool_map_id(
        pool: *mut pw_mempool,
        id: u32,
        flags: pw_memmap_flags,
        offset: u32,
        size: u32,
        tag: *mut u32,
    ) -> *mut pw_memmap;
    pub fn pw_mempool_import_map(
        pool: *mut pw_mempool,
        other: *mut pw_mempool,
        data: *mut c_void,
        size: u32,
        tag: *mut u32,
    ) -> *mut pw_memmap;
    pub fn pw_mempool_find_tag(pool: *mut pw_mempool, tag: *mut u32, size: usize)
    -> *mut pw_memmap;
    pub fn pw_memmap_free(map: *mut pw_memmap) -> c_int;

    pub fn pw_work_queue_new(r#loop: *mut pw_loop) -> *mut pw_work_queue;
    pub fn pw_work_queue_destroy(queue: *mut pw_work_queue);
    pub fn pw_work_queue_add(
        queue: *mut pw_work_queue,
        obj: *mut c_void,
        res: c_int,
        func: Option<pw_work_func_t>,
        data: *mut c_void,
    ) -> u32;
    pub fn pw_work_queue_cancel(queue: *mut pw_work_queue, obj: *mut c_void, id: u32) -> c_int;
    pub fn pw_work_queue_complete(
        queue: *mut pw_work_queue,
        obj: *mut c_void,
        seq: u32,
        res: c_int,
    ) -> c_int;

    pub fn pw_device_info_update(
        info: *mut pw_device_info,
        update: *const pw_device_info,
    ) -> *mut pw_device_info;
    pub fn pw_device_info_merge(
        info: *mut pw_device_info,
        update: *const pw_device_info,
        reset: bool,
    ) -> *mut pw_device_info;
    pub fn pw_device_info_free(info: *mut pw_device_info);
}
