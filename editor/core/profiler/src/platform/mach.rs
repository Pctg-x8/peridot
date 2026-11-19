#![allow(non_upper_case_globals, non_camel_case_types)]

pub type kern_return_t = core::ffi::c_int;
pub const KERN_SUCCESS: kern_return_t = 0;

pub type integer_t = i32;
pub type natural_t = u32;
pub type mach_port_t = natural_t;
pub type task_name_t = mach_port_t;
pub type task_flavor_t = natural_t;
pub type task_info_t = *mut integer_t;
pub type mach_msg_type_number_t = natural_t;

pub type mach_vm_size_t = u64;
pub type mach_vm_address_t = u64;

pub const TASK_VM_INFO: core::ffi::c_uint = 22;
#[derive(Debug)]
#[repr(C)]
pub struct task_vm_info {
    pub virtual_size: mach_vm_size_t,
    pub region_count: integer_t,
    pub page_size: integer_t,
    pub resident_size: mach_vm_size_t,
    pub resident_size_peak: mach_vm_size_t,
    pub device: mach_vm_size_t,
    pub device_peak: mach_vm_size_t,
    pub internal: mach_vm_size_t,
    pub internal_peak: mach_vm_size_t,
    pub external: mach_vm_size_t,
    pub external_peak: mach_vm_size_t,
    pub reusable: mach_vm_size_t,
    pub reusable_peak: mach_vm_size_t,
    pub purgeable_volatile_pmap: mach_vm_size_t,
    pub purgeable_volatile_resident: mach_vm_size_t,
    pub purgeable_volatile_virtual: mach_vm_size_t,
    pub compressed: mach_vm_size_t,
    pub compressed_peak: mach_vm_size_t,
    pub compressed_lifetime: mach_vm_size_t,
    pub phys_footprint: mach_vm_size_t,
    pub min_address: mach_vm_address_t,
    pub max_address: mach_vm_address_t,
    pub lerger_phys_footprint_peak: i64,
    pub ledger_purgeable_nonvolatile: i64,
    pub ledger_purgeable_novolatile_compressed: i64,
    pub ledger_purgeable_volatile: i64,
    pub ledger_pudgeable_volatile_compressed: i64,
    pub ledger_tag_network_nonvolatile: i64,
    pub ledger_tag_network_nonvolatile_compressed: i64,
    pub ledger_tag_network_volatile: i64,
    pub ledger_tag_network_volatile_compressed: i64,
    pub ledger_tag_media_footprint: i64,
    pub ledger_tag_media_footprint_compressed: i64,
    pub ledger_tag_media_nofootprint: i64,
    pub ledger_tag_media_nofootprint_compressed: i64,
    pub ledger_tag_graphics_footprint: i64,
    pub ledger_tag_graphics_footprint_compressed: i64,
    pub ledger_tag_graphics_nofootprint: i64,
    pub ledger_tag_graphics_nofootprint_compressed: i64,
    pub ledger_tag_neural_footprint: i64,
    pub ledger_tag_neural_footprint_compressed: i64,
    pub ledger_tag_neural_nofootprint: i64,
    pub ledger_tag_neural_nofootprint_compressed: i64,
    pub limit_bytes_remaining: u64,
    pub decompressions: integer_t,
    pub ledger_swapins: i64,
    pub ledger_tag_neural_nofootprint_total: i64,
    pub ledger_tag_neural_nofootprint_peak: i64,
}
pub type task_vm_info_data_t = task_vm_info;
pub type task_vm_info_t = *mut task_vm_info;
pub const TASK_VM_INFO_COUNT: mach_msg_type_number_t =
    (size_of::<task_vm_info_data_t>() / size_of::<natural_t>()) as _;

unsafe extern "C" {
    pub static mach_task_self_: mach_port_t;

    pub fn task_info(
        target_task: task_name_t,
        flavor: task_flavor_t,
        task_info_out: task_info_t,
        task_info_outCnt: *mut mach_msg_type_number_t,
    ) -> kern_return_t;
}
