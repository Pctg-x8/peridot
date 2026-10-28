//! #include <linux/io_uring.h>
#![allow(non_camel_case_types)]

/// IO submission data structure (Submission Queue Entry)
#[repr(C)]
#[derive(Clone)]
pub struct io_uring_sqe {
    /// type of operation for this sqe
    pub opcode: u8,
    /// IOSQE_ flags
    pub flags: u8,
    /// ioprio for the request
    pub ioprio: u16,
    /// file descriptor to do IO on
    pub fd: i32,
    pub union1: io_uring_sqe_union1,
    pub union2: io_uring_sqe_union2,
    /// buffer size or number of iovecs
    pub len: u32,
    pub union3: io_uring_sqe_union3,
    /// data tobe passed back at completion time
    pub user_data: u64,
    /// pack this to avoid bogus arm OABI complaints
    pub union4: io_uring_sqe_union4,
    /// personality to use, if used
    pub personality: u16,
    pub union5: io_uring_sqe_union5,
    pub union6: io_uring_sqe_union6,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union io_uring_sqe_union1 {
    /// offset into file
    pub off: u64,
    pub addr2: u64,
    pub struct1: io_uring_sqe_union1_struct1,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct io_uring_sqe_union1_struct1 {
    pub cmp_op: u32,
    pub __pad1: u32,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union io_uring_sqe_union2 {
    /// pointer to buffer or iovecs
    pub addr: u64,
    pub splice_off_in: u64,
    pub struct1: io_uring_sqe_union2_struct1,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct io_uring_sqe_union2_struct1 {
    pub level: u32,
    pub optname: u32,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union io_uring_sqe_union3 {
    pub rw_flags: core::ffi::c_int,
    pub fsync_flags: u32,
    /// compatibility
    pub poll_events: u16,
    /// word-reserved for BE
    pub poll32_events: u32,
    pub sync_range_flags: u32,
    pub msg_flags: u32,
    pub timeout_flags: u32,
    pub accept_flags: u32,
    pub cancel_flags: u32,
    pub open_flags: u32,
    pub statx_flags: u32,
    pub fadvise_advice: u32,
    pub splice_flags: u32,
    pub rename_flags: u32,
    pub unlink_flags: u32,
    pub hardlink_flags: u32,
    pub xattr_flags: u32,
    pub msg_ring_flags: u32,
    pub uring_cmd_flags: u32,
    pub waitid_flags: u32,
    pub futex_flags: u32,
    pub install_fd_flags: u32,
    pub nop_flags: u32,
}

#[repr(C, packed)]
#[derive(Clone, Copy)]
pub union io_uring_sqe_union4 {
    /// index into fixed buffers, if used
    pub buf_index: u16,
    /// for grouped buffer selection
    pub buf_group: u16,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union io_uring_sqe_union5 {
    pub splice_fd_in: i32,
    pub file_index: u32,
    pub optlen: u32,
    pub struct1: io_uring_sqe_union5_struct1,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct io_uring_sqe_union5_struct1 {
    pub addr_len: u16,
    pub __pad3: [u16; 1],
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union io_uring_sqe_union6 {
    pub struct1: io_uring_sqe_union6_struct1,
    pub optval: u64,
    /// If the ring is initialized with IORING_SETUP_SQE128, then this field if used for 80 bytes of arbitrary command data
    pub cmd: [u8; 0],
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct io_uring_sqe_union6_struct1 {
    pub addr3: u64,
    pub __pad2: [u64; 1],
}

pub const IORING_FILE_INDEX_ALLOC: u32 = !0;

pub type io_uring_sqe_flags_bit = u8;
pub const IOSQE_FIXED_FILE_BIT: io_uring_sqe_flags_bit = 0;
pub const IOSQE_IO_DRAIN_BIT: io_uring_sqe_flags_bit = 1;
pub const IOSQE_IO_LINK_BIT: io_uring_sqe_flags_bit = 2;
pub const IOSQE_IO_HARDLINK_BIT: io_uring_sqe_flags_bit = 3;
pub const IOSQE_ASYNC_BIT: io_uring_sqe_flags_bit = 4;
pub const IOSQE_BUFFER_SELECT_BIT: io_uring_sqe_flags_bit = 5;
pub const IOSQE_CQE_SKIP_SUCCESS_BIT: io_uring_sqe_flags_bit = 6;

pub const IOSQE_FIXED_FILE: u8 = 1 << IOSQE_FIXED_FILE_BIT;
pub const IOSQE_IO_DRAIN: u8 = 1 << IOSQE_IO_DRAIN_BIT;
pub const IOSQE_IO_LINK: u8 = 1 << IOSQE_IO_LINK_BIT;
pub const IOSQE_IO_HARDLINK: u8 = 1 << IOSQE_IO_HARDLINK_BIT;
pub const IOSQE_ASYNC: u8 = 1 << IOSQE_ASYNC_BIT;
pub const IOSQE_BUFFER_SELECT: u8 = 1 << IOSQE_BUFFER_SELECT_BIT;
pub const IOSQE_CQE_SKIP_SUCCESS: u8 = 1 << IOSQE_CQE_SKIP_SUCCESS_BIT;

pub const IORING_SETUP_IOPOLL: u32 = 1 << 0;
pub const IORING_SETUP_SQPOLL: u32 = 1 << 1;
pub const IORING_SETUP_SQ_AFF: u32 = 1 << 2;
pub const IORING_SETUP_CQSIZE: u32 = 1 << 3;
pub const IORING_SETUP_CLAMP: u32 = 1 << 4;
pub const IORING_SETUP_ATTACH_WQ: u32 = 1 << 5;
pub const IORING_SETUP_R_DISABLED: u32 = 1 << 6;
pub const IORING_SETUP_SUBMIT_ALL: u32 = 1 << 7;
pub const IORING_SETUP_COOP_TASKRUN: u32 = 1 << 8;
pub const IORING_SETUP_TASKRUN_FLAG: u32 = 1 << 9;
pub const IORING_SETUP_SQE128: u32 = 1 << 10;
pub const IORING_SETUP_CQE32: u32 = 1 << 11;
pub const IORING_SETUP_SINGLE_ISSUER: u32 = 1 << 12;
pub const IORING_SETUP_DEFER_TASKRUN: u32 = 1 << 13;
pub const IORING_SETUP_NO_MMAP: u32 = 1 << 14;
pub const IORING_SETUP_REGISTERED_FD_ONLY: u32 = 1 << 15;
pub const IORING_SETUP_NO_SQARRAY: u32 = 1 << 16;

pub type io_uring_op = core::ffi::c_int;
pub const IORING_OP_NOP: io_uring_op = 0;
pub const IORING_OP_READV: io_uring_op = 1;
pub const IORING_OP_WRITEV: io_uring_op = 2;
pub const IORING_OP_FSYNC: io_uring_op = 3;
pub const IORING_OP_READ_FIXED: io_uring_op = 4;
pub const IORING_OP_WRITE_FIXED: io_uring_op = 5;
pub const IORING_OP_POLL_ADD: io_uring_op = 6;
pub const IORING_OP_POLL_REMOVE: io_uring_op = 7;
pub const IORING_OP_SYNC_FILE_RANGE: io_uring_op = 8;
pub const IORING_OP_SENDMSG: io_uring_op = 9;
pub const IORING_OP_RECVMSG: io_uring_op = 10;
pub const IORING_OP_TIMEOUT: io_uring_op = 11;
pub const IORING_OP_TIMEOUT_REMOVE: io_uring_op = 12;
pub const IORING_OP_ACCEPT: io_uring_op = 13;
pub const IORING_OP_ASYNC_CANCEL: io_uring_op = 14;
pub const IORING_OP_LINK_TIMEOUT: io_uring_op = 15;
pub const IORING_OP_CONNECT: io_uring_op = 16;
pub const IORING_OP_FALLOCATE: io_uring_op = 17;
pub const IORING_OP_OPENAT: io_uring_op = 18;
pub const IORING_OP_CLOSE: io_uring_op = 19;
pub const IORING_OP_FILES_UPDATE: io_uring_op = 20;
pub const IORING_OP_STATX: io_uring_op = 21;
pub const IORING_OP_READ: io_uring_op = 22;
pub const IORING_OP_WRITE: io_uring_op = 23;
pub const IORING_OP_FADVISE: io_uring_op = 24;
pub const IORING_OP_MADVISE: io_uring_op = 25;
pub const IORING_OP_SEND: io_uring_op = 26;
pub const IORING_OP_RECV: io_uring_op = 27;
pub const IORING_OP_OPENAT2: io_uring_op = 28;
pub const IORING_OP_EPOLL_CTL: io_uring_op = 29;
pub const IORING_OP_SPLICE: io_uring_op = 30;
pub const IORING_OP_PROVIDE_BUFFERS: io_uring_op = 31;
pub const IORING_OP_REMOVE_BUFFERS: io_uring_op = 32;
pub const IORING_OP_TEE: io_uring_op = 33;
pub const IORING_OP_SHUTDOWN: io_uring_op = 34;
pub const IORING_OP_RENAMEAT: io_uring_op = 35;
pub const IORING_OP_UNLINKAT: io_uring_op = 36;
pub const IORING_OP_MKDIRAT: io_uring_op = 37;
pub const IORING_OP_SYMLINKAT: io_uring_op = 38;
pub const IORING_OP_LINKAT: io_uring_op = 39;
pub const IORING_OP_MSG_RING: io_uring_op = 40;
pub const IORING_OP_FSETXATTR: io_uring_op = 41;
pub const IORING_OP_SETXATTR: io_uring_op = 42;
pub const IORING_OP_FGETXATTR: io_uring_op = 43;
pub const IORING_OP_GETXATTR: io_uring_op = 44;
pub const IORING_OP_SOCKET: io_uring_op = 45;
pub const IORING_OP_URING_CMD: io_uring_op = 46;
pub const IORING_OP_SEND_ZC: io_uring_op = 47;
pub const IORING_OP_SENDMSG_ZC: io_uring_op = 48;
pub const IORING_OP_READ_MULTISHOT: io_uring_op = 49;
pub const IORING_OP_WAITID: io_uring_op = 50;
pub const IORING_OP_FUTEX_WAIT: io_uring_op = 51;
pub const IORING_OP_FUTEX_WAKE: io_uring_op = 52;
pub const IORING_OP_FUTEX_WAITV: io_uring_op = 53;
pub const IORING_OP_FIXED_FD_INSTALL: io_uring_op = 54;
pub const IORING_OP_FTRUNCATE: io_uring_op = 55;
pub const IORING_OP_BIND: io_uring_op = 56;
pub const IORING_OP_LISTEN: io_uring_op = 57;
pub const IORING_OP_LAST: io_uring_op = IORING_OP_LISTEN + 1;

pub const IORING_URING_CMD_FIXED: u32 = 1 << 0;
pub const IORING_URING_CMD_MASK: u32 = IORING_URING_CMD_FIXED;

pub const IORING_FSYNC_DATASYNC: u32 = 1 << 0;

pub const IORING_TIMEOUT_ABS: u32 = 1 << 0;
pub const IORING_TIMEOUT_UPDATE: u32 = 1 << 1;
pub const IORING_TIMEOUT_BOOTTIME: u32 = 1 << 2;
pub const IORING_TIMEOUT_REALTIME: u32 = 1 << 3;
pub const IORING_LINK_TIMEOUT_UPDATE: u32 = 1 << 4;
pub const IORING_TIMEOUT_ETIME_SUCCESS: u32 = 1 << 5;
pub const IORING_TIMEOUT_MULTISHOT: u32 = 1 << 6;
pub const IORING_TIMEOUT_CLOCK_MASK: u32 = IORING_TIMEOUT_BOOTTIME | IORING_TIMEOUT_REALTIME;
pub const IORING_TIMEOUT_UPDATE_MASK: u32 = IORING_TIMEOUT_UPDATE | IORING_LINK_TIMEOUT_UPDATE;

pub const SPLICE_F_FD_IN_FIXED: u32 = 1 << 31;

pub const IORING_POLL_ADD_MULTI: u32 = 1 << 0;
pub const IORING_POLL_UPDATE_EVENTS: u32 = 1 << 1;
pub const IORING_POLL_UPDATE_USER_DATA: u32 = 1 << 2;
pub const IORING_POLL_ADD_LEVEL: u32 = 1 << 3;

pub const IORING_ASYNC_CANCEL_ALL: u32 = 1 << 0;
pub const IORING_ASYNC_CANCEL_FD: u32 = 1 << 1;
pub const IORING_ASYNC_CANCEL_ANY: u32 = 1 << 2;
pub const IORING_ASYNC_CANCEL_FD_FIXED: u32 = 1 << 3;
pub const IORING_ASYNC_CANCEL_USERDATA: u32 = 1 << 4;
pub const IORING_ASYNC_CANCEL_OP: u32 = 1 << 5;

pub const IORING_RECVSEND_POLL_FIRST: u32 = 1 << 0;
pub const IORING_RECV_MULTISHOT: u32 = 1 << 1;
pub const IORING_RECVSEND_FIXED_BUF: u32 = 1 << 2;
pub const IORING_SEND_ZC_REPORT_USAGE: u32 = 1 << 3;
pub const IORING_RECVSEND_BUNDLE: u32 = 1 << 4;

pub const IORING_NOTIF_USAGE_ZC_COPIED: u32 = 1 << 31;

pub const IORING_ACCEPT_MULTISHOT: u32 = 1 << 0;
pub const IORING_ACCEPT_DONTWAIT: u32 = 1 << 1;
pub const IORING_ACCEPT_POLL_FIRST: u32 = 1 << 2;

pub type io_uring_msg_ring_flags = core::ffi::c_int;
pub const IORING_MSG_DATA: io_uring_msg_ring_flags = 0;
pub const IORING_MSG_SEND_FD: io_uring_msg_ring_flags = 1;

pub const IORING_MSG_RING_CQE_SKIP: u32 = 1 << 0;
pub const IORING_MSG_RING_FLAGS_PASS: u32 = 1 << 1;

pub const IORING_FIXED_FD_NO_CLOEXEC: u32 = 1 << 0;

pub const IORING_NOP_INJECT_RESULT: u32 = 1 << 0;

/// IO completion data structure (Completion Queue Entry)
#[repr(C)]
#[derive(Clone)]
pub struct io_uring_cqe {
    /// sqe->user_data value passed back
    pub user_data: u64,
    /// result code for this event
    pub res: i32,
    pub flags: u32,
    /// If the ring is initialized with IORING_SETUP_CQE32, then this field contains
    /// 16-bytes of padding, doubling the size of the CQE.
    pub big_cqe: [u64; 0],
}

pub const IORING_CQE_F_BUFFER: u32 = 1 << 0;
pub const IORING_CQE_F_MORE: u32 = 1 << 1;
pub const IORING_CQE_F_SOCK_NONEMPTY: u32 = 1 << 2;
pub const IORING_CQE_F_NOTIF: u32 = 1 << 3;
pub const IORING_CQE_F_BUF_MORE: u32 = 1 << 4;

pub const IORING_CQE_BUFFER_SHIFT: u32 = 16;

pub const IORING_OFF_SQ_RING: u64 = 0;
pub const IORING_OFF_CQ_RING: u64 = 0x0800_0000;
pub const IORING_OFF_SQES: u64 = 0x1000_0000;
pub const IORING_OFF_PBUF_RING: u64 = 0x8000_0000;
pub const IORING_OFF_PBUF_SHIFT: u64 = 16;
pub const IORING_OFF_MMAP_MASK: u64 = 0xf800_0000;

/// Filled with the offset for mmap(2)
#[repr(C)]
#[derive(Clone)]
pub struct io_sqring_offsets {
    pub head: u32,
    pub tail: u32,
    pub ring_mask: u32,
    pub ring_entries: u32,
    pub flags: u32,
    pub dropped: u32,
    pub array: u32,
    pub resv1: u32,
    pub user_addr: u64,
}

pub const IORING_SQ_NEED_WAKEUP: u32 = 1 << 0;
pub const IORING_SQ_CQ_OVERFLOW: u32 = 1 << 1;
pub const IORING_SQ_TASKRUN: u32 = 1 << 2;

#[repr(C)]
#[derive(Clone)]
pub struct io_cqring_offsets {
    pub head: u32,
    pub tail: u32,
    pub ring_mask: u32,
    pub ring_entries: u32,
    pub overflow: u32,
    pub cqes: u32,
    pub flags: u32,
    pub resv1: u32,
    pub user_addr: u64,
}

pub const IORING_CQ_EVENTFD_DISABLED: u32 = 1 << 0;

pub const IORING_ENTER_GETEVENTS: u32 = 1 << 0;
pub const IORING_ENTER_SQ_WAKEUP: u32 = 1 << 1;
pub const IORING_ENTER_SQ_WAIT: u32 = 1 << 2;
pub const IORING_ENTER_EXT_ARG: u32 = 1 << 3;
pub const IORING_ENTER_REGISTERED_RING: u32 = 1 << 4;
pub const IORING_ENTER_ABS_TIMER: u32 = 1 << 5;

/// Passed in for io_uring_setup(2). Copied back with updated info on success
#[repr(C)]
#[derive(Clone)]
pub struct io_uring_params {
    pub sq_entries: u32,
    pub cq_entries: u32,
    pub flags: u32,
    pub sq_thread_cpu: u32,
    pub sq_thread_idle: u32,
    pub features: u32,
    pub wq_fd: u32,
    pub resv: [u32; 3],
    pub sq_off: io_sqring_offsets,
    pub cq_off: io_cqring_offsets,
}

pub const IORING_FEAT_SINGLE_MMAP: u32 = 1 << 0;
pub const IORING_FEAT_NODROP: u32 = 1 << 1;
pub const IORING_FEAT_SUBMIT_STABLE: u32 = 1 << 2;
pub const IORING_FEAT_RW_CUR_POS: u32 = 1 << 3;
pub const IORING_FEAT_CUR_PERSONALITY: u32 = 1 << 4;
pub const IORING_FEAT_FAST_POLL: u32 = 1 << 5;
pub const IORING_FEAT_POLL_32BITS: u32 = 1 << 6;
pub const IORING_FEAT_SQPOLL_NONFIXED: u32 = 1 << 7;
pub const IORING_FEAT_EXT_ARG: u32 = 1 << 8;
pub const IORING_FEAT_NATIVE_WORKERS: u32 = 1 << 9;
pub const IORING_FEAT_RSRC_TAGS: u32 = 1 << 10;
pub const IORING_FEAT_CQE_SKIP: u32 = 1 << 11;
pub const IORING_FEAT_LINKED_FILE: u32 = 1 << 12;
pub const IORING_FEAT_REG_REG_RING: u32 = 1 << 13;
pub const IORING_FEAT_RECVSEND_BUNDLE: u32 = 1 << 14;
pub const IORING_FEAT_MIN_TIMEOUT: u32 = 1 << 15;

pub type io_uring_register_op = u32;
pub const IORING_REGISTER_BUFFERS: io_uring_register_op = 0;
pub const IORING_UNREGSITER_BUFFERS: io_uring_register_op = 1;
pub const IORING_REGISTER_FILES: io_uring_register_op = 2;
pub const IORING_UNREGISTER_FILES: io_uring_register_op = 3;
pub const IORING_REGISTER_EVENTFD: io_uring_register_op = 4;
pub const IORING_UNREGISTER_EVENTFD: io_uring_register_op = 5;
pub const IORING_REGISTER_FILES_UPDATE: io_uring_register_op = 6;
pub const IORING_REGISTER_EVENTFD_ASYNC: io_uring_register_op = 7;
pub const IORING_REGISTER_PROBE: io_uring_register_op = 8;
pub const IORING_REGISTER_PERSONALITY: io_uring_register_op = 9;
pub const IORING_UNREGISTER_PERSONALITY: io_uring_register_op = 10;
pub const IORING_REGISTER_RESTRICTIONS: io_uring_register_op = 11;
pub const IORING_REGISTER_ENABLE_RINGS: io_uring_register_op = 12;
pub const IORING_REGISTER_FILES2: io_uring_register_op = 13;
pub const IORING_REGISTER_FILES_UPDATE2: io_uring_register_op = 14;
pub const IORING_REGISTER_BUFFERS2: io_uring_register_op = 15;
pub const IORING_REGISTER_BUFFERS_UPDATE: io_uring_register_op = 16;
pub const IORING_REGISTER_IOWQ_AFF: io_uring_register_op = 17;
pub const IORING_UNREGISTER_IOWQ_AFF: io_uring_register_op = 18;
pub const IORING_REGISTER_IOWQ_MAX_WORKERS: io_uring_register_op = 19;
pub const IORING_REGISTER_RING_FDS: io_uring_register_op = 20;
pub const IORING_UNREGISTER_RING_FDS: io_uring_register_op = 21;
pub const IORING_REGISTER_PBUF_RING: io_uring_register_op = 22;
pub const IORING_UNREGISTER_PBUF_RING: io_uring_register_op = 23;
pub const IORING_REGISTER_SYNC_CANCEL: io_uring_register_op = 24;
pub const IORING_REGISTER_FILE_ALLOC_RANGE: io_uring_register_op = 25;
pub const IORING_REGISTER_PBUF_STATUS: io_uring_register_op = 26;
pub const IORING_REGISTER_NAPI: io_uring_register_op = 27;
pub const IORING_UNREGISTER_NAPI: io_uring_register_op = 28;
pub const IORING_REGISTER_CLOCK: io_uring_register_op = 29;
pub const IORING_REGISTER_CLONE_BUFFERS: io_uring_register_op = 30;
pub const IORING_REGISTER_LAST: io_uring_register_op = IORING_REGISTER_CLONE_BUFFERS + 1;
pub const IORING_REGISTER_USE_REGISTERED_RING: io_uring_register_op = 1 << 31;

pub type io_wq_type = core::ffi::c_int;
pub const IO_WQ_BOUND: io_wq_type = 0;
pub const IO_WQ_UNBOUND: io_wq_type = 1;

pub const IORING_RSRC_REGISTER_SPARSE: u32 = 1 << 0;

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_rsrc_register {
    pub nr: u32,
    pub flags: u32,
    pub resv2: u64,
    pub data: u64,
    pub tags: u64,
}

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_rsrc_update {
    pub offset: u32,
    pub resv: u32,
    pub data: u64,
}

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_rsrc_update2 {
    pub offset: u32,
    pub resv: u32,
    pub data: u64,
    pub tags: u64,
    pub nr: u32,
    pub resv2: u32,
}

pub const IORING_REGISTER_FILES_SKIP: i32 = -2;

pub const IO_URING_OP_SUPPORTED: u16 = 1 << 0;

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_probe_op {
    pub op: u8,
    pub resv: u8,
    pub flags: u16,
    pub resv2: u32,
}

#[repr(C)]
pub struct io_uring_probe {
    /// last opcode supported
    pub last_op: u8,
    /// length of ops[] array below
    pub ops_len: u8,
    pub resv: u16,
    pub resv2: [u32; 3],
    pub ops: [io_uring_probe_op],
}

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_restriction {
    pub opcode: u16,
    pub union1: io_uring_restriction_union1,
    pub resv: u8,
    pub resv2: [u32; 3],
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union io_uring_restriction_union1 {
    /// IORING_RESTRICTION_REGISTER_OP
    pub register_op: u8,
    /// IORING_RESTRICTION_SQE_OP
    pub sqe_op: u8,
    /// IORING_RESTRICTION_SQE_FLAGS
    pub sqe_flags: u8,
}

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_clock_register {
    pub clockid: u32,
    pub __resv: [u32; 3],
}

pub const IORING_REGISTER_SRC_REGISTERED: core::ffi::c_int = 1;

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_clone_buffers {
    pub src_fd: u32,
    pub flags: u32,
    pub pad: [u32; 6],
}

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_buf {
    pub addr: u64,
    pub len: u32,
    pub bid: u16,
    pub resv: u16,
}

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_buf_ring {
    pub bufs: [io_uring_buf; 1],
}

pub type io_uring_register_pbuf_ring_flags = u32;
pub const IOU_PBUF_RING_MMAP: io_uring_register_pbuf_ring_flags = 1;
pub const IOU_PBUF_RING_INC: io_uring_register_pbuf_ring_flags = 2;

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_buf_reg {
    pub ring_addr: u64,
    pub ring_entries: u32,
    pub bgid: u16,
    pub flags: u16,
    pub resv: [u64; 3],
}

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_buf_status {
    pub buf_group: u32,
    pub head: u32,
    pub resv: [u32; 8],
}

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_napi {
    pub busy_poll_to: u32,
    pub prefer_busy_poll: u8,
    pub pad: [u8; 3],
    pub resv: u64,
}

pub type io_uring_register_restriction_op = u32;
pub const IORING_RESTRICTION_REGISTER_OP: io_uring_register_restriction_op = 0;
pub const IORING_RESTRICTION_SQE_OP: io_uring_register_restriction_op = 1;
pub const IORING_RESTRICTION_SQE_FLAGS_ALLOWED: io_uring_register_restriction_op = 2;
pub const IORING_RESTRICTION_SQE_FLAGS_REQUIRED: io_uring_register_restriction_op = 3;
pub const IORING_RESTRICTION_LAST: io_uring_register_restriction_op =
    IORING_RESTRICTION_SQE_FLAGS_REQUIRED + 1;

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_getevents_arg {
    pub sigmask: u64,
    pub sigmask_sz: u32,
    pub min_wait_usec: u32,
    pub ts: u64,
}

// support typedef: linux/time_types.h
#[repr(C)]
#[derive(Clone)]
pub struct kernel_timespec {
    pub tv_sec: i64,
    pub tv_nsec: core::ffi::c_longlong,
}

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_sync_cancel_reg {
    pub addr: u64,
    pub fd: i32,
    pub flags: u32,
    pub timeout: kernel_timespec,
    pub opcode: u8,
    pub pad: [u8; 7],
    pub pad2: [u64; 3],
}

#[repr(C)]
#[derive(Clone)]
pub struct io_uring_file_index_range {
    pub off: u32,
    pub len: u32,
    pub resv: u64,
}

#[repr(C)]
#[derive(Clone)]
pub struct io_urign_recvmsg_out {
    pub namelen: u32,
    pub controllen: u32,
    pub payloadlen: u32,
    pub flags: u32,
}

pub type io_uring_socket_op = u32;
pub const SOCKET_URING_OP_SIOCINQ: io_uring_socket_op = 0;
pub const SOCKET_URING_OP_SIOCOUTQ: io_uring_socket_op = 1;
pub const SOCKET_URING_OP_GETSOCKOPT: io_uring_socket_op = 2;
pub const SOCKET_URING_OP_SETSOCKOPT: io_uring_socket_op = 3;

// unistd.h: linux syscall numbers
#[allow(non_upper_case_globals)]
pub const __NR_io_uring_setup: core::ffi::c_long = 425;
#[allow(non_upper_case_globals)]
pub const __NR_io_uring_enter: core::ffi::c_long = 426;
#[allow(non_upper_case_globals)]
pub const __NR_io_uring_register: core::ffi::c_long = 427;

// libc function
#[link(name = "c")]
unsafe extern "C" {
    pub fn syscall(number: core::ffi::c_long, ...) -> core::ffi::c_long;
    pub fn close(fd: core::ffi::c_int) -> core::ffi::c_int;
}
