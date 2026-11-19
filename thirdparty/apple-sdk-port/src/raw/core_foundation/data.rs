use super::{CFAllocatorRef, CFIndex, CFOptionFlags, CFRange, CFTypeID};

#[repr(C)]
pub struct __CFData(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
pub type CFDataRef = *const __CFData;
pub type CFMutableDataRef = *mut __CFData;

pub type CFDataSearchFlags = CFOptionFlags;
pub const kCFDataSearchBackwards: CFDataSearchFlags = 1 << 0;
pub const kCFDataSearchAnchored: CFDataSearchFlags = 1 << 1;

unsafe extern "C" {
    pub fn CFDataGetTypeID() -> CFTypeID;

    pub fn CFDataCreate(allocator: CFAllocatorRef, bytes: *const u8, length: CFIndex) -> CFDataRef;
    pub fn CFDataCreateWithBytesNoCopy(
        allocator: CFAllocatorRef,
        bytes: *const u8,
        length: CFIndex,
        bytes_deallocator: CFAllocatorRef,
    ) -> CFDataRef;
    pub fn CFDataCreateCopy(allocator: CFAllocatorRef, data: CFDataRef) -> CFDataRef;
    pub fn CFDataCreateMutable(allocator: CFAllocatorRef, capacity: CFIndex) -> CFMutableDataRef;
    pub fn CFDataCreateMutableCopy(
        allocator: CFAllocatorRef,
        capacity: CFIndex,
        data: CFDataRef,
    ) -> CFMutableDataRef;
    pub fn CFDataGetLength(data: CFDataRef) -> CFIndex;
    pub fn CFDataGetBytePtr(data: CFDataRef) -> *const u8;
    pub fn CFDataGetMutableBytePtr(data: CFMutableDataRef) -> *mut u8;
    pub fn CFDataGetBytes(data: CFDataRef, range: CFRange, buffer: *mut u8);
    pub fn CFDataSetLength(data: CFMutableDataRef, length: CFIndex);
    pub fn CFDataIncreaseLength(data: CFMutableDataRef, extra_length: CFIndex);
    pub fn CFDataAppendBytes(data: CFMutableDataRef, bytes: *const u8, length: CFIndex);
    pub fn CFDataReplaceBytes(
        data: CFMutableDataRef,
        range: CFRange,
        new_bytes: *const u8,
        new_length: CFIndex,
    );
    pub fn CFDataDeleteBytes(data: CFMutableDataRef, range: CFRange);

    pub fn CFDataFind(
        data: CFDataRef,
        data_to_find: CFDataRef,
        search_range: CFRange,
        compare_options: CFDataSearchFlags,
    ) -> CFRange;
}
