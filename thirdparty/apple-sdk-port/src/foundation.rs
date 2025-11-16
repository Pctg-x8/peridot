use crate::{MutableObject, Object, Owned, raw::*};

pub type Boolean = crate::raw::Boolean;
pub type UniChar = crate::raw::UniChar;

pub type Index = crate::raw::CFIndex;
pub type Range = crate::raw::CFRange;
pub type HashCode = CFHashCode;

#[repr(transparent)]
pub struct Null(__CFNull);
impl Object for Null {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        (&self.0 as *const __CFNull).cast()
    }
}
impl Null {
    pub const fn instance() -> &'static Self {
        unsafe { &*kCFNull.cast::<Self>() }
    }
}

#[repr(transparent)]
pub struct Allocator(__CFAllocator);
impl Object for Allocator {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl Allocator {
    pub const fn system_default() -> &'static Self {
        unsafe { &*kCFAllocatorSystemDefault.cast::<Self>() }
    }

    pub const fn malloc() -> &'static Self {
        unsafe { &*kCFAllocatorMalloc.cast::<Self>() }
    }

    pub const fn malloc_zone() -> &'static Self {
        unsafe { &*kCFAllocatorMallocZone.cast::<Self>() }
    }

    pub const fn null() -> &'static Self {
        unsafe { &*kCFAllocatorNull.cast::<Self>() }
    }

    pub const fn use_context() -> &'static Self {
        unsafe { &*kCFAllocatorUseContext.cast::<Self>() }
    }

    #[inline(always)]
    pub fn get_default<'a>() -> &'a Self {
        unsafe { &*CFAllocatorGetDefault().cast::<Self>() }
    }
}

#[repr(transparent)]
pub struct Data(__CFData);
impl Object for Data {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        (&self.0 as *const __CFData).cast()
    }
}
impl Data {
    #[inline(always)]
    pub fn new(allocator: Option<&Allocator>, data: &[u8]) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CFDataCreate(
                allocator.map_or(kCFAllocatorDefault, |x| &x.0),
                data.as_ptr(),
                data.len() as _,
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub unsafe fn new_no_copy(allocator: Option<&Allocator>, data: &[u8]) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CFDataCreateWithBytesNoCopy(
                allocator.map_or(kCFAllocatorDefault, |x| &x.0),
                data.as_ptr(),
                data.len() as _,
                kCFAllocatorNull,
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn len(&self) -> Index {
        unsafe { CFDataGetLength(&self.0) }
    }

    #[inline(always)]
    pub fn byte_ptr(&self) -> *const u8 {
        unsafe { CFDataGetBytePtr(&self.0) }
    }
}

pub trait ArrayValue {
    fn retain(&self, allocator: Option<&Allocator>) -> &Self;
    fn release(&self, allocator: Option<&Allocator>);
    fn copy_description(&self) -> CFStringRef;
    fn equal(&self, other: &Self) -> Boolean;
}

const fn array_value_callbacks<T: ArrayValue>() -> &'static CFArrayCallBacks {
    extern "C" fn retain<T: ArrayValue>(
        allocator: CFAllocatorRef,
        value: *const core::ffi::c_void,
    ) -> *const core::ffi::c_void {
        unsafe {
            T::retain(
                &*value.cast::<T>(),
                if allocator.is_null() {
                    None
                } else {
                    Some(&*allocator.cast())
                },
            ) as *const _ as _
        }
    }

    extern "C" fn release<T: ArrayValue>(
        allocator: CFAllocatorRef,
        value: *const core::ffi::c_void,
    ) {
        unsafe {
            T::release(
                &*value.cast::<T>(),
                if allocator.is_null() {
                    None
                } else {
                    Some(&*allocator.cast())
                },
            )
        }
    }

    extern "C" fn copy_description<T: ArrayValue>(value: *const core::ffi::c_void) -> CFStringRef {
        unsafe { T::copy_description(&*value.cast::<T>()) }
    }

    extern "C" fn equal<T: ArrayValue>(
        value1: *const core::ffi::c_void,
        value2: *const core::ffi::c_void,
    ) -> Boolean {
        unsafe { T::equal(&*value1.cast::<T>(), &*value2.cast::<T>()) }
    }

    &CFArrayCallBacks {
        version: 0,
        retain: Some(retain::<T>),
        release: Some(release::<T>),
        copy_description: Some(copy_description::<T>),
        equal: Some(equal::<T>),
    }
}

#[repr(transparent)]
pub struct Array<T>(__CFArray, core::marker::PhantomData<*const T>);
impl<T> Object for Array<T> {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl<T> core::ops::Index<Index> for Array<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, index: Index) -> &Self::Output {
        unsafe { &*CFArrayGetValueAtIndex(&self.0, index).cast::<T>() }
    }
}
impl<T> Array<T> {
    #[inline(always)]
    pub fn new(allocator: Option<&Allocator>, values: &[T]) -> Option<Owned<Self>>
    where
        T: ArrayValue,
    {
        unsafe { Self::new_raw(allocator, values, array_value_callbacks::<T>()) }
    }

    #[inline(always)]
    pub unsafe fn new_raw(
        allocator: Option<&Allocator>,
        values: &[T],
        callbacks: &CFArrayCallBacks,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CFArrayCreate(
                allocator.map_or(kCFAllocatorDefault, |x| &x.0),
                values.as_ptr() as *mut _,
                values.len() as _,
                callbacks,
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn len(&self) -> Index {
        unsafe { CFArrayGetCount(&self.0) }
    }
}

#[repr(transparent)]
pub struct MutableArray<T>(__CFArray, core::marker::PhantomData<*mut T>);
impl<T> Object for MutableArray<T> {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl<T> MutableObject for MutableArray<T> {}
impl<T> core::ops::Index<Index> for MutableArray<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, index: Index) -> &Self::Output {
        unsafe { &*CFArrayGetValueAtIndex(&self.0, index).cast::<T>() }
    }
}
impl<T> MutableArray<T> {
    #[inline(always)]
    pub fn new(allocator: Option<&Allocator>, capacity: Index) -> Option<Owned<Self>>
    where
        T: ArrayValue,
    {
        unsafe { Self::new_raw(allocator, capacity, array_value_callbacks::<T>()) }
    }

    #[inline(always)]
    pub unsafe fn new_raw(
        allocator: Option<&Allocator>,
        capacity: Index,
        callbacks: &CFArrayCallBacks,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CFArrayCreateMutable(
                allocator.map_or(kCFAllocatorDefault, |x| &x.0),
                capacity,
                callbacks,
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn len(&self) -> Index {
        unsafe { CFArrayGetCount(&self.0) }
    }

    #[inline(always)]
    pub fn append(&mut self, value: &T) {
        unsafe { CFArrayAppendValue(&mut self.0, value as *const _ as _) }
    }

    #[inline(always)]
    pub fn set_at(&mut self, index: Index, value: &T) {
        unsafe { CFArraySetValueAtIndex(&mut self.0, index, value as *const _ as _) }
    }
}

pub trait DictionaryKey {
    fn retain(&self, allocator: Option<&Allocator>) -> &Self;
    fn release(&self, allocator: Option<&Allocator>);
    fn copy_description(&self) -> CFStringRef;
    fn equal(&self, other: &Self) -> Boolean;
    fn hash(&self) -> HashCode;
}

const fn dictionary_key_callbacks<T: DictionaryKey>() -> &'static CFDictionaryKeyCallBacks {
    extern "C" fn retain<T: DictionaryKey>(
        allocator: CFAllocatorRef,
        value: *const core::ffi::c_void,
    ) -> *const core::ffi::c_void {
        unsafe {
            T::retain(
                &*value.cast::<T>(),
                if allocator.is_null() {
                    None
                } else {
                    Some(&*allocator.cast())
                },
            ) as *const _ as _
        }
    }

    extern "C" fn release<T: DictionaryKey>(
        allocator: CFAllocatorRef,
        value: *const core::ffi::c_void,
    ) {
        unsafe {
            T::release(
                &*value.cast::<T>(),
                if allocator.is_null() {
                    None
                } else {
                    Some(&*allocator.cast())
                },
            )
        }
    }

    extern "C" fn copy_description<T: DictionaryKey>(
        value: *const core::ffi::c_void,
    ) -> CFStringRef {
        unsafe { T::copy_description(&*value.cast::<T>()) }
    }

    extern "C" fn equal<T: DictionaryKey>(
        value1: *const core::ffi::c_void,
        value2: *const core::ffi::c_void,
    ) -> Boolean {
        unsafe { T::equal(&*value1.cast::<T>(), &*value2.cast::<T>()) }
    }

    extern "C" fn hash<T: DictionaryKey>(value: *const core::ffi::c_void) -> HashCode {
        unsafe { T::hash(&*value.cast::<T>()) }
    }

    &CFDictionaryKeyCallBacks {
        version: 0,
        retain: Some(retain::<T>),
        release: Some(release::<T>),
        copy_description: Some(copy_description::<T>),
        equal: Some(equal::<T>),
        hash: Some(hash::<T>),
    }
}

pub trait DictionaryValue {
    fn retain(&self, allocator: &Allocator) -> &Self;
    fn release(&self, allocator: &Allocator);
    fn copy_description(&self) -> CFStringRef;
    fn equal(&self, other: &Self) -> Boolean;
}

const fn dictionary_value_callbacks<T: DictionaryValue>() -> &'static CFDictionaryValueCallBacks {
    extern "C" fn retain<T: DictionaryValue>(
        allocator: CFAllocatorRef,
        value: *const core::ffi::c_void,
    ) -> *const core::ffi::c_void {
        unsafe { T::retain(&*value.cast::<T>(), &*allocator.cast()) as *const _ as _ }
    }

    extern "C" fn release<T: DictionaryValue>(
        allocator: CFAllocatorRef,
        value: *const core::ffi::c_void,
    ) {
        unsafe { T::release(&*value.cast::<T>(), &*allocator.cast()) }
    }

    extern "C" fn copy_description<T: DictionaryValue>(
        value: *const core::ffi::c_void,
    ) -> CFStringRef {
        unsafe { T::copy_description(&*value.cast::<T>()) }
    }

    extern "C" fn equal<T: DictionaryValue>(
        value1: *const core::ffi::c_void,
        value2: *const core::ffi::c_void,
    ) -> Boolean {
        unsafe { T::equal(&*value1.cast::<T>(), &*value2.cast::<T>()) }
    }

    &CFDictionaryValueCallBacks {
        version: 0,
        retain: Some(retain::<T>),
        release: Some(release::<T>),
        copy_description: Some(copy_description::<T>),
        equal: Some(equal::<T>),
    }
}

#[repr(C)]
pub struct Dictionary<K: ?Sized, V: ?Sized>(
    __CFDictionary,
    core::marker::PhantomData<(*const K, *const V)>,
);
impl<K: ?Sized, V: ?Sized> Object for Dictionary<K, V> {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl<K: ?Sized, V> core::ops::Index<&'_ K> for Dictionary<K, V> {
    type Output = V;

    #[inline(always)]
    fn index(&self, index: &K) -> &Self::Output {
        unsafe { &*CFDictionaryGetValue(&self.0, index as *const _ as _).cast() }
    }
}
impl<K: ?Sized, V: ?Sized> Dictionary<K, V> {
    #[inline(always)]
    pub fn new(allocator: Option<&Allocator>, keys: &[&K], values: &[&V]) -> Option<Owned<Self>>
    where
        K: DictionaryKey + Sized,
        V: DictionaryValue + Sized,
    {
        debug_assert_eq!(keys.len(), values.len());

        unsafe {
            Self::new_raw(
                allocator,
                keys.as_ptr() as *mut _,
                values.as_ptr() as *mut _,
                keys.len() as _,
                dictionary_key_callbacks::<K>(),
                dictionary_value_callbacks::<V>(),
            )
        }
    }

    #[inline(always)]
    pub unsafe fn new_raw(
        allocator: Option<&Allocator>,
        keys: *mut *const core::ffi::c_void,
        values: *mut *const core::ffi::c_void,
        num_values: Index,
        key_callbacks: &CFDictionaryKeyCallBacks,
        value_callbacks: &CFDictionaryValueCallBacks,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CFDictionaryCreate(
                allocator.map_or(kCFAllocatorDefault, |x| &x.0),
                keys,
                values,
                num_values,
                key_callbacks,
                value_callbacks,
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn len(&self) -> Index {
        unsafe { CFDictionaryGetCount(&self.0) }
    }
}

#[repr(C)]
pub struct MutableDictionary<K: ?Sized, V: ?Sized>(
    __CFDictionary,
    core::marker::PhantomData<(*mut K, *mut V)>,
);
impl<K: ?Sized, V: ?Sized> core::ops::Deref for MutableDictionary<K, V> {
    type Target = Dictionary<K, V>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute(self) }
    }
}
impl<K: ?Sized, V: ?Sized> Object for MutableDictionary<K, V> {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl<K: ?Sized, V: ?Sized> MutableObject for MutableDictionary<K, V> {}
impl<K: ?Sized, V> core::ops::Index<&'_ K> for MutableDictionary<K, V> {
    type Output = V;

    #[inline(always)]
    fn index(&self, index: &K) -> &Self::Output {
        unsafe { &*CFDictionaryGetValue(&self.0, index as *const _ as _).cast() }
    }
}
impl<K: ?Sized, V: ?Sized> MutableDictionary<K, V> {
    #[inline(always)]
    pub fn new(allocator: Option<&Allocator>, capacity: Index) -> Option<Owned<Self>>
    where
        K: DictionaryKey + Sized,
        V: DictionaryValue + Sized,
    {
        unsafe {
            Self::new_raw(
                allocator,
                capacity,
                dictionary_key_callbacks::<K>(),
                dictionary_value_callbacks::<V>(),
            )
        }
    }

    #[inline(always)]
    pub fn new_generic_key_value(
        allocator: Option<&Allocator>,
        capacity: Index,
    ) -> Option<Owned<Self>>
    where
        K: Object,
        V: Object,
    {
        unsafe {
            Self::new_raw(
                allocator,
                capacity,
                &kCFTypeDictionaryKeyCallBacks,
                &kCFTypeDictionaryValueCallBacks,
            )
        }
    }

    #[inline(always)]
    pub unsafe fn new_raw(
        allocator: Option<&Allocator>,
        capacity: Index,
        key_callbacks: &CFDictionaryKeyCallBacks,
        value_callbacks: &CFDictionaryValueCallBacks,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CFDictionaryCreateMutable(
                allocator.map_or(kCFAllocatorDefault, |x| &x.0),
                capacity,
                key_callbacks,
                value_callbacks,
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn set(&mut self, key: &K, value: &V) {
        unsafe { CFDictionarySetValue(&mut self.0, key as *const _ as _, value as *const _ as _) }
    }
}

#[repr(i64)]
pub enum NumberType {
    SInt8 = kCFNumberSInt8Type,
    SInt16 = kCFNumberSInt16Type,
    SInt32 = kCFNumberSInt32Type,
    SInt64 = kCFNumberSInt64Type,
    Float32 = kCFNumberFloat32Type,
    Float64 = kCFNumberFloat64Type,
    Char = kCFNumberCharType,
    Short = kCFNumberShortType,
    Int = kCFNumberIntType,
    Long = kCFNumberLongType,
    LongLong = kCFNumberLongLongType,
    Float = kCFNumberFloatType,
    Double = kCFNumberDoubleType,
    CFIndex = kCFNumberCFIndexType,
    NSInteger = kCFNumberNSIntegerType,
    CGFloat = kCFNumberCGFloatType,
}

#[repr(transparent)]
pub struct Number(__CFNumber);
impl Object for Number {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl Number {
    pub const POSITIVE_INFINITY: &Self = unsafe { &*kCFNumberPositiveInfinity.cast::<Self>() };
    pub const NEGATIVE_INFINITY: &Self = unsafe { &*kCFNumberNegativeInfinity.cast::<Self>() };
    pub const NAN: &Self = unsafe { &*kCFNumberNaN.cast::<Self>() };

    #[inline(always)]
    pub unsafe fn new_raw(
        allocator: Option<&Allocator>,
        r#type: NumberType,
        value_ptr: *const core::ffi::c_void,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CFNumberCreate(
                allocator.map_or(kCFAllocatorDefault, |x| &x.0),
                r#type as _,
                value_ptr,
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn new_u32(allocator: Option<&Allocator>, value: u32) -> Option<Owned<Self>> {
        // Note: swift-corelibs-foundationの実装を覗いた限りでは、どうやら符号なし指定ビット長の整数はより長いビット長の符号付き整数で表せばいいらしい
        Self::new_i64(allocator, value as _)
    }

    #[inline(always)]
    pub fn new_i64(allocator: Option<&Allocator>, value: i64) -> Option<Owned<Self>> {
        unsafe {
            Self::new_raw(
                allocator,
                NumberType::SInt64,
                value.to_ne_bytes().as_ptr().cast(),
            )
        }
    }

    #[inline(always)]
    pub fn new_f32(allocator: Option<&Allocator>, value: f32) -> Option<Owned<Self>> {
        unsafe {
            Self::new_raw(
                allocator,
                NumberType::Float32,
                value.to_ne_bytes().as_ptr().cast(),
            )
        }
    }
}

#[repr(u32)]
#[derive(Clone, Copy)]
pub enum StringEncoding {
    InvalidId = kCFStringEncodingInvalidId,
    MacRoman = kCFStringEncodingMacRoman,
    WindowsLatin1 = kCFStringEncodingWindowsLatin1,
    ISOLatin1 = kCFStringEncodingISOLatin1,
    NextStepLatin = kCFStringEncodingNextStepLatin,
    ASCII = kCFStringEncodingASCII,
    UTF8 = kCFStringEncodingUTF8,
    NonLossyASCII = kCFStringEncodingNonLossyASCII,
    UTF16 = kCFStringEncodingUTF16,
    UTF16BE = kCFStringEncodingUTF16BE,
    UTF16LE = kCFStringEncodingUTF16LE,
    UTF32 = kCFStringEncodingUTF32,
    UTF32BE = kCFStringEncodingUTF32BE,
    UTF32LE = kCFStringEncodingUTF32LE,
}

#[repr(transparent)]
pub struct String(__CFString);
impl Object for String {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl String {
    #[inline(always)]
    pub fn from_cstring(
        allocator: Option<&Allocator>,
        cstr: &core::ffi::CStr,
    ) -> Option<Owned<Self>> {
        Self::from_cstring_with_encoding(allocator, cstr, StringEncoding::UTF8)
    }

    #[inline(always)]
    pub fn from_str(allocator: Option<&Allocator>, r#str: &str) -> Option<Owned<Self>> {
        Self::from_bytes(allocator, r#str.as_bytes(), StringEncoding::UTF8, false)
    }

    #[inline(always)]
    pub unsafe fn from_str_no_copy<'a>(
        allocator: Option<&Allocator>,
        r#str: &'a str,
    ) -> Option<Owned<Self>> {
        unsafe {
            Self::from_bytes_no_copy(allocator, r#str.as_bytes(), StringEncoding::UTF8, false)
        }
    }

    #[inline(always)]
    pub fn from_cstring_with_encoding(
        allocator: Option<&Allocator>,
        cstr: &core::ffi::CStr,
        encoding: StringEncoding,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CFStringCreateWithCString(
                allocator.map_or(kCFAllocatorDefault, |x| &x.0),
                cstr.as_ptr(),
                encoding as _,
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn from_bytes(
        allocator: Option<&Allocator>,
        bytes: &[u8],
        encoding: StringEncoding,
        is_external_representation: bool,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CFStringCreateWithBytes(
                allocator.map_or(kCFAllocatorDefault, |x| &x.0),
                bytes.as_ptr(),
                bytes.len() as _,
                encoding as _,
                if is_external_representation { 1 } else { 0 },
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub unsafe fn from_bytes_no_copy(
        allocator: Option<&Allocator>,
        bytes: &[u8],
        encoding: StringEncoding,
        is_external_representation: bool,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CFStringCreateWithBytesNoCopy(
                allocator.map_or(kCFAllocatorDefault, |x| &x.0),
                bytes.as_ptr(),
                bytes.len() as _,
                encoding as _,
                if is_external_representation { 1 } else { 0 },
                kCFAllocatorNull,
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn len(&self) -> Index {
        unsafe { CFStringGetLength(&self.0) }
    }

    #[inline(always)]
    pub fn cstring(&self) -> &core::ffi::CStr {
        self.cstring_with_encoding(StringEncoding::UTF8)
    }

    #[inline(always)]
    pub fn cstring_with_encoding(&self, encoding: StringEncoding) -> &core::ffi::CStr {
        unsafe { core::ffi::CStr::from_ptr(CFStringGetCStringPtr(&self.0, encoding as _)) }
    }

    #[inline(always)]
    pub fn get_bytes(
        &self,
        range: Range,
        encoding: StringEncoding,
        loss_byte: u8,
        is_external_representation: bool,
        buffer: &mut [core::mem::MaybeUninit<u8>],
        used_buf_len: &mut core::mem::MaybeUninit<Index>,
    ) -> Index {
        unsafe {
            CFStringGetBytes(
                &self.0,
                range,
                encoding as _,
                loss_byte,
                if is_external_representation { 1 } else { 0 },
                buffer.as_mut_ptr() as *mut _,
                buffer.len() as _,
                used_buf_len.as_mut_ptr(),
            )
        }
    }
}
