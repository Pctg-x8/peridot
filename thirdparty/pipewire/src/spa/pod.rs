use std::{ffi::CStr, ops::Deref};

use crate::raw::{
    SPA_CHOICE_Enum, SPA_CHOICE_Flags, SPA_CHOICE_None, SPA_CHOICE_Range, SPA_CHOICE_Step,
    spa_fraction, spa_pod, spa_pod_array, spa_pod_bitmap, spa_pod_bool, spa_pod_bytes,
    spa_pod_choice, spa_pod_double, spa_pod_float, spa_pod_fraction, spa_pod_id, spa_pod_int,
    spa_pod_long, spa_pod_object, spa_pod_prop, spa_pod_rectangle, spa_pod_string, spa_pod_struct,
    spa_rectangle,
};

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    None = 1,
    Bool,
    Id,
    Int,
    Long,
    Float,
    Double,
    String,
    Bytes,
    Rectangle,
    Fraction,
    Bitmap,
    Array,
    Struct,
    Object,
    Sequence,
    Pointer,
    Fd,
    Choice,
    Pod,
}
impl TryFrom<u32> for Type {
    type Error = u32;

    #[inline]
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if Self::None as u32 <= value && value <= Self::Pod as u32 {
            Ok(unsafe { core::mem::transmute(value) })
        } else {
            Err(value)
        }
    }
}

pub struct Builder {
    bytes: Vec<u8>,
    container_size_pos: Vec<usize>,
}
impl Builder {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            container_size_pos: Vec::new(),
        }
    }

    #[inline(always)]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            bytes: Vec::with_capacity(cap),
            container_size_pos: Vec::new(),
        }
    }

    #[inline(always)]
    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }
}
impl Builder {
    #[inline]
    pub fn header(&mut self, size: u32, r#type: Type) -> &mut Self {
        self.header_noinc(size, r#type);
        self.inc_container_size(8);

        self
    }

    #[inline]
    fn header_noinc(&mut self, size: u32, r#type: Type) -> &mut Self {
        self.bytes.extend(size.to_ne_bytes());
        self.bytes.extend((r#type as u32).to_ne_bytes());

        self
    }

    #[inline]
    pub fn none(&mut self) -> &mut Self {
        self.header(0, Type::None)
    }

    #[inline]
    pub fn r#bool(&mut self, v: bool) -> &mut Self {
        self.header(4, Type::Bool);
        self.bytes
            .extend((if v { 1u32 } else { 0u32 }).to_ne_bytes());
        self.bytes.extend([0u8; 4]);
        self.inc_container_size(8);

        self
    }

    #[inline]
    pub fn id(&mut self, v: u32) -> &mut Self {
        self.header(4, Type::Id);
        self.bytes.extend(v.to_ne_bytes());
        self.bytes.extend([0u8; 4]);
        self.inc_container_size(8);

        self
    }

    #[inline]
    pub fn int(&mut self, v: i32) -> &mut Self {
        self.header(4, Type::Int);
        self.bytes.extend(v.to_ne_bytes());
        self.bytes.extend([0u8; 4]);
        self.inc_container_size(8);

        self
    }

    #[inline]
    pub fn long(&mut self, v: i64) -> &mut Self {
        self.header(8, Type::Long);
        self.bytes.extend(v.to_ne_bytes());
        self.inc_container_size(8);

        self
    }

    #[inline]
    pub fn float(&mut self, v: f32) -> &mut Self {
        self.header(4, Type::Float);
        self.bytes.extend(v.to_ne_bytes());
        self.bytes.extend([0u8; 4]);
        self.inc_container_size(8);

        self
    }

    #[inline]
    pub fn double(&mut self, v: f64) -> &mut Self {
        self.header(8, Type::Double);
        self.bytes.extend(v.to_ne_bytes());
        self.inc_container_size(8);

        self
    }

    #[inline]
    pub fn string(&mut self, s: &str) -> &mut Self {
        self.header((s.len() + 1) as u32, Type::String);
        self.bytes.extend(s.as_bytes());
        self.bytes.push(0);
        self.inc_container_size(((s.len() + 1 + 7) & !7) as _);
        self.adjust_pad()
    }

    #[inline]
    pub fn bytes(&mut self, v: &[u8]) -> &mut Self {
        self.header(v.len() as _, Type::Bytes);
        self.bytes.extend(v);
        self.inc_container_size(((v.len() + 7) & !7) as _);
        self.adjust_pad()
    }

    #[inline]
    pub fn rectangle(&mut self, width: u32, height: u32) -> &mut Self {
        self.header(8, Type::Rectangle);
        self.bytes.extend(width.to_ne_bytes());
        self.bytes.extend(height.to_ne_bytes());
        self.inc_container_size(8);

        self
    }

    #[inline]
    pub fn fraction(&mut self, num: u32, denom: u32) -> &mut Self {
        self.header(8, Type::Fraction);
        self.bytes.extend(num.to_ne_bytes());
        self.bytes.extend(denom.to_ne_bytes());
        self.inc_container_size(8);

        self
    }

    #[inline]
    pub fn bitmap(&mut self, bitmap: &[u8]) -> &mut Self {
        self.header(bitmap.len() as _, Type::Bitmap);
        self.bytes.extend(bitmap);
        self.inc_container_size(((bitmap.len() + 7) & !7) as _);
        self.adjust_pad()
    }

    #[inline]
    pub fn array<'a, T: ArrayValue>(&'a mut self, vs: &[T]) -> &'a mut Self {
        self.header(8 + core::mem::size_of_val(vs) as u32, Type::Array);
        self.header(core::mem::size_of::<T>() as _, T::TYPE);
        self.bytes.extend(unsafe {
            core::slice::from_raw_parts(vs.as_ptr().cast(), core::mem::size_of_val(vs))
        });
        self.inc_container_size(((core::mem::size_of_val(vs) + 7) & !7) as u32);
        self.adjust_pad()
    }

    #[inline]
    pub fn begin_struct(&mut self) -> &mut Self {
        self.container_size_pos.push(self.bytes.len());
        self.header_noinc(0, Type::Struct)
    }

    #[inline]
    pub fn end_struct(&mut self) -> &mut Self {
        self.container_size_pos.pop();
        self
    }

    #[inline]
    pub fn begin_object(&mut self, object_type: u32, object_id: u32) -> &mut Self {
        self.container_size_pos.push(self.bytes.len());
        self.header_noinc(0, Type::Object);
        self.bytes.extend(object_type.to_ne_bytes());
        self.bytes.extend(object_id.to_ne_bytes());
        self.inc_container_size(8);

        self
    }

    #[inline]
    pub fn prop_heading(&mut self, key: u32, flags: u32) -> &mut Self {
        self.bytes.extend(key.to_ne_bytes());
        self.bytes.extend(flags.to_ne_bytes());
        self.inc_container_size(8);

        self
    }

    #[inline]
    pub fn end_object(&mut self) -> &mut Self {
        self.container_size_pos.pop();
        self
    }

    #[inline]
    fn adjust_pad(&mut self) -> &mut Self {
        self.bytes.resize((self.bytes.len() + 7) & !7, 0);
        self
    }

    fn inc_container_size(&mut self, size: u32) {
        for &p in self.container_size_pos.iter() {
            let r = unsafe { &mut *self.bytes.as_mut_ptr().add(p).cast::<u32>() };
            *r += size;
        }
    }
}

#[repr(transparent)]
pub struct Parser(spa_pod);
impl Parser {
    pub fn new<'a>(head: &'a spa_pod) -> &'a Self {
        unsafe { core::mem::transmute(head) }
    }

    #[inline(always)]
    pub fn size(&self) -> u32 {
        self.0.size
    }

    #[inline(always)]
    pub fn r#type(&self) -> Result<Type, u32> {
        self.0.r#type.try_into()
    }

    #[inline(always)]
    pub fn try_as_none(&self) -> Option<&ParserNone> {
        if self.r#type() == Ok(Type::None) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_bool(&self) -> Option<&ParserBool> {
        if self.r#type() == Ok(Type::Bool) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_id(&self) -> Option<&ParserId> {
        if self.r#type() == Ok(Type::Id) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_int(&self) -> Option<&ParserInt> {
        if self.r#type() == Ok(Type::Int) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_long(&self) -> Option<&ParserLong> {
        if self.r#type() == Ok(Type::Long) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_float(&self) -> Option<&ParserFloat> {
        if self.r#type() == Ok(Type::Float) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_double(&self) -> Option<&ParserDouble> {
        if self.r#type() == Ok(Type::Double) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_string(&self) -> Option<&ParserString> {
        if self.r#type() == Ok(Type::String) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_bytes(&self) -> Option<&ParserBytes> {
        if self.r#type() == Ok(Type::Bytes) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_rectangle(&self) -> Option<&ParserRectangle> {
        if self.r#type() == Ok(Type::Rectangle) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_fraction(&self) -> Option<&ParserFraction> {
        if self.r#type() == Ok(Type::Fraction) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_bitmap(&self) -> Option<&ParserBitmap> {
        if self.r#type() == Ok(Type::Bitmap) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_array(&self) -> Option<&ParserArray> {
        if self.r#type() == Ok(Type::Array) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_choice(&self) -> Option<&ParserChoice> {
        if self.r#type() == Ok(Type::Choice) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_struct(&self) -> Option<&ParserStruct> {
        if self.r#type() == Ok(Type::Struct) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_object(&self) -> Option<&ParserObject> {
        if self.r#type() == Ok(Type::Object) {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }
}

#[repr(transparent)]
pub struct ParserNone(spa_pod);

#[repr(transparent)]
pub struct ParserBool(spa_pod_bool);
impl ParserBool {
    #[inline(always)]
    pub fn value(&self) -> bool {
        self.0.value != 0
    }
}

#[repr(transparent)]
pub struct ParserId(spa_pod_id);
impl ParserId {
    #[inline(always)]
    pub fn value(&self) -> u32 {
        self.0.value
    }
}

#[repr(transparent)]
pub struct ParserInt(spa_pod_int);
impl ParserInt {
    #[inline(always)]
    pub fn value(&self) -> i32 {
        self.0.value
    }
}

#[repr(transparent)]
pub struct ParserLong(spa_pod_long);
impl ParserLong {
    #[inline(always)]
    pub fn value(&self) -> i64 {
        self.0.value
    }
}

#[repr(transparent)]
pub struct ParserFloat(spa_pod_float);
impl ParserFloat {
    #[inline(always)]
    pub fn value(&self) -> f32 {
        self.0.value
    }
}

#[repr(transparent)]
pub struct ParserDouble(spa_pod_double);
impl ParserDouble {
    #[inline(always)]
    pub fn value(&self) -> f64 {
        self.0.value
    }
}

#[repr(transparent)]
pub struct ParserString(spa_pod_string);
impl ParserString {
    #[inline(always)]
    pub fn value(&self) -> &CStr {
        debug_assert!(
            unsafe {
                core::slice::from_raw_parts(
                    (&self.0 as *const spa_pod_string)
                        .cast::<core::ffi::c_char>()
                        .add(core::mem::size_of::<spa_pod>()),
                    self.0.pod.size as usize,
                )
                .contains(&0)
            },
            "illformed spa_pod_string (no nul byte in the range)"
        );

        unsafe {
            core::ffi::CStr::from_ptr(
                (&self.0 as *const spa_pod_string)
                    .cast::<core::ffi::c_char>()
                    .add(core::mem::size_of::<spa_pod>()),
            )
        }
    }
}

#[repr(transparent)]
pub struct ParserBytes(spa_pod_bytes);
impl ParserBytes {
    #[inline(always)]
    pub fn value(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                (&self.0 as *const spa_pod_bytes)
                    .cast::<u8>()
                    .add(core::mem::size_of::<spa_pod>()),
                self.0.pod.size as _,
            )
        }
    }
}

#[repr(transparent)]
pub struct ParserRectangle(spa_pod_rectangle);
impl ParserRectangle {
    #[inline(always)]
    pub fn value(&self) -> &spa_rectangle {
        &self.0.value
    }
}

#[repr(transparent)]
pub struct ParserFraction(spa_pod_fraction);
impl ParserFraction {
    #[inline(always)]
    pub fn value(&self) -> &spa_fraction {
        &self.0.value
    }
}

#[repr(transparent)]
pub struct ParserBitmap(spa_pod_bitmap);
impl ParserBitmap {
    #[inline(always)]
    pub fn value(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(
                (&self.0 as *const spa_pod_bitmap)
                    .cast::<u8>()
                    .add(core::mem::size_of::<spa_pod>()),
                self.0.pod.size as _,
            )
        }
    }
}

#[repr(transparent)]
pub struct ParserArray(spa_pod_array);
impl ParserArray {
    #[inline(always)]
    pub fn child_type(&self) -> Result<Type, u32> {
        self.0.body.child.r#type.try_into()
    }

    #[inline(always)]
    pub fn child_size(&self) -> u32 {
        self.0.body.child.size
    }

    #[inline(always)]
    pub fn values_head_ptr<T>(&self) -> *const T {
        self.0.body.values.as_ptr().cast()
    }

    #[inline(always)]
    pub unsafe fn values_unchecked<T: ArrayValue>(&self) -> &[T] {
        debug_assert!(self.0.pod.size as usize >= core::mem::size_of::<spa_pod>());
        debug_assert_eq!(self.0.body.child.r#type, T::TYPE as u32);

        unsafe {
            core::slice::from_raw_parts(
                self.values_head_ptr::<T>(),
                (self.0.pod.size as usize - core::mem::size_of::<spa_pod>())
                    / core::mem::size_of::<T>(),
            )
        }
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChoiceType {
    None,
    Range,
    Step,
    Enum,
    Flags,
}
impl TryFrom<u32> for ChoiceType {
    type Error = u32;

    #[inline(always)]
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if Self::None as u32 <= value && value <= Self::Flags as u32 {
            Ok(unsafe { core::mem::transmute(value) })
        } else {
            Err(value)
        }
    }
}

#[repr(transparent)]
pub struct ParserChoice(spa_pod_choice);
impl ParserChoice {
    #[inline(always)]
    pub fn try_as_none(&self) -> Option<&ParserChoiceNone> {
        if self.0.body.r#type == SPA_CHOICE_None {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_range(&self) -> Option<&ParserChoiceRange> {
        if self.0.body.r#type == SPA_CHOICE_Range {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_step(&self) -> Option<&ParserChoiceStep> {
        if self.0.body.r#type == SPA_CHOICE_Step {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_enum(&self) -> Option<&ParserChoiceEnum> {
        if self.0.body.r#type == SPA_CHOICE_Enum {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn try_as_flags(&self) -> Option<&ParserChoiceFlags> {
        if self.0.body.r#type == SPA_CHOICE_Flags {
            Some(unsafe { core::mem::transmute(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn choice_type(&self) -> Result<ChoiceType, u32> {
        self.0.body.r#type.try_into()
    }

    #[inline(always)]
    pub fn child_type(&self) -> Result<Type, u32> {
        self.0.body.child.r#type.try_into()
    }

    #[inline(always)]
    pub fn child_size(&self) -> u32 {
        self.0.body.child.size
    }

    #[inline(always)]
    pub fn values_head_ptr<T>(&self) -> *const T {
        self.0.body.values.as_ptr().cast()
    }
}
impl Deref for ParserChoice {
    type Target = Parser;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute(self) }
    }
}

#[repr(transparent)]
pub struct ParserChoiceNone(spa_pod_choice);
impl ParserChoiceNone {
    #[inline(always)]
    fn check_invariant_size<T>(&self) -> bool {
        self.0.pod.size as usize
            >= core::mem::size_of::<u32>()
                + core::mem::size_of::<u32>()
                + core::mem::size_of::<spa_pod>()
                + core::mem::size_of::<T>()
    }

    #[inline(always)]
    pub unsafe fn current_unchecked<T>(&self) -> &T {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>() }
    }
}
impl Deref for ParserChoiceNone {
    type Target = ParserChoice;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute(self) }
    }
}

#[repr(transparent)]
pub struct ParserChoiceRange(spa_pod_choice);
impl ParserChoiceRange {
    #[inline(always)]
    fn check_invariant_size<T>(&self) -> bool {
        self.0.pod.size as usize
            >= core::mem::size_of::<u32>()
                + core::mem::size_of::<u32>()
                + core::mem::size_of::<spa_pod>()
                + core::mem::size_of::<[T; 3]>()
    }

    #[inline(always)]
    pub unsafe fn default_unchecked<T>(&self) -> &T {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>() }
    }

    #[inline(always)]
    pub unsafe fn min_unchecked<T>(&self) -> &T {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>().add(1) }
    }

    #[inline(always)]
    pub unsafe fn max_unchecked<T>(&self) -> &T {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>().add(2) }
    }

    #[inline(always)]
    pub unsafe fn values_unchecked<T>(&self) -> &ChoiceRange<T> {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>().cast() }
    }
}
impl Deref for ParserChoiceRange {
    type Target = ParserChoice;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute(self) }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ChoiceRange<T> {
    pub default: T,
    pub min: T,
    pub max: T,
}

#[repr(transparent)]
pub struct ParserChoiceStep(spa_pod_choice);
impl ParserChoiceStep {
    #[inline(always)]
    fn check_invariant_size<T>(&self) -> bool {
        self.0.pod.size as usize
            >= core::mem::size_of::<u32>()
                + core::mem::size_of::<u32>()
                + core::mem::size_of::<spa_pod>()
                + core::mem::size_of::<[T; 4]>()
    }

    #[inline(always)]
    pub unsafe fn default_unchecked<T>(&self) -> &T {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>() }
    }

    #[inline(always)]
    pub unsafe fn min_unchecked<T>(&self) -> &T {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>().add(1) }
    }

    #[inline(always)]
    pub unsafe fn max_unchecked<T>(&self) -> &T {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>().add(2) }
    }

    #[inline(always)]
    pub unsafe fn step_unchecked<T>(&self) -> &T {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>().add(3) }
    }
}
impl Deref for ParserChoiceStep {
    type Target = ParserChoice;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute(self) }
    }
}

#[repr(transparent)]
pub struct ParserChoiceEnum(spa_pod_choice);
impl ParserChoiceEnum {
    #[inline(always)]
    fn check_invariant_size<T>(&self) -> bool {
        self.0.pod.size as usize
            >= core::mem::size_of::<u32>()
                + core::mem::size_of::<u32>()
                + core::mem::size_of::<spa_pod>()
                + core::mem::size_of::<T>()
    }

    #[inline(always)]
    pub unsafe fn default_unchecked<T>(&self) -> &T {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>() }
    }

    #[inline(always)]
    pub unsafe fn alternatives_unchecked<T>(&self) -> &[T] {
        debug_assert!(self.check_invariant_size::<T>());

        unsafe {
            core::slice::from_raw_parts(
                self.values_head_ptr::<T>().add(1),
                self.0.pod.size as usize
                    - core::mem::size_of::<u32>()
                    - core::mem::size_of::<u32>()
                    - core::mem::size_of::<spa_pod>()
                    - core::mem::size_of::<T>(),
            )
        }
    }
}
impl Deref for ParserChoiceEnum {
    type Target = ParserChoice;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute(self) }
    }
}

#[repr(transparent)]
pub struct ParserChoiceFlags(spa_pod_choice);
impl ParserChoiceFlags {
    #[inline(always)]
    fn check_invariant_size<T>(&self) -> bool {
        self.0.pod.size as usize
            >= core::mem::size_of::<u32>()
                + core::mem::size_of::<u32>()
                + core::mem::size_of::<spa_pod>()
                + core::mem::size_of::<T>()
    }

    #[inline(always)]
    pub unsafe fn default_unchecked<T>(&self) -> &T {
        debug_assert!(self.check_invariant_size::<T>());
        unsafe { &*self.values_head_ptr::<T>() }
    }

    #[inline(always)]
    pub unsafe fn possible_flags_unchecked<T>(&self) -> &[T] {
        debug_assert!(self.check_invariant_size::<T>());

        unsafe {
            core::slice::from_raw_parts(
                self.values_head_ptr::<T>().add(1),
                self.0.pod.size as usize
                    - core::mem::size_of::<u32>()
                    - core::mem::size_of::<u32>()
                    - core::mem::size_of::<spa_pod>()
                    - core::mem::size_of::<T>(),
            )
        }
    }
}
impl Deref for ParserChoiceFlags {
    type Target = ParserChoice;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute(self) }
    }
}

#[repr(transparent)]
pub struct ParserStruct(spa_pod_struct);
impl ParserStruct {
    #[inline(always)]
    pub fn iter_members<'a>(&'a self) -> StructMemberIterator<'a> {
        StructMemberIterator {
            pod: &self.0,
            offset: 0,
        }
    }
}
pub struct StructMemberIterator<'a> {
    pod: &'a spa_pod_struct,
    offset: usize,
}
impl<'a> Iterator for StructMemberIterator<'a> {
    type Item = &'a Parser;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.pod.pod.size as usize {
            return None;
        }

        let v = unsafe { &*self.pod.values.as_ptr().add(self.offset).cast::<Parser>() };
        // round up to 8 bytes
        self.offset = (self.offset + v.0.total_size() + 7) & !7;
        Some(v)
    }
}

#[repr(transparent)]
pub struct ParserObject(spa_pod_object);
impl ParserObject {
    #[inline(always)]
    pub fn object_type(&self) -> u32 {
        self.0.body.r#type
    }

    #[inline(always)]
    pub fn object_id(&self) -> u32 {
        self.0.body.id
    }

    #[inline(always)]
    pub fn iter_props<'a>(&'a self) -> ObjectPropsIterator<'a> {
        ObjectPropsIterator {
            pod: &self.0,
            offset: 0,
        }
    }
}
pub struct ObjectPropsIterator<'a> {
    pod: &'a spa_pod_object,
    offset: usize,
}
impl<'a> Iterator for ObjectPropsIterator<'a> {
    type Item = &'a ParserProp;

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
                .cast::<ParserProp>()
        };
        // round up to 8 bytes
        self.offset = (self.offset + v.0.total_size() + 7) & !7;
        Some(v)
    }
}

#[repr(transparent)]
pub struct ParserProp(spa_pod_prop);
impl ParserProp {
    #[inline(always)]
    pub fn key(&self) -> u32 {
        self.0.key
    }

    #[inline(always)]
    pub fn value(&self) -> &Parser {
        Parser::new(&self.0.value)
    }
}

pub unsafe trait ArrayValue {
    const TYPE: Type;
}
unsafe impl ArrayValue for u32 {
    const TYPE: Type = Type::Id;
}
unsafe impl ArrayValue for i32 {
    const TYPE: Type = Type::Int;
}
unsafe impl ArrayValue for i64 {
    const TYPE: Type = Type::Long;
}
unsafe impl ArrayValue for f32 {
    const TYPE: Type = Type::Float;
}
unsafe impl ArrayValue for f64 {
    const TYPE: Type = Type::Double;
}

#[repr(transparent)]
pub struct ArrayValueBool(u32);
impl ArrayValueBool {
    #[inline(always)]
    pub const fn new(v: bool) -> Self {
        Self(if v { 1 } else { 0 })
    }

    #[inline(always)]
    pub const fn value(&self) -> bool {
        self.0 != 0
    }
}
impl From<bool> for ArrayValueBool {
    #[inline(always)]
    fn from(value: bool) -> Self {
        Self::new(value)
    }
}
impl core::fmt::Debug for ArrayValueBool {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self.0 != 0).fmt(f)
    }
}
unsafe impl ArrayValue for ArrayValueBool {
    const TYPE: Type = Type::Bool;
}
