use std::io::{IoSlice, Write};

use crate::raw::spa_rectangle;

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
}
impl From<bool> for ArrayValueBool {
    #[inline(always)]
    fn from(value: bool) -> Self {
        Self::new(value)
    }
}
unsafe impl ArrayValue for ArrayValueBool {
    const TYPE: Type = Type::Bool;
}
