use std::io::{BufRead, IoSlice, IoSliceMut, Read, SeekFrom, Write};

use core::pin::Pin;
use futures_io::{AsyncBufRead, AsyncRead};
use peridot_semantic_shader::VertexInputSemantic;
use peridot_serialization_utils::{PascalStr, PascalString, VariableUInt};
use pinned_futures_helper::{read_byte_async, read_exact_async_pinned};

use crate::{
    DescriptorTypeVk, FaceCulling, FrontFace, PolygonRasterizationMode, PropertyDestinationVk,
    PropertyMappingVk, PropertyType, RenderingOptionOverrides,
};

#[inline(always)]
const fn b32<'x>(v: &'x u32) -> &'x [u8] {
    unsafe { core::mem::transmute::<&_, &[u8; 4]>(v) }
}
#[inline(always)]
const fn b32m<'x>(v: &'x mut u32) -> &'x mut [u8] {
    unsafe { core::mem::transmute::<&mut _, &mut [u8; 4]>(v) }
}
#[inline(always)]
const fn b32m_uninit<'x>(
    v: &'x mut core::mem::MaybeUninit<u32>,
) -> &'x mut [core::mem::MaybeUninit<u8>] {
    unsafe { core::mem::transmute::<&mut _, &mut [core::mem::MaybeUninit<u8>; 4]>(v) }
}
#[inline(always)]
const fn b64<'x>(v: &'x u64) -> &'x [u8] {
    unsafe { core::mem::transmute::<&_, &[u8; 8]>(v) }
}
#[inline(always)]
const fn b64m<'x>(v: &'x mut u64) -> &'x mut [u8] {
    unsafe { core::mem::transmute::<&mut _, &mut [u8; 8]>(v) }
}

pub struct Header {
    pub shading_pass_directory_offset: u64,
}
impl Header {
    const MAGIC: u32 = u32::from_be_bytes(*b"prc\x01");
    pub const BYTE_LENGTH: usize = 4 + 8;
    pub const READ_SEEK_POS: SeekFrom = SeekFrom::End(-Self::BYTE_LENGTH.cast_signed() as _);

    pub fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        let mut iovs = &mut [
            IoSlice::new(b32(&Self::MAGIC)),
            IoSlice::new(b64(&self.shading_pass_directory_offset)),
        ][..];

        let mut writes = 0;
        while !iovs.is_empty() {
            let w = sink.write_vectored(iovs)?;
            IoSlice::advance_slices(&mut iovs, w);
            writes += w;
        }

        Ok(writes)
    }

    pub fn read(source: &mut impl Read) -> std::io::Result<(Self, bool)> {
        let mut magic_buf = 0u32;
        source.read_exact(b32m(&mut magic_buf))?;
        let needs_swap = if magic_buf == Self::MAGIC {
            cfg!(target_endian = "big")
        } else if magic_buf.swap_bytes() == Self::MAGIC {
            cfg!(target_endian = "little")
        } else {
            panic!("magic mismatching");
        };

        let mut shading_pass_directory_offset = 0u64;
        let mut iovs = &mut [IoSliceMut::new(b64m(&mut shading_pass_directory_offset))][..];
        while !iovs.is_empty() {
            let r = source.read_vectored(iovs)?;
            IoSliceMut::advance_slices(&mut iovs, r);
        }

        if needs_swap {
            shading_pass_directory_offset = shading_pass_directory_offset.swap_bytes();
        }

        Ok((
            Self {
                shading_pass_directory_offset,
            },
            needs_swap,
        ))
    }

    pub async fn read_async(
        source: &mut peridot_native_io::BufferedRandomBlobReader<
            impl peridot_native_io::RandomReadBlobAsync,
        >,
        pos: u64,
    ) -> std::io::Result<(Self, bool)> {
        let mut magic_buf = core::mem::MaybeUninit::uninit();
        source
            .read_exact_at_async(pos, b32m_uninit(&mut magic_buf))
            .await?;
        let magic_buf = unsafe { magic_buf.assume_init() };
        let needs_swap = if magic_buf == Self::MAGIC {
            cfg!(target_endian = "big")
        } else if magic_buf.swap_bytes() == Self::MAGIC {
            cfg!(target_endian = "little")
        } else {
            panic!("magic mismatching");
        };

        let mut shading_pass_directory_offset = 0u64;
        source
            .readv_all_at_async(
                pos + 4,
                &mut [IoSliceMut::new(b64m(&mut shading_pass_directory_offset))],
            )
            .await?;
        if needs_swap {
            shading_pass_directory_offset = shading_pass_directory_offset.swap_bytes();
        }

        Ok((
            Self {
                shading_pass_directory_offset,
            },
            needs_swap,
        ))
    }
}

pub struct PropertyDirectory {
    pub entries: Vec<(String, PropertyType, PropertyMappingVk)>,
    pub descriptor_set_bindings: Vec<DescriptorTypeVk>,
    pub push_constant_buffer_size_bytes: usize,
}
impl PropertyDirectory {
    pub fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        let mut writes = VariableUInt(self.entries.len() as _).write(sink)?;
        for (name, r#type, mapping_vk) in self.entries.iter() {
            writes += PascalStr(name).write(sink)?;
            writes += r#type.write(sink)?;
            writes += mapping_vk.write(sink)?;
        }

        writes += VariableUInt(self.descriptor_set_bindings.len() as _).write(sink)?;
        for t in self.descriptor_set_bindings.iter() {
            writes += t.write(sink)?;
        }

        writes += VariableUInt(self.push_constant_buffer_size_bytes as _).write(sink)?;

        Ok(writes)
    }

    pub fn read(source: &mut impl BufRead) -> std::io::Result<Self> {
        let entry_count = VariableUInt::read(source)?.0 as usize;
        let mut entries = Vec::with_capacity(entry_count);
        for _ in 0..entry_count {
            let name = PascalString::read(source)?.0;
            let r#type = PropertyType::read(source)?;
            let mapping_vk = PropertyMappingVk::read(source)?;

            entries.push((name, r#type, mapping_vk));
        }

        let descriptor_set_binding_count = VariableUInt::read(source)?.0 as usize;
        let mut descriptor_set_bindings = Vec::with_capacity(descriptor_set_binding_count);
        for _ in 0..descriptor_set_binding_count {
            let t = DescriptorTypeVk::read(source)?;

            descriptor_set_bindings.push(t);
        }

        let push_constant_buffer_size_bytes = VariableUInt::read(source)?.0 as usize;

        Ok(Self {
            entries,
            descriptor_set_bindings,
            push_constant_buffer_size_bytes,
        })
    }

    pub async fn read_async(
        mut source: Pin<&mut (impl AsyncBufRead + ?Sized)>,
    ) -> std::io::Result<Self> {
        let VariableUInt(entry_count) = VariableUInt::read_async(source.as_mut()).await?;
        let mut entries = Vec::with_capacity(entry_count as _);
        for _ in 0..entry_count {
            let PascalString(name) = PascalString::read_async(source.as_mut()).await?;
            let r#type = PropertyType::read_async(source.as_mut()).await?;
            let mapping_vk = PropertyMappingVk::read_async(source.as_mut()).await?;

            entries.push((name, r#type, mapping_vk));
        }

        let VariableUInt(descriptor_set_binding_count) =
            VariableUInt::read_async(source.as_mut()).await?;
        let mut descriptor_set_bindings = Vec::with_capacity(descriptor_set_binding_count as _);
        for _ in 0..descriptor_set_binding_count {
            let t = DescriptorTypeVk::read_async(source.as_mut()).await?;

            descriptor_set_bindings.push(t);
        }

        let VariableUInt(push_constant_buffer_size_bytes) =
            VariableUInt::read_async(source.as_mut()).await?;

        Ok(Self {
            entries,
            descriptor_set_bindings,
            push_constant_buffer_size_bytes: push_constant_buffer_size_bytes as _,
        })
    }
}

pub struct ShadingPassDirectory {
    pub entries: Vec<(String, ShadingPassDirectoryEntry)>,
}
impl ShadingPassDirectory {
    pub fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        let mut writes = VariableUInt(self.entries.len() as _).write(sink)?;
        for (n, e) in self.entries.iter() {
            writes += PascalStr(n).write(sink)?;
            writes += e.write(sink)?;
        }

        Ok(writes)
    }

    pub fn read(source: &mut impl BufRead) -> std::io::Result<Self> {
        let entry_count = VariableUInt::read(source)?.0 as usize;
        let mut entries = Vec::with_capacity(entry_count);
        for _ in 0..entry_count {
            let name = PascalString::read(source)?.0;
            let entry = ShadingPassDirectoryEntry::read(source)?;

            entries.push((name, entry));
        }

        Ok(Self { entries })
    }

    pub async fn read_async(
        mut source: Pin<&mut (impl AsyncBufRead + ?Sized)>,
    ) -> std::io::Result<Self> {
        let VariableUInt(entry_count) = VariableUInt::read_async(source.as_mut()).await?;
        let mut entries = Vec::with_capacity(entry_count as _);
        for _ in 0..entry_count {
            let PascalString(name) = PascalString::read_async(source.as_mut()).await?;
            let entry = ShadingPassDirectoryEntry::read_async(source.as_mut()).await?;

            entries.push((name, entry))
        }

        Ok(Self { entries })
    }
}

pub enum ShadingPassDirectoryEntry {
    Located(u64),
    SimpleDeriveBuiltin(String),
}
impl ShadingPassDirectoryEntry {
    pub fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        match self {
            &Self::Located(x) => {
                sink.write_all(&[0])?;
                Ok(1 + VariableUInt(x as _).write(sink)?)
            }
            Self::SimpleDeriveBuiltin(name) => {
                sink.write_all(&[1])?;
                Ok(1 + PascalStr(name).write(sink)?)
            }
        }
    }

    pub fn read(source: &mut impl BufRead) -> std::io::Result<Self> {
        let mut first_byte = [0u8];
        source.read_exact(&mut first_byte)?;

        match first_byte[0] {
            0 => Ok(Self::Located(VariableUInt::read(source)?.0 as _)),
            1 => Ok(Self::SimpleDeriveBuiltin(PascalString::read(source)?.0)),
            x => panic!("invalid ShadingPassDirectoryEntry first byte: 0x{x:02x}"),
        }
    }

    async fn read_async(
        mut source: Pin<&mut (impl AsyncBufRead + ?Sized)>,
    ) -> std::io::Result<Self> {
        match read_byte_async(source.as_mut()).await? {
            0 => Ok(Self::Located(
                VariableUInt::read_async(source).await?.0 as _,
            )),
            1 => Ok(Self::SimpleDeriveBuiltin(
                PascalString::read_async(source).await?.0,
            )),
            x => panic!("invalid ShadingPassDirectoryEntry first byte: 0x{x:02x}"),
        }
    }
}

pub struct ShadingPassVk {
    pub option_overrides: RenderingOptionOverrides,
    pub vertex_semantic_to_location: Vec<(VertexInputSemantic, u32)>,
    pub vertex_entry_point_name: Option<String>,
    pub fragment_entry_point_name: Option<String>,
    pub code: Vec<u32>,
}
impl ShadingPassVk {
    pub fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        let mut writes = self.option_overrides.write(sink)?;

        writes += VariableUInt(self.vertex_semantic_to_location.len() as _).write(sink)?;
        for (n, l) in self.vertex_semantic_to_location.iter() {
            writes += n.write(sink)?;
            writes += VariableUInt(*l).write(sink)?;
        }

        let mut stage_flags = 0u8;
        if self.vertex_entry_point_name.is_some() {
            stage_flags |= 0x01;
        }
        if self.fragment_entry_point_name.is_some() {
            stage_flags |= 0x02;
        }
        sink.write_all(&[stage_flags])?;
        writes += 1;
        if let Some(ref x) = self.vertex_entry_point_name {
            writes += PascalStr(x).write(sink)?;
        }
        if let Some(ref x) = self.fragment_entry_point_name {
            writes += PascalStr(x).write(sink)?;
        }

        writes += VariableUInt(self.code.len() as _).write(sink)?;
        sink.write_all(unsafe {
            core::slice::from_raw_parts(self.code.as_ptr() as *const u8, self.code.len() << 2)
        })?;

        Ok(writes + (self.code.len() << 2))
    }

    pub fn read(source: &mut impl BufRead) -> std::io::Result<Self> {
        let option_overrides = RenderingOptionOverrides::read(source)?;

        let vertex_semantic_to_location_count = VariableUInt::read(source)?.0 as usize;
        let mut vertex_semantic_to_location = Vec::with_capacity(vertex_semantic_to_location_count);
        for _ in 0..vertex_semantic_to_location_count {
            let name = VertexInputSemantic::read(source)?;
            let location = VariableUInt::read(source)?.0;
            vertex_semantic_to_location.push((name, location));
        }

        let mut stage_flags = [0u8];
        source.read_exact(&mut stage_flags)?;
        let vertex_entry_point_name = if (stage_flags[0] & 0x01) == 0x01 {
            Some(PascalString::read(source)?.0)
        } else {
            None
        };
        let fragment_entry_point_name = if (stage_flags[0] & 0x02) == 0x02 {
            Some(PascalString::read(source)?.0)
        } else {
            None
        };

        let code_word_count = VariableUInt::read(source)?.0 as usize;
        let mut code = Vec::<u32>::with_capacity(code_word_count);
        source.read_exact(unsafe {
            core::slice::from_raw_parts_mut(
                code.spare_capacity_mut().as_mut_ptr() as *mut u8,
                code_word_count << 2,
            )
        })?;
        unsafe {
            code.set_len(code.capacity());
        }

        Ok(Self {
            option_overrides,
            vertex_semantic_to_location,
            vertex_entry_point_name,
            fragment_entry_point_name,
            code,
        })
    }

    pub async fn read_async(
        mut source: Pin<&mut (impl AsyncBufRead + ?Sized)>,
    ) -> std::io::Result<Self> {
        let option_overrides = RenderingOptionOverrides::read_async(source.as_mut()).await?;

        let vertex_semantic_to_location_count =
            VariableUInt::read_async(source.as_mut()).await?.0 as usize;
        let mut vertex_semantic_to_location = Vec::with_capacity(vertex_semantic_to_location_count);
        for _ in 0..vertex_semantic_to_location_count {
            let name = VertexInputSemantic::read_async(source.as_mut()).await?;
            let location = VariableUInt::read_async(source.as_mut()).await?.0;
            vertex_semantic_to_location.push((name, location));
        }

        let stage_flags = read_byte_async(source.as_mut()).await?;
        let vertex_entry_point_name = if stage_flags & 0x01 != 0 {
            Some(PascalString::read_async(source.as_mut()).await?.0)
        } else {
            None
        };
        let fragment_entry_point_name = if stage_flags & 0x02 != 0 {
            Some(PascalString::read_async(source.as_mut()).await?.0)
        } else {
            None
        };

        let code_word_count = VariableUInt::read_async(source.as_mut()).await?.0 as usize;
        let mut code = Vec::<u32>::with_capacity(code_word_count);
        read_exact_async_pinned(source.as_mut(), unsafe {
            core::slice::from_raw_parts_mut(
                code.spare_capacity_mut().as_mut_ptr() as *mut core::mem::MaybeUninit<u8>,
                code_word_count << 2,
            )
        })
        .await?;
        unsafe {
            code.set_len(code.capacity());
        }

        Ok(Self {
            option_overrides,
            vertex_semantic_to_location,
            vertex_entry_point_name,
            fragment_entry_point_name,
            code,
        })
    }
}

impl RenderingOptionOverrides {
    fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        let existential_bitmap: u32 = if self.mode.is_some() { 0x01 } else { 0 }
            | if self.culling.is_some() { 0x02 } else { 0 }
            | if self.front_face.is_some() { 0x04 } else { 0 };
        sink.write_all(b32(&existential_bitmap))?;
        let mut writes = 4;

        if let Some(ref x) = self.mode {
            writes += x.write(sink)?;
        }
        if let Some(ref x) = self.culling {
            writes += x.write(sink)?;
        }
        if let Some(ref x) = self.front_face {
            writes += x.write(sink)?;
        }

        Ok(writes)
    }

    fn read(source: &mut impl Read) -> std::io::Result<Self> {
        let mut existential_bitmap = 0u32;
        source.read_exact(b32m(&mut existential_bitmap))?;

        let mode = if (existential_bitmap & 0x01) == 0x01 {
            Some(PolygonRasterizationMode::read(source)?)
        } else {
            None
        };
        let culling = if (existential_bitmap & 0x02) == 0x02 {
            Some(FaceCulling::read(source)?)
        } else {
            None
        };
        let front_face = if (existential_bitmap & 0x04) == 0x04 {
            Some(FrontFace::read(source)?)
        } else {
            None
        };

        Ok(Self {
            mode,
            culling,
            front_face,
        })
    }

    async fn read_async(mut source: Pin<&mut (impl AsyncRead + ?Sized)>) -> std::io::Result<Self> {
        let mut existential_bitmap = 0u32;
        core::future::poll_fn(|cx| source.as_mut().poll_read(cx, b32m(&mut existential_bitmap)))
            .await?;

        let mode = if existential_bitmap & 0x01 != 0 {
            Some(PolygonRasterizationMode::read_async(source.as_mut()).await?)
        } else {
            None
        };
        let culling = if existential_bitmap & 0x02 != 0 {
            Some(FaceCulling::read_async(source.as_mut()).await?)
        } else {
            None
        };
        let front_face = if existential_bitmap & 0x04 != 0 {
            Some(FrontFace::read_async(source.as_mut()).await?)
        } else {
            None
        };

        Ok(Self {
            mode,
            culling,
            front_face,
        })
    }
}

impl PropertyType {
    fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        match self {
            Self::UInt => sink.write_all(&[0]).map(|_| 1),
            Self::Int => sink.write_all(&[1]).map(|_| 1),
            Self::Float => sink.write_all(&[2]).map(|_| 1),
            Self::Float2 => sink.write_all(&[3]).map(|_| 1),
            Self::Float4 => sink.write_all(&[4]).map(|_| 1),
            Self::RGB => sink.write_all(&[5]).map(|_| 1),
            Self::Texture2D => sink.write_all(&[6]).map(|_| 1),
        }
    }

    fn read(source: &mut impl Read) -> std::io::Result<Self> {
        let mut first_byte = [0u8];
        source.read_exact(&mut first_byte)?;

        match first_byte[0] {
            0 => Ok(Self::UInt),
            1 => Ok(Self::Int),
            2 => Ok(Self::Float),
            3 => Ok(Self::Float2),
            4 => Ok(Self::Float4),
            5 => Ok(Self::RGB),
            6 => Ok(Self::Texture2D),
            x => panic!("invalid PropertyType first byte: 0x{x:02x}"),
        }
    }

    async fn read_async(source: Pin<&mut (impl AsyncRead + ?Sized)>) -> std::io::Result<Self> {
        match read_byte_async(source).await? {
            0 => Ok(Self::UInt),
            1 => Ok(Self::Int),
            2 => Ok(Self::Float),
            3 => Ok(Self::Float2),
            4 => Ok(Self::Float4),
            5 => Ok(Self::RGB),
            6 => Ok(Self::Texture2D),
            x => panic!("invalid PropertyType first byte: 0x{x:02x}"),
        }
    }
}

impl PropertyMappingVk {
    fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        match self {
            Self::Direct(x) => {
                sink.write_all(&[0])?;
                Ok(1 + x.write(sink)?)
            }
            Self::Splitted(xs) => {
                sink.write_all(&[1])?;
                let mut content_writes = VariableUInt(xs.len() as _).write(sink)?;
                for x in xs {
                    content_writes += x.write(sink)?;
                }

                Ok(1 + content_writes)
            }
        }
    }

    fn read(source: &mut impl BufRead) -> std::io::Result<Self> {
        let mut first_byte = [0u8];
        source.read_exact(&mut first_byte)?;

        match first_byte[0] {
            0 => Ok(Self::Direct(PropertyDestinationVk::read(source)?)),
            1 => {
                let count = VariableUInt::read(source)?.0 as usize;
                let mut xs = Vec::with_capacity(count);
                for _ in 0..count {
                    xs.push(PropertyDestinationVk::read(source)?);
                }

                Ok(Self::Splitted(xs))
            }
            x => panic!("invalid PropertyMappingVk first byte: 0x{x:02x}"),
        }
    }

    async fn read_async(
        mut source: Pin<&mut (impl AsyncBufRead + ?Sized)>,
    ) -> std::io::Result<Self> {
        match read_byte_async(source.as_mut()).await? {
            0 => Ok(Self::Direct(
                PropertyDestinationVk::read_async(source).await?,
            )),
            1 => {
                let VariableUInt(count) = VariableUInt::read_async(source.as_mut()).await?;
                let mut xs = Vec::with_capacity(count as _);
                for _ in 0..count {
                    xs.push(PropertyDestinationVk::read_async(source.as_mut()).await?);
                }

                Ok(Self::Splitted(xs))
            }
            x => panic!("invalid PropertyMappingVk first byte: 0x{x:02x}"),
        }
    }
}

impl PropertyDestinationVk {
    fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        match *self {
            Self::SpecConstant(n) => {
                sink.write_all(&[0])?;
                Ok(1 + VariableUInt(n as _).write(sink)?)
            }
            Self::PushConstantBlock(n) => {
                sink.write_all(&[1])?;
                Ok(1 + VariableUInt(n as _).write(sink)?)
            }
            Self::DescriptorSet(n) => {
                sink.write_all(&[2])?;
                Ok(1 + VariableUInt(n as _).write(sink)?)
            }
            Self::RealtimeBuffer(n) => {
                sink.write_all(&[3])?;
                Ok(1 + VariableUInt(n as _).write(sink)?)
            }
        }
    }

    fn read(source: &mut impl BufRead) -> std::io::Result<Self> {
        let mut first_byte = [0u8];
        source.read_exact(&mut first_byte)?;

        match first_byte[0] {
            0 => Ok(Self::SpecConstant(VariableUInt::read(source)?.0 as _)),
            1 => Ok(Self::PushConstantBlock(VariableUInt::read(source)?.0 as _)),
            2 => Ok(Self::DescriptorSet(VariableUInt::read(source)?.0 as _)),
            3 => Ok(Self::RealtimeBuffer(VariableUInt::read(source)?.0 as _)),
            x => panic!("invalid PropertyDestinationVk first byte: 0x{x:02x}"),
        }
    }

    async fn read_async(
        mut source: Pin<&mut (impl AsyncBufRead + ?Sized)>,
    ) -> std::io::Result<Self> {
        match read_byte_async(source.as_mut()).await? {
            0 => Ok(Self::SpecConstant(
                VariableUInt::read_async(source).await?.0 as _,
            )),
            1 => Ok(Self::PushConstantBlock(
                VariableUInt::read_async(source).await?.0 as _,
            )),
            2 => Ok(Self::DescriptorSet(
                VariableUInt::read_async(source).await?.0 as _,
            )),
            3 => Ok(Self::RealtimeBuffer(
                VariableUInt::read_async(source).await?.0 as _,
            )),
            x => panic!("invalid PropertyDestinationVk first byte: 0x{x:02x}"),
        }
    }
}

impl DescriptorTypeVk {
    fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        match self {
            &Self::UniformBuffer { size_bytes } => {
                sink.write_all(&[0])?;
                Ok(1 + VariableUInt(size_bytes as _).write(sink)?)
            }
            Self::CombinedImageSampler => {
                sink.write_all(&[1])?;
                Ok(1)
            }
        }
    }

    fn read(source: &mut impl BufRead) -> std::io::Result<Self> {
        let mut first_byte = [0u8];
        source.read_exact(&mut first_byte)?;

        match first_byte[0] {
            0 => Ok(Self::UniformBuffer {
                size_bytes: VariableUInt::read(source)?.0 as _,
            }),
            1 => Ok(Self::CombinedImageSampler),
            x => panic!("invalid DescriptorTypeVk first byte: 0x{x:02x}"),
        }
    }

    async fn read_async(
        mut source: Pin<&mut (impl AsyncBufRead + ?Sized)>,
    ) -> std::io::Result<Self> {
        match read_byte_async(source.as_mut()).await? {
            0 => Ok(Self::UniformBuffer {
                size_bytes: VariableUInt::read_async(source).await?.0 as _,
            }),
            1 => Ok(Self::CombinedImageSampler),
            x => panic!("invalid DescriptorTypeVk first byte: 0x{x:02x}"),
        }
    }
}

impl PolygonRasterizationMode {
    fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        match self {
            Self::Point => sink.write_all(&[0]).map(|_| 1),
            Self::Line => sink.write_all(&[1]).map(|_| 1),
            Self::Fill => sink.write_all(&[2]).map(|_| 1),
        }
    }

    fn read(source: &mut impl Read) -> std::io::Result<Self> {
        let mut first_byte = [0u8];
        source.read_exact(&mut first_byte)?;

        match first_byte[0] {
            0 => Ok(Self::Point),
            1 => Ok(Self::Line),
            2 => Ok(Self::Fill),
            x => panic!("invalid PolygonRasterizationMode first byte: 0x{x:02x}"),
        }
    }

    async fn read_async(source: Pin<&mut (impl AsyncRead + ?Sized)>) -> std::io::Result<Self> {
        match read_byte_async(source).await? {
            0 => Ok(Self::Point),
            1 => Ok(Self::Line),
            2 => Ok(Self::Fill),
            x => panic!("invalid PolygonRasterizationMode first byte: 0x{x:02x}"),
        }
    }
}

impl FaceCulling {
    fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        match self {
            Self::None => sink.write_all(&[0]).map(|_| 1),
            Self::Front => sink.write_all(&[1]).map(|_| 1),
            Self::Back => sink.write_all(&[2]).map(|_| 1),
            Self::Both => sink.write_all(&[3]).map(|_| 1),
        }
    }

    fn read(source: &mut impl Read) -> std::io::Result<Self> {
        let mut first_byte = [0u8];
        source.read_exact(&mut first_byte)?;

        match first_byte[0] {
            0 => Ok(Self::None),
            1 => Ok(Self::Front),
            2 => Ok(Self::Back),
            3 => Ok(Self::Both),
            x => panic!("invalid FaceCulling first byte: 0x{x:02x}"),
        }
    }

    async fn read_async(source: Pin<&mut (impl AsyncRead + ?Sized)>) -> std::io::Result<Self> {
        match read_byte_async(source).await? {
            0 => Ok(Self::None),
            1 => Ok(Self::Front),
            2 => Ok(Self::Back),
            3 => Ok(Self::Both),
            x => panic!("invalid FaceCulling first byte: 0x{x:02x}"),
        }
    }
}

impl FrontFace {
    fn write(&self, sink: &mut impl Write) -> std::io::Result<usize> {
        match self {
            Self::CounterClockwise => sink.write_all(&[0]).map(|_| 1),
            Self::Clockwise => sink.write_all(&[1]).map(|_| 1),
        }
    }

    fn read(source: &mut impl Read) -> std::io::Result<Self> {
        let mut first_byte = [0u8];
        source.read_exact(&mut first_byte)?;

        match first_byte[0] {
            0 => Ok(Self::CounterClockwise),
            1 => Ok(Self::Clockwise),
            x => panic!("invalid FrontFace first byte: 0x{x:02x}"),
        }
    }

    async fn read_async(source: Pin<&mut (impl AsyncRead + ?Sized)>) -> std::io::Result<Self> {
        match read_byte_async(source).await? {
            0 => Ok(Self::CounterClockwise),
            1 => Ok(Self::Clockwise),
            x => panic!("invalid FrontFace first byte: 0x{x:02x}"),
        }
    }
}
