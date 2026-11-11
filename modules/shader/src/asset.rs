use std::{
    collections::HashMap,
    io::{BufRead, IoSlice, IoSliceMut, Write},
};

use bitflags::bitflags;
use peridot::VertexInputSemantic;
use peridot_serialization_utils::{PascalStr, PascalString, VariableULong};
use vectored_io_helper::{read_vectored_all, write_vectored_all};

pub struct AssetCore;
impl peridot::LogicalAssetData for AssetCore {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_SHADER;
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssetInputVariableType {
    Float4,
    Texture2D,
}
impl AssetInputVariableType {
    pub fn from_slang_type(t: &slang::reflection::Type) -> Option<Self> {
        match t.kind() {
            slang::reflection::TypeKind::ConstantBuffer
            | slang::reflection::TypeKind::ParameterBlock
            | slang::reflection::TypeKind::Struct => {
                // no support for compound objects
                None
            }
            slang::reflection::TypeKind::Array => todo!("array type property"),
            slang::reflection::TypeKind::Vector => {
                let element_count = t.element_count(None);
                let element_type = t.element_type().expect("no vector element type");
                let element_scalar_type = element_type.scalar_type();
                match (element_scalar_type, element_count) {
                    (slang::reflection::ScalarType::Float32, 4) => Some(Self::Float4),
                    (t, c) => {
                        eprintln!("unknown vector element combination: {t:?} x {c}");
                        None
                    }
                }
            }
            slang::reflection::TypeKind::Matrix => todo!("matrix type property"),
            slang::reflection::TypeKind::Resource => {
                let shape = t.resource_shape();
                let result_type = t.resource_result_type().expect("no result type");
                match (
                    shape & slang::ffi::SLANG_RESOURCE_BASE_SHAPE_MASK,
                    result_type.kind(),
                ) {
                    (slang::ffi::SLANG_TEXTURE_2D, slang::reflection::TypeKind::Vector) => {
                        let element_count = result_type.element_count(None);
                        let element_type =
                            result_type.element_type().expect("no vector element type");
                        let element_scalar_type = element_type.scalar_type();
                        match (element_scalar_type, element_count) {
                            (slang::reflection::ScalarType::Float32, 4) => Some(Self::Texture2D),
                            (t, c) => {
                                eprintln!(
                                    "unknown resource result vector combination: {t:?} x {c}"
                                );
                                None
                            }
                        }
                    }
                    (s, k) => {
                        eprintln!("unknown resource shape/result combination: {s:?}/{k:?}");
                        None
                    }
                }
            }
            _ => unimplemented!("unknown type kind"),
        }
    }
}

#[derive(Debug)]
pub struct AssetVertexInput {
    pub location: u8,
}

#[derive(Debug)]
pub struct AssetMaterialPropertyVk {
    pub name: String,
    pub property_name_override: Option<String>,
    pub r#type: AssetInputVariableType,
    pub descriptor_binding: u32,
    pub descriptor_set: u32,
    pub offset: u32,
    pub size: u32,
}
impl AssetMaterialPropertyVk {
    fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        PascalStr(&self.name).write(w)?;
        PascalStr(self.property_name_override.as_deref().unwrap_or("")).write(w)?;
        write_vectored_all(
            w,
            &mut [
                IoSlice::new(&[self.r#type as u8]),
                IoSlice::new(&self.descriptor_set.to_ne_bytes()),
                IoSlice::new(&self.descriptor_binding.to_ne_bytes()),
                IoSlice::new(&self.offset.to_ne_bytes()),
                IoSlice::new(&self.size.to_ne_bytes()),
            ],
        )?;

        Ok(())
    }

    fn read(r: &mut (impl BufRead + ?Sized), is_native_endian: bool) -> std::io::Result<Self> {
        let name = PascalString::read(r)?;
        let property_name_override = PascalString::read(r)?;
        let property_name_override = if property_name_override.0.is_empty() {
            None
        } else {
            Some(property_name_override.0)
        };
        let mut r#type = core::mem::MaybeUninit::<AssetInputVariableType>::uninit();
        let mut descriptor_set = core::mem::MaybeUninit::<u32>::uninit();
        let mut descriptor_binding = core::mem::MaybeUninit::<u32>::uninit();
        let mut offset = core::mem::MaybeUninit::<u32>::uninit();
        let mut size = core::mem::MaybeUninit::<u32>::uninit();
        read_vectored_all(
            r,
            &mut [
                mkioslicem(&mut r#type),
                mkioslicem(&mut descriptor_set),
                mkioslicem(&mut descriptor_binding),
                mkioslicem(&mut offset),
                mkioslicem(&mut size),
            ],
        )?;
        let r#type = unsafe { r#type.assume_init() };
        let descriptor_set = unsafe { descriptor_set.assume_init() };
        let descriptor_binding = unsafe { descriptor_binding.assume_init() };
        let offset = unsafe { offset.assume_init() };
        let size = unsafe { size.assume_init() };

        if is_native_endian {
            Ok(Self {
                name: name.0,
                property_name_override,
                r#type,
                descriptor_set,
                descriptor_binding,
                offset,
                size,
            })
        } else {
            Ok(Self {
                name: name.0,
                property_name_override,
                r#type,
                descriptor_set: descriptor_set.swap_bytes(),
                descriptor_binding: descriptor_binding.swap_bytes(),
                offset: offset.swap_bytes(),
                size: size.swap_bytes(),
            })
        }
    }
}

#[derive(Debug)]
pub struct AssetDescriptorSlotFieldVk {
    pub offset: u32,
    pub size: u32,
}

#[derive(Debug)]
pub enum AssetInputVariableLayoutVk {
    CombinedImageSampler,
    UniformBuffer {
        fields: Vec<AssetDescriptorSlotFieldVk>,
    },
}
impl AssetInputVariableLayoutVk {
    fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        match self {
            Self::CombinedImageSampler => {
                w.write_all(&[0])?;
            }
            Self::UniformBuffer { fields } => {
                w.write_all(&[128])?;
                VariableULong(fields.len() as _).write(w)?;
                let mut fields = fields.iter().collect::<Vec<_>>();
                fields.sort_by_key(|v| v.offset);
                for f in fields {
                    write_vectored_all(
                        w,
                        &mut [
                            IoSlice::new(&f.offset.to_ne_bytes()),
                            IoSlice::new(&f.size.to_ne_bytes()),
                        ],
                    )?;
                }
            }
        }

        Ok(())
    }

    fn read(r: &mut (impl BufRead + ?Sized), is_native_endian: bool) -> std::io::Result<Self> {
        let mut discriminant = [0u8];
        r.read_exact(&mut discriminant)?;
        match discriminant[0] {
            0 => Ok(Self::CombinedImageSampler),
            128 => {
                let field_count = VariableULong::read(r)?.0 as usize;
                let mut fields = Vec::with_capacity(field_count);
                for _ in 0..field_count {
                    let mut offset = core::mem::MaybeUninit::<u32>::uninit();
                    let mut size = core::mem::MaybeUninit::<u32>::uninit();
                    read_vectored_all(r, &mut [mkioslicem(&mut offset), mkioslicem(&mut size)])?;
                    let offset = unsafe { offset.assume_init() };
                    let size = unsafe { size.assume_init() };

                    fields.push(AssetDescriptorSlotFieldVk {
                        offset: if is_native_endian {
                            offset
                        } else {
                            offset.swap_bytes()
                        },
                        size: if is_native_endian {
                            size
                        } else {
                            size.swap_bytes()
                        },
                    });
                }

                Ok(Self::UniformBuffer { fields })
            }
            v => unreachable!("invalid discriminant byte: {v}"),
        }
    }
}

pub struct AssetVk {
    pub code_binary: Vec<u32>,
    pub vertex_entry_point_name: String,
    pub fragment_entry_point_name: Option<String>,
    pub vertex_inputs: HashMap<VertexInputSemantic, AssetVertexInput>,
    pub material_properties: Vec<AssetMaterialPropertyVk>,
    pub input_variable_layout: HashMap<(u32, u32), AssetInputVariableLayoutVk>,
    pub object_parameters_placement: Option<(u32, u32)>,
    pub camera_parameters_placement: Option<(u32, u32)>,
}
impl peridot::LogicalAssetData for AssetVk {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_SHADER;
}
impl peridot::FromAssetBlob for AssetVk {
    type Error = std::io::Error;

    #[inline(always)]
    fn from_asset_blob<'a, Blob: peridot::AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error> {
        Self::read(&mut std::io::BufReader::new(
            peridot_native_io::RandomBlobReadSeekAdapter::new(blob),
        ))
    }
}
impl AssetVk {
    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        let mut content_flags = AssetVkContentFlags::empty();
        if self.object_parameters_placement.is_some() {
            content_flags.insert(AssetVkContentFlags::HAS_OBJECT_PARAMETER);
        }
        if self.camera_parameters_placement.is_some() {
            content_flags.insert(AssetVkContentFlags::HAS_CAMERA_PARAMETER);
        }
        if self.fragment_entry_point_name.is_some() {
            content_flags.insert(AssetVkContentFlags::HAS_FRAGMENT_ENTRY_POINT);
        }

        // head
        write_vectored_all(
            w,
            &mut [
                IoSlice::new(&0x1234u16.to_ne_bytes()),
                IoSlice::new(&[content_flags.bits()]),
            ],
        )?;

        // material props
        VariableULong(self.material_properties.len() as _).write(w)?;
        for p in self.material_properties.iter() {
            p.write(w)?;
        }

        // input layouts
        VariableULong(self.vertex_inputs.len() as _).write(w)?;
        for (s, vi) in self.vertex_inputs.iter() {
            s.write(w)?;
            w.write_all(&[vi.location])?;
        }

        VariableULong(self.input_variable_layout.len() as _).write(w)?;
        let mut input_variable_layouts = self.input_variable_layout.iter().collect::<Vec<_>>();
        input_variable_layouts.sort_by_key(|&(k, _)| k);
        for (k, v) in input_variable_layouts {
            write_vectored_all(
                w,
                &mut [
                    IoSlice::new(&k.0.to_ne_bytes()),
                    IoSlice::new(&k.1.to_ne_bytes()),
                ],
            )?;
            v.write(w)?;
        }

        // shader props
        PascalStr(&self.vertex_entry_point_name).write(w)?;
        if let Some(ref x) = self.fragment_entry_point_name {
            PascalStr(x).write(w)?;
        }
        if let Some(ref x) = self.object_parameters_placement {
            write_vectored_all(
                w,
                &mut [
                    IoSlice::new(&x.0.to_ne_bytes()),
                    IoSlice::new(&x.1.to_ne_bytes()),
                ],
            )?;
        }
        if let Some(ref x) = self.camera_parameters_placement {
            write_vectored_all(
                w,
                &mut [
                    IoSlice::new(&x.0.to_ne_bytes()),
                    IoSlice::new(&x.1.to_ne_bytes()),
                ],
            )?;
        }

        // shader code
        VariableULong(self.code_binary.len() as _).write(w)?;
        w.write_all(unsafe {
            core::slice::from_raw_parts(
                self.code_binary.as_ptr().cast(),
                self.code_binary.len() << 2,
            )
        })?;

        Ok(())
    }

    pub fn read(r: &mut (impl BufRead + ?Sized)) -> std::io::Result<Self> {
        // head
        let mut endian_mark = core::mem::MaybeUninit::<u16>::uninit();
        let mut content_flags = core::mem::MaybeUninit::<AssetVkContentFlags>::uninit();
        read_vectored_all(
            r,
            &mut [mkioslicem(&mut endian_mark), mkioslicem(&mut content_flags)],
        )?;
        let endian_mark = unsafe { endian_mark.assume_init() };
        let is_native_endian = if endian_mark == 0x1234 {
            true
        } else if endian_mark.swap_bytes() == 0x1234 {
            false
        } else {
            panic!("invalid endian mark: 0x{endian_mark:04x}")
        };
        let content_flags = unsafe { content_flags.assume_init() };

        // material props
        let material_property_count = VariableULong::read(r)?.0 as usize;
        let mut material_properties = Vec::with_capacity(material_property_count);
        for _ in 0..material_property_count {
            material_properties.push(AssetMaterialPropertyVk::read(r, is_native_endian)?);
        }

        // input layouts
        let vertex_input_count = VariableULong::read(r)?.0 as usize;
        let mut vertex_inputs = HashMap::with_capacity(vertex_input_count);
        for _ in 0..vertex_input_count {
            let semantic = VertexInputSemantic::read(r)?;
            let mut location = [0u8];
            r.read_exact(&mut location)?;

            vertex_inputs.insert(
                semantic,
                AssetVertexInput {
                    location: location[0],
                },
            );
        }

        let input_variable_layout_count = VariableULong::read(r)?.0 as usize;
        let mut input_variable_layout = HashMap::with_capacity(input_variable_layout_count);
        for _ in 0..input_variable_layout_count {
            let mut set = core::mem::MaybeUninit::<u32>::uninit();
            let mut binding = core::mem::MaybeUninit::<u32>::uninit();
            read_vectored_all(r, &mut [mkioslicem(&mut set), mkioslicem(&mut binding)])?;
            let set = unsafe { set.assume_init() };
            let binding = unsafe { binding.assume_init() };
            let content = AssetInputVariableLayoutVk::read(r, is_native_endian)?;

            if is_native_endian {
                input_variable_layout.insert((set, binding), content);
            } else {
                input_variable_layout.insert((set.swap_bytes(), binding.swap_bytes()), content);
            }
        }

        // shader props
        let vertex_entry_point_name = PascalString::read(r)?.0;
        let fragment_entry_point_name =
            if content_flags.contains(AssetVkContentFlags::HAS_FRAGMENT_ENTRY_POINT) {
                Some(PascalString::read(r)?.0)
            } else {
                None
            };
        let object_parameters_placement =
            if content_flags.contains(AssetVkContentFlags::HAS_OBJECT_PARAMETER) {
                let mut set = core::mem::MaybeUninit::<u32>::uninit();
                let mut binding = core::mem::MaybeUninit::<u32>::uninit();
                read_vectored_all(r, &mut [mkioslicem(&mut set), mkioslicem(&mut binding)])?;
                let set = unsafe { set.assume_init() };
                let binding = unsafe { binding.assume_init() };

                Some(if is_native_endian {
                    (set, binding)
                } else {
                    (set.swap_bytes(), binding.swap_bytes())
                })
            } else {
                None
            };
        let camera_parameters_placement =
            if content_flags.contains(AssetVkContentFlags::HAS_CAMERA_PARAMETER) {
                let mut set = core::mem::MaybeUninit::<u32>::uninit();
                let mut binding = core::mem::MaybeUninit::<u32>::uninit();
                read_vectored_all(r, &mut [mkioslicem(&mut set), mkioslicem(&mut binding)])?;
                let set = unsafe { set.assume_init() };
                let binding = unsafe { binding.assume_init() };

                Some(if is_native_endian {
                    (set, binding)
                } else {
                    (set.swap_bytes(), binding.swap_bytes())
                })
            } else {
                None
            };

        // shader code
        let code_binary_words = VariableULong::read(r)?.0 as usize;
        let mut code_binary = Vec::<u32>::with_capacity(code_binary_words);
        r.read_exact(unsafe {
            core::slice::from_raw_parts_mut(
                code_binary.spare_capacity_mut().as_mut_ptr().cast(),
                code_binary_words << 2,
            )
        })?;
        unsafe {
            code_binary.set_len(code_binary_words);
        }
        if !is_native_endian {
            for x in code_binary.iter_mut() {
                *x = x.swap_bytes();
            }
        }

        Ok(Self {
            code_binary,
            vertex_entry_point_name,
            fragment_entry_point_name,
            vertex_inputs,
            material_properties,
            input_variable_layout,
            object_parameters_placement,
            camera_parameters_placement,
        })
    }
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct AssetVkContentFlags : u8 {
        const HAS_OBJECT_PARAMETER = 0x01;
        const HAS_CAMERA_PARAMETER = 0x02;
        const HAS_FRAGMENT_ENTRY_POINT = 0x04;
    }
}

#[inline(always)]
fn mkioslicem<'a, T>(data: &'a mut core::mem::MaybeUninit<T>) -> IoSliceMut<'a> {
    IoSliceMut::new(unsafe {
        core::slice::from_raw_parts_mut(data.as_mut_ptr().cast(), size_of::<T>())
    })
}
