use std::{
    ffi::CStr,
    io::{Read, Write},
};

use bedrock as br;

use crate::config::blending::PresetBlendingConfigurationContent;

pub const OBJECT_PARAMETERS_FULL_TYPE_NAME: &CStr = c"peridot.ObjectParameters";
pub const CAMERA_PARAMETERS_FULL_TYPE_NAME: &CStr = c"peridot.CameraParameters";

pub const MATERIAL_PROPERTY_ATTR_NAME: &CStr = c"peridot_material_property";
pub const PROPERTY_NAME_ATTR_NAME: &CStr = c"peridot_property_name";

#[derive(Debug)]
pub enum FragmentShaderAdditionalAttribute {
    Blending(BlendingAttachment, Blending),
    BlendingConstant([f32; 4]),
}
impl FragmentShaderAdditionalAttribute {
    #[tracing::instrument(skip(a), fields(name = ?a.name()))]
    pub fn parse(a: &slang::reflection::Attribute) -> Option<Self> {
        fn validate_argument_count(a: &slang::reflection::Attribute, count: u32) -> bool {
            if a.argument_count() != count {
                tracing::warn!(count = a.argument_count(), "argument count mismatched");
                false
            } else {
                true
            }
        }

        match a.name() {
            x if x == c"peridot_blending" => {
                if !validate_argument_count(a, 3) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::Default,
                    Blending::Specified {
                        src_factor: BlendFactor::from_attr_arg(a, 0)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 1)?,
                        op: BlendOp::from_attr_arg(a, 2)?,
                    },
                ))
            }
            x if x == c"peridot_blending_preset" => {
                if !validate_argument_count(a, 1) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::Default,
                    Blending::Preset(BlendPreset::from_attr_arg(a, 0)?),
                ))
            }
            x if x == c"peridot_blending_preset_for" => {
                if !validate_argument_count(a, 2) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::specified_from_attr_arg(a, 0)?,
                    Blending::Preset(BlendPreset::from_attr_arg(a, 1)?),
                ))
            }
            x if x == c"peridot_blending_for" => {
                if !validate_argument_count(a, 4) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::specified_from_attr_arg(a, 0)?,
                    Blending::Specified {
                        src_factor: BlendFactor::from_attr_arg(a, 1)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 2)?,
                        op: BlendOp::from_attr_arg(a, 3)?,
                    },
                ))
            }
            x if x == c"peridot_blending_color" => {
                if !validate_argument_count(a, 3) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::Default,
                    Blending::Color {
                        src_factor: BlendFactor::from_attr_arg(a, 0)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 1)?,
                        op: BlendOp::from_attr_arg(a, 2)?,
                    },
                ))
            }
            x if x == c"peridot_blending_color_for" => {
                if !validate_argument_count(a, 4) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::specified_from_attr_arg(a, 0)?,
                    Blending::Color {
                        src_factor: BlendFactor::from_attr_arg(a, 1)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 2)?,
                        op: BlendOp::from_attr_arg(a, 3)?,
                    },
                ))
            }
            x if x == c"peridot_blending_alpha" => {
                if !validate_argument_count(a, 3) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::Default,
                    Blending::Alpha {
                        src_factor: BlendFactor::from_attr_arg(a, 0)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 1)?,
                        op: BlendOp::from_attr_arg(a, 2)?,
                    },
                ))
            }
            x if x == c"peridot_blending_alpha_for" => {
                if !validate_argument_count(a, 4) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::specified_from_attr_arg(a, 0)?,
                    Blending::Alpha {
                        src_factor: BlendFactor::from_attr_arg(a, 1)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 2)?,
                        op: BlendOp::from_attr_arg(a, 3)?,
                    },
                ))
            }
            x if x == c"peridot_blending_const" => {
                if !validate_argument_count(a, 4) {
                    return None;
                }

                Some(Self::BlendingConstant([
                    a.argument_value_float(0)
                        .inspect_err(|e| tracing::warn!(reason = ?e, "argument_value_float"))
                        .ok()?,
                    a.argument_value_float(1)
                        .inspect_err(|e| tracing::warn!(reason = ?e, "argument_value_float"))
                        .ok()?,
                    a.argument_value_float(2)
                        .inspect_err(|e| tracing::warn!(reason = ?e, "argument_value_float"))
                        .ok()?,
                    a.argument_value_float(3)
                        .inspect_err(|e| tracing::warn!(reason = ?e, "argument_value_float"))
                        .ok()?,
                ]))
            }
            name => {
                tracing::warn!(?name, "unknown fragment shader attribute");
                None
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BlendingAttachment {
    Default,
    Specified(u32),
}
impl BlendingAttachment {
    #[tracing::instrument(skip(attr))]
    fn specified_from_attr_arg(attr: &slang::reflection::Attribute, index: u32) -> Option<Self> {
        let value = match attr.argument_value_int(index) {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, "attr.argument_value_int");
                return None;
            }
        };

        match u32::try_from(value) {
            Ok(x) => Some(Self::Specified(x)),
            Err(_) => {
                tracing::warn!(value, "attachment index is out of range");
                None
            }
        }
    }
}

#[derive(Debug)]
pub enum Blending {
    Preset(BlendPreset),
    Specified {
        src_factor: BlendFactor,
        dst_factor: BlendFactor,
        op: BlendOp,
    },
    Color {
        src_factor: BlendFactor,
        dst_factor: BlendFactor,
        op: BlendOp,
    },
    Alpha {
        src_factor: BlendFactor,
        dst_factor: BlendFactor,
        op: BlendOp,
    },
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum BlendFactor {
    SrcAlpha = 0,
    DstAlpha = 1,
    InvSrcAlpha = 2,
    InvDstAlpha = 3,
    SrcColor = 4,
    DstColor = 5,
    InvSrcColor = 6,
    InvDstColor = 7,
    Constant = 8,
    ConstantOne = 9,
    ConstantZero = 10,
}
impl BlendFactor {
    #[tracing::instrument(skip(attr))]
    fn from_attr_arg(attr: &slang::reflection::Attribute, index: u32) -> Option<Self> {
        let value = match attr.argument_value_int(index) {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, "attr.argument_value_int");
                return None;
            }
        };

        if Self::SrcAlpha as i32 <= value && value <= Self::ConstantZero as i32 {
            Some(unsafe { core::mem::transmute::<u8, Self>(value as _) })
        } else {
            tracing::warn!(value, "value out of range");
            None
        }
    }

    pub const fn into_vk(self) -> br::vk::VkBlendFactor {
        match self {
            Self::SrcAlpha => br::vk::VK_BLEND_FACTOR_SRC_ALPHA,
            Self::DstAlpha => br::vk::VK_BLEND_FACTOR_DST_ALPHA,
            Self::InvSrcAlpha => br::vk::VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
            Self::InvDstAlpha => br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA,
            Self::SrcColor => br::vk::VK_BLEND_FACTOR_SRC_COLOR,
            Self::DstColor => br::vk::VK_BLEND_FACTOR_DST_COLOR,
            Self::InvSrcColor => br::vk::VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR,
            Self::InvDstColor => br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR,
            // TODO: should split alpha/color?
            Self::Constant => br::vk::VK_BLEND_FACTOR_CONSTANT_ALPHA,
            Self::ConstantOne => br::vk::VK_BLEND_FACTOR_ONE,
            Self::ConstantZero => br::vk::VK_BLEND_FACTOR_ZERO,
        }
    }

    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&[*self as u8])
    }

    pub fn read(r: &mut (impl Read + ?Sized)) -> Result<Self, BlendFactorReadError> {
        let mut byte = [0u8];
        r.read_exact(&mut byte)?;
        match byte[0] {
            v if Self::SrcAlpha as u8 <= v && v <= Self::ConstantZero as u8 => {
                Ok(unsafe { core::mem::transmute::<u8, Self>(v) })
            }
            v => Err(BlendFactorReadError::ByteOutOfRange(v)),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum BlendFactorReadError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("blend factor byte out of range: {0}")]
    ByteOutOfRange(u8),
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum BlendOp {
    Add = 0,
    Min = 1,
    Max = 2,
}
impl BlendOp {
    #[tracing::instrument(skip(attr))]
    fn from_attr_arg(attr: &slang::reflection::Attribute, index: u32) -> Option<Self> {
        let value = match attr.argument_value_int(index) {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, "attr.argument_value_int");
                return None;
            }
        };

        if Self::Add as i32 <= value && value <= Self::Max as i32 {
            Some(unsafe { core::mem::transmute::<u8, Self>(value as _) })
        } else {
            tracing::warn!(value, "value out of range");
            None
        }
    }

    pub const fn into_vk(self) -> br::vk::VkBlendOp {
        match self {
            Self::Add => br::vk::VK_BLEND_OP_ADD,
            Self::Min => br::vk::VK_BLEND_OP_MIN,
            Self::Max => br::vk::VK_BLEND_OP_MAX,
        }
    }

    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&[*self as u8])
    }

    pub fn read(r: &mut (impl Read + ?Sized)) -> Result<Self, BlendOpReadError> {
        let mut byte = [0u8];
        r.read_exact(&mut byte)?;
        match byte[0] {
            v if Self::Add as u8 <= v && v <= Self::Max as u8 => {
                Ok(unsafe { core::mem::transmute::<u8, Self>(v) })
            }
            v => Err(BlendOpReadError::ByteOutOfRange(v)),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum BlendOpReadError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("blend op byte out of range: {0}")]
    ByteOutOfRange(u8),
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum BlendPreset {
    Disabled = 0,
    StraightAlpha = 1,
    PremultipliedAlpha = 2,
}
impl BlendPreset {
    #[tracing::instrument(skip(attr))]
    fn from_attr_arg(attr: &slang::reflection::Attribute, index: u32) -> Option<Self> {
        let value = match attr.argument_value_int(index) {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, "attr.argument_value_int");
                return None;
            }
        };

        if Self::Disabled as i32 <= value && value <= Self::PremultipliedAlpha as i32 {
            Some(unsafe { core::mem::transmute::<u8, Self>(value as _) })
        } else {
            tracing::warn!(value, "value out of range");
            None
        }
    }

    pub fn make_content(&self) -> PresetBlendingConfigurationContent {
        match self {
            Self::Disabled => PresetBlendingConfigurationContent {
                enabled: false,
                ..Self::PremultipliedAlpha.make_content()
            },
            Self::StraightAlpha => PresetBlendingConfigurationContent {
                enabled: true,
                src_color_factor: BlendFactor::SrcAlpha,
                dst_color_factor: BlendFactor::InvSrcAlpha,
                color_op: BlendOp::Add,
                src_alpha_factor: BlendFactor::SrcAlpha,
                dst_alpha_factor: BlendFactor::InvSrcAlpha,
                alpha_op: BlendOp::Add,
            },
            Self::PremultipliedAlpha => PresetBlendingConfigurationContent {
                enabled: true,
                src_color_factor: BlendFactor::ConstantOne,
                dst_color_factor: BlendFactor::InvSrcAlpha,
                color_op: BlendOp::Add,
                src_alpha_factor: BlendFactor::ConstantOne,
                dst_alpha_factor: BlendFactor::InvSrcAlpha,
                alpha_op: BlendOp::Add,
            },
        }
    }

    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&[*self as u8])
    }

    pub fn read(r: &mut (impl Read + ?Sized)) -> Result<Self, BlendPresetReadError> {
        let mut byte = [0u8];
        r.read_exact(&mut byte)?;
        match byte[0] {
            v if Self::Disabled as u8 <= v && v <= Self::PremultipliedAlpha as u8 => {
                Ok(unsafe { core::mem::transmute::<u8, Self>(v) })
            }
            v => Err(BlendPresetReadError::ByteOutOfRange(v)),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum BlendPresetReadError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("blend preset byte out of range: {0}")]
    ByteOutOfRange(u8),
}
