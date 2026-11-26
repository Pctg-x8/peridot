use std::{
    collections::BTreeMap,
    io::{BufRead, Read, Write},
};

use peridot_serialization_utils::{VariableUInt, VariableULong};
use vectored_io_helper::{mkioslice, mkioslicem, read_vectored_all, write_vectored_all};

use crate::slang_bridge::{
    BlendFactorReadError, BlendOpReadError, BlendPresetReadError, Blending, BlendingAttachment,
    FragmentShaderAdditionalAttribute,
};

pub use crate::slang_bridge::{BlendFactor, BlendOp, BlendPreset};

pub fn resolve_cascade(
    target: &mut PresetBlendingConfigurationContent,
    per_attachment: &PartialBlendingConfiguration,
    default: &PartialBlendingConfiguration,
) {
    let mut overrides = BlendingConfigurationContentOverrides::from(per_attachment);
    overrides.merge_lower_cascade(BlendingConfigurationContentOverrides::from(default));

    target.apply_overrides(overrides);
}

pub struct PresetBlendingConfigurationContent {
    pub enabled: bool,
    pub src_color_factor: BlendFactor,
    pub dst_color_factor: BlendFactor,
    pub color_op: BlendOp,
    pub src_alpha_factor: BlendFactor,
    pub dst_alpha_factor: BlendFactor,
    pub alpha_op: BlendOp,
}
impl PresetBlendingConfigurationContent {
    pub fn apply_overrides(&mut self, overrides: BlendingConfigurationContentOverrides) {
        self.enabled = overrides.enabled.unwrap_or(self.enabled);
        self.src_color_factor = overrides.src_color_factor.unwrap_or(self.src_color_factor);
        self.dst_color_factor = overrides.dst_color_factor.unwrap_or(self.dst_color_factor);
        self.color_op = overrides.color_op.unwrap_or(self.color_op);
        self.src_alpha_factor = overrides.src_alpha_factor.unwrap_or(self.src_alpha_factor);
        self.dst_alpha_factor = overrides.dst_alpha_factor.unwrap_or(self.dst_alpha_factor);
        self.alpha_op = overrides.alpha_op.unwrap_or(self.alpha_op);
    }
}

pub struct BlendingConfigurationContentOverrides {
    pub enabled: Option<bool>,
    pub src_color_factor: Option<BlendFactor>,
    pub dst_color_factor: Option<BlendFactor>,
    pub color_op: Option<BlendOp>,
    pub src_alpha_factor: Option<BlendFactor>,
    pub dst_alpha_factor: Option<BlendFactor>,
    pub alpha_op: Option<BlendOp>,
}
impl BlendingConfigurationContentOverrides {
    pub fn merge_lower_cascade(&mut self, lower: Self) {
        self.enabled = self.enabled.or(lower.enabled);
        self.src_color_factor = self.src_color_factor.or(lower.src_color_factor);
        self.dst_color_factor = self.dst_color_factor.or(lower.dst_color_factor);
        self.color_op = self.color_op.or(lower.color_op);
        self.src_alpha_factor = self.src_alpha_factor.or(lower.src_alpha_factor);
        self.dst_alpha_factor = self.dst_alpha_factor.or(lower.dst_alpha_factor);
        self.alpha_op = self.alpha_op.or(lower.alpha_op);
    }
}
impl<'a> From<&'a PartialBlendingConfiguration> for BlendingConfigurationContentOverrides {
    fn from(value: &'a PartialBlendingConfiguration) -> Self {
        let preset = value.preset.map(|x| x.make_content());
        let color = value.color.as_ref().or(value.base.as_ref());
        let alpha = value.alpha.as_ref().or(value.base.as_ref());

        Self {
            enabled: if color.is_some() || alpha.is_some() {
                // force true if there are some overrides
                Some(true)
            } else {
                preset.as_ref().map(|x| x.enabled)
            },
            src_color_factor: color
                .map(|x| x.src)
                .or_else(|| preset.as_ref().map(|x| x.src_color_factor)),
            dst_color_factor: color
                .map(|x| x.dst)
                .or_else(|| preset.as_ref().map(|x| x.dst_color_factor)),
            color_op: color
                .map(|x| x.op)
                .or_else(|| preset.as_ref().map(|x| x.color_op)),
            src_alpha_factor: alpha
                .map(|x| x.src)
                .or_else(|| preset.as_ref().map(|x| x.src_alpha_factor)),
            dst_alpha_factor: alpha
                .map(|x| x.dst)
                .or_else(|| preset.as_ref().map(|x| x.dst_alpha_factor)),
            alpha_op: alpha
                .map(|x| x.op)
                .or_else(|| preset.as_ref().map(|x| x.alpha_op)),
        }
    }
}

#[derive(Debug)]
pub struct PartialBlendingConfigurationCascadeLayer {
    pub src: BlendFactor,
    pub dst: BlendFactor,
    pub op: BlendOp,
}

/// 上から下にオーバーライドできる構成
/// (presetでベーズパラメータ決定->baseのパラメータで上書き->colorのパラメータでcolor関係を上書き->alphaのパラメータでalpha関係を上書き)
///
/// Blendingの有効/無効だけ特殊で、base/color/alphaいずれかの上書きがあればpresetがDisabledでもブレンディング有効になる
#[derive(Debug, Default)]
pub struct PartialBlendingConfiguration {
    pub preset: Option<BlendPreset>,
    pub base: Option<PartialBlendingConfigurationCascadeLayer>,
    pub color: Option<PartialBlendingConfigurationCascadeLayer>,
    pub alpha: Option<PartialBlendingConfigurationCascadeLayer>,
}
impl PartialBlendingConfiguration {
    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        let mut content_flags = PartialBlendingConfigurationContentFlags::empty();
        if self.preset.is_some() {
            content_flags |= PartialBlendingConfigurationContentFlags::HAS_PRESET;
        }
        if self.base.is_some() {
            content_flags |= PartialBlendingConfigurationContentFlags::HAS_BASE;
        }
        if self.color.is_some() {
            content_flags |= PartialBlendingConfigurationContentFlags::HAS_COLOR;
        }
        if self.alpha.is_some() {
            content_flags |= PartialBlendingConfigurationContentFlags::HAS_ALPHA;
        }

        w.write_all(&[content_flags.bits()])?;
        if let Some(p) = self.preset {
            w.write_all(&[p as u8])?;
        }
        if let Some(ref p) = self.base {
            w.write_all(&[p.src as u8, p.dst as u8, p.op as u8])?;
        }
        if let Some(ref p) = self.color {
            w.write_all(&[p.src as u8, p.dst as u8, p.op as u8])?;
        }
        if let Some(ref p) = self.alpha {
            w.write_all(&[p.src as u8, p.dst as u8, p.op as u8])?;
        }

        Ok(())
    }

    pub fn read(r: &mut (impl Read + ?Sized)) -> Result<Self, BlendConfigurationReadError> {
        let mut content_flags = [0u8];
        r.read_exact(&mut content_flags)?;
        let content_flags =
            PartialBlendingConfigurationContentFlags::from_bits_retain(content_flags[0]);

        let preset = if content_flags.contains(PartialBlendingConfigurationContentFlags::HAS_PRESET)
        {
            Some(BlendPreset::read(r)?)
        } else {
            None
        };
        let base = if content_flags.contains(PartialBlendingConfigurationContentFlags::HAS_BASE) {
            let src = BlendFactor::read(r)?;
            let dst = BlendFactor::read(r)?;
            let op = BlendOp::read(r)?;

            Some(PartialBlendingConfigurationCascadeLayer { src, dst, op })
        } else {
            None
        };
        let color = if content_flags.contains(PartialBlendingConfigurationContentFlags::HAS_COLOR) {
            let src = BlendFactor::read(r)?;
            let dst = BlendFactor::read(r)?;
            let op = BlendOp::read(r)?;

            Some(PartialBlendingConfigurationCascadeLayer { src, dst, op })
        } else {
            None
        };
        let alpha = if content_flags.contains(PartialBlendingConfigurationContentFlags::HAS_ALPHA) {
            let src = BlendFactor::read(r)?;
            let dst = BlendFactor::read(r)?;
            let op = BlendOp::read(r)?;

            Some(PartialBlendingConfigurationCascadeLayer { src, dst, op })
        } else {
            None
        };

        Ok(Self {
            preset,
            base,
            color,
            alpha,
        })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum BlendConfigurationReadError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error(transparent)]
    BlendFactor(#[from] BlendFactorReadError),
    #[error(transparent)]
    BlendOp(#[from] BlendOpReadError),
    #[error(transparent)]
    BlendPreset(#[from] BlendPresetReadError),
}

bitflags::bitflags! {
    #[derive(Clone, Copy)]
    struct PartialBlendingConfigurationContentFlags : u8 {
        const HAS_PRESET = 0x01;
        const HAS_BASE = 0x02;
        const HAS_COLOR = 0x04;
        const HAS_ALPHA = 0x08;
    }
}

#[derive(Debug, Default)]
pub struct FragmentShaderBlendingConfiguration {
    pub default: PartialBlendingConfiguration,
    pub per_attachment: BTreeMap<u32, PartialBlendingConfiguration>,
    pub constant: Option<[f32; 4]>,
}
impl FragmentShaderBlendingConfiguration {
    pub fn collect<'a>(attrs: impl Iterator<Item = &'a FragmentShaderAdditionalAttribute>) -> Self {
        let mut sink = Self::default();

        for a in attrs {
            match a {
                FragmentShaderAdditionalAttribute::BlendingConstant(xs) => {
                    if sink.constant.is_some() {
                        tracing::warn!("overwriting blending constant");
                    }

                    sink.constant = Some(*xs);
                }
                FragmentShaderAdditionalAttribute::Blending(attachment_index, blending) => {
                    let sink = match attachment_index {
                        BlendingAttachment::Default => &mut sink.default,
                        &BlendingAttachment::Specified(index) => {
                            sink.per_attachment.entry(index).or_default()
                        }
                    };

                    match *blending {
                        Blending::Preset(preset) => {
                            if sink.preset.is_some() {
                                tracing::warn!("overwriting blending preset");
                            }

                            sink.preset = Some(preset);
                        }
                        Blending::Specified {
                            src_factor,
                            dst_factor,
                            op,
                        } => {
                            if sink.base.is_some() {
                                tracing::warn!("overwriting blending base override");
                            }

                            sink.base = Some(PartialBlendingConfigurationCascadeLayer {
                                src: src_factor,
                                dst: dst_factor,
                                op,
                            });
                        }
                        Blending::Color {
                            src_factor,
                            dst_factor,
                            op,
                        } => {
                            if sink.color.is_some() {
                                tracing::warn!("overwriting blending color override");
                            }

                            sink.color = Some(PartialBlendingConfigurationCascadeLayer {
                                src: src_factor,
                                dst: dst_factor,
                                op,
                            });
                        }
                        Blending::Alpha {
                            src_factor,
                            dst_factor,
                            op,
                        } => {
                            if sink.alpha.is_some() {
                                tracing::warn!("overwriting blending alpha override");
                            }

                            sink.alpha = Some(PartialBlendingConfigurationCascadeLayer {
                                src: src_factor,
                                dst: dst_factor,
                                op,
                            });
                        }
                    }
                }
                _ => {}
            }
        }

        sink
    }

    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        self.default.write(w)?;
        VariableULong(self.per_attachment.len() as _).write(w)?;
        for (&a, x) in self.per_attachment.iter() {
            VariableUInt(a).write(w)?;
            x.write(w)?;
        }

        match self.constant {
            None => {
                w.write_all(&[0x00])?;
            }
            Some(ref xs) => {
                write_vectored_all(
                    w,
                    &mut [
                        mkioslice(&0x01u8),
                        mkioslice(&xs[0]),
                        mkioslice(&xs[1]),
                        mkioslice(&xs[2]),
                        mkioslice(&xs[3]),
                    ],
                )?;
            }
        }

        Ok(())
    }

    pub fn read(
        r: &mut (impl BufRead + ?Sized),
        swap_bytes: bool,
    ) -> Result<Self, BlendConfigurationReadError> {
        let default = PartialBlendingConfiguration::read(r)?;
        let per_attachment_count = VariableULong::read(r)?;
        let mut per_attachment = BTreeMap::new();
        for _ in 0..per_attachment_count.0 {
            let a = VariableUInt::read(r)?;
            let x = PartialBlendingConfiguration::read(r)?;

            per_attachment.insert(a.0, x);
        }

        let mut has_constant = [0u8];
        r.read_exact(&mut has_constant)?;
        let constant = if has_constant[0] == 0x00 {
            None
        } else {
            let mut x = core::mem::MaybeUninit::<f32>::uninit();
            let mut y = core::mem::MaybeUninit::<f32>::uninit();
            let mut z = core::mem::MaybeUninit::<f32>::uninit();
            let mut w = core::mem::MaybeUninit::<f32>::uninit();
            read_vectored_all(
                r,
                &mut [
                    mkioslicem(&mut x),
                    mkioslicem(&mut y),
                    mkioslicem(&mut z),
                    mkioslicem(&mut w),
                ],
            )?;
            if swap_bytes {
                x.write(f32::from_bits(
                    unsafe { x.assume_init() }.to_bits().swap_bytes(),
                ));
                y.write(f32::from_bits(
                    unsafe { y.assume_init() }.to_bits().swap_bytes(),
                ));
                z.write(f32::from_bits(
                    unsafe { z.assume_init() }.to_bits().swap_bytes(),
                ));
                w.write(f32::from_bits(
                    unsafe { w.assume_init() }.to_bits().swap_bytes(),
                ));
            }

            Some([
                unsafe { x.assume_init() },
                unsafe { y.assume_init() },
                unsafe { z.assume_init() },
                unsafe { w.assume_init() },
            ])
        };

        Ok(Self {
            default,
            per_attachment,
            constant,
        })
    }
}
