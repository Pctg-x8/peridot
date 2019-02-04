
use super::*;
use bedrock as br;

#[derive(Clone, Copy)]
pub enum TargetUsage { Texture, DirectRender }
impl TargetUsage {
    fn external_layout(&self) -> br::ImageLayout {
        match self {
            TargetUsage::Texture => br::ImageLayout::ShaderReadOnlyOpt,
            TargetUsage::DirectRender => br::ImageLayout::PresentSrc
        }
    }
}
/// ParameterPack for `ShadingDrivers`
pub struct ShadingDriverParams {
    /// An output texture format. default=`VK_FORMAT_R8G8B8A8_UNORM`
    pub format: br::vk::VkFormat,
    /// How target will be used outside the RenderPass. default=`TargetUsage::Texture`
    pub target_usage: TargetUsage,
    /// Usage of the previous target pixel value.
    /// * `br::LoadOp::Load` when you want to use the value(e.g. frame blending)
    /// * `br::LoadOp::Clear` when the shader requires an initial value(e.g. rendering some transparent objects)
    /// * Otherwise(Default), `br::LoadOp::DontCare`
    pub load_op: br::LoadOp,
    /// Whether the Shading Driver will be used for initialize. default=`false`
    /// 
    /// ## Note
    /// 
    /// Setting this to `true` requires that the image layout of the target texture
    /// must be `br::ImageLayout::Preinitialized`
    pub initializer: bool
}
impl Default for ShadingDriverParams {
    fn default() -> Self {
        ShadingDriverParams {
            format: br::vk::VK_FORMAT_R8G8B8A8_UNORM, target_usage: TargetUsage::Texture,
            load_op: br::LoadOp::DontCare, initializer: false
        }
    }
}
/// Resource Independent Shading Objects
pub struct ShadingDrivers {
    renderpass: br::RenderPass, 
}
impl ShadingDrivers {
    pub fn new(d: &br::Device, vsh: br::PipelineShader, fsh: br::PipelineShader, params: ShadingDriverParams)
            -> br::Result<Self> {
        let external_layout = params.target_usage.external_layout();
        let in_layout = if params.initializer { br::ImageLayout::Preinitialized } else { external_layout };
        let att_desc = br::AttachmentDescription::new(params.format, in_layout, external_layout)
            .load_op(params.load_op).store_op(br::StoreOp::Store);
        let renderpass = br::RenderPassBuilder::new().add_attachment(att_desc)
            .add_subpass(br::SubpassDescription::new().add_color_output(0, br::ImageLayout::ColorAttachmentOpt, None))
            .add_dependency(SubpassDependencyTemplates::to_color_attachment_in(None, 0, true))
            .create(d)?;
        
        return Ok(ShadingDrivers {
            renderpass
        })
    }
}
