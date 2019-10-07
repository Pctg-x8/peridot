//! Vulkan Common Settings

use bedrock as br;
use bedrock::vk::*;

pub const fn common_rasterization_state() -> VkPipelineRasterizationStateCreateInfo
{
    VkPipelineRasterizationStateCreateInfo
    {
        sType: VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
        pNext: std::ptr::null(),
        flags: 0,
        polygonMode: VK_POLYGON_MODE_FILL,
        cullMode: VK_CULL_MODE_FRONT_BIT,
        frontFace: VK_FRONT_FACE_COUNTER_CLOCKWISE,
        lineWidth: 1.0,
        depthBiasEnable: VK_FALSE,
        depthBiasConstantFactor: 0.0,
        depthBiasSlopeFactor: 0.0,
        depthClampEnable: VK_FALSE,
        depthBiasClamp: 0.0,
        rasterizerDiscardEnable: VK_FALSE
    }
}

pub const fn common_multisample_state() -> VkPipelineMultisampleStateCreateInfo
{
    VkPipelineMultisampleStateCreateInfo
    {
        sType: VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
        pNext: std::ptr::null(),
        flags: 0,
        alphaToCoverageEnable: VK_FALSE,
        alphaToOneEnable: VK_FALSE,
        rasterizationSamples: VK_SAMPLE_COUNT_1_BIT,
        sampleShadingEnable: VK_FALSE,
        minSampleShading: 1.0,
        pSampleMask: std::ptr::null()
    }
}

pub enum SubpassDependencyTemplates {}
impl SubpassDependencyTemplates
{
    pub fn to_color_attachment_in(from_subpass: Option<u32>, occurence_subpass: u32, by_region: bool)
        -> VkSubpassDependency
    {
        VkSubpassDependency
        {
            dstSubpass: occurence_subpass, srcSubpass: from_subpass.unwrap_or(br::vk::VK_SUBPASS_EXTERNAL),
            dstStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.0,
            dstAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write,
            dependencyFlags: if by_region { br::vk::VK_DEPENDENCY_BY_REGION_BIT } else { 0 },
            srcStageMask: br::PipelineStageFlags::TOP_OF_PIPE.0,
            .. Default::default()
        }
    }
}

pub const fn presentation_attachment_description(format: VkFormat, require_clear: bool) -> br::AttachmentDescription
{
    br::AttachmentDescription::new(format, br::ImageLayout::PresentSrc, br::ImageLayout::PresentSrc)
        .store_op(br::StoreOp::Store)
        .load_op([br::LoadOp::DontCare, br::LoadOp::Clear][require_clear as usize])
}

pub enum RenderPassTemplates {}
impl RenderPassTemplates
{
    pub fn single_render(format: VkFormat) -> br::RenderPassBuilder
    {
        let mut b = br::RenderPassBuilder::new();
        let adesc = presentation_attachment_description(format, true);
        b.add_attachment(adesc);
        b.add_subpass(br::SubpassDescription::new().add_color_output(0, br::ImageLayout::ColorAttachmentOpt, None));
        b.add_dependency(SubpassDependencyTemplates::to_color_attachment_in(None, 0, true));

        return b;
    }
}
