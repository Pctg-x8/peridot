use bedrock as br;

pub struct Blending {
    pub src_factor: br::vk::VkBlendFactor,
    pub dst_factor: br::vk::VkBlendFactor,
    pub op: br::vk::VkBlendOp,
}
impl Blending {
    pub const fn new(
        src: br::vk::VkBlendFactor,
        op: br::vk::VkBlendOp,
        dst: br::vk::VkBlendFactor,
    ) -> Self {
        Self {
            src_factor: src,
            dst_factor: dst,
            op,
        }
    }

    /// src * factor + dest * 0
    pub const fn source_only(factor: br::vk::VkBlendFactor) -> Self {
        Self::new(
            factor,
            br::vk::VK_BLEND_OP_ADD,
            br::vk::VK_BLEND_FACTOR_ZERO,
        )
    }

    /// src * 0 + dest * factor
    pub const fn dest_only(factor: br::vk::VkBlendFactor) -> Self {
        Self::new(
            br::vk::VK_BLEND_FACTOR_ZERO,
            br::vk::VK_BLEND_OP_ADD,
            factor,
        )
    }

    /// no factors applied: src * 1 op dest * 1
    pub const fn pure_color_op(op: br::vk::VkBlendOp) -> Self {
        Self::new(br::vk::VK_BLEND_FACTOR_ONE, op, br::vk::VK_BLEND_FACTOR_ONE)
    }

    /// no factors applied: src * 1 op dest * 1
    pub const fn pure_alpha_op(op: br::vk::VkBlendOp) -> Self {
        Self::new(br::vk::VK_BLEND_FACTOR_ONE, op, br::vk::VK_BLEND_FACTOR_ONE)
    }

    pub const STRAIGHT_SOURCE: Self = Self::source_only(br::vk::VK_BLEND_FACTOR_ONE);
    pub const STRAIGHT_DEST: Self = Self::dest_only(br::vk::VK_BLEND_FACTOR_ONE);
    pub const MAX_COLOR: Self = Self::pure_color_op(br::vk::VK_BLEND_OP_MAX);
    pub const MAX_ALPHA: Self = Self::pure_alpha_op(br::vk::VK_BLEND_OP_MAX);
    pub const MIN_COLOR: Self = Self::pure_color_op(br::vk::VK_BLEND_OP_MIN);
    pub const MIN_ALPHA: Self = Self::pure_color_op(br::vk::VK_BLEND_OP_MIN);
    pub const FOR_PREMULTIPLIED_ALPHA: Self = Self::new(
        br::vk::VK_BLEND_FACTOR_ONE,
        br::vk::VK_BLEND_OP_ADD,
        br::vk::VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
    );
}

pub enum ColorAttachmentBlending {
    Disabled,
    Enabled {
        color: Blending,
        alpha: Blending,
        color_write_mask: u32,
    },
}
impl ColorAttachmentBlending {
    pub const fn new(color: Blending, alpha: Blending) -> Self {
        Self::Enabled {
            color,
            alpha,
            color_write_mask: br::vk::VK_COLOR_COMPONENT_A_BIT
                | br::vk::VK_COLOR_COMPONENT_B_BIT
                | br::vk::VK_COLOR_COMPONENT_G_BIT
                | br::vk::VK_COLOR_COMPONENT_R_BIT,
        }
    }

    pub const MAX: Self = Self::new(Blending::MAX_COLOR, Blending::MAX_ALPHA);
    pub const MIN: Self = Self::new(Blending::MIN_COLOR, Blending::MIN_ALPHA);
    pub const PREMULTIPLIED_ALPHA: Self = Self::new(
        Blending::FOR_PREMULTIPLIED_ALPHA,
        Blending::FOR_PREMULTIPLIED_ALPHA,
    );

    pub const fn with_color_write_mask(self, mask: u32) -> Self {
        match self {
            Self::Enabled { color, alpha, .. } => Self::Enabled {
                color,
                alpha,
                color_write_mask: mask,
            },
            s => s,
        }
    }

    pub const fn into_vk(self) -> br::PipelineColorBlendAttachmentState {
        match self {
            Self::Disabled => {
                br::PipelineColorBlendAttachmentState(br::vk::VkPipelineColorBlendAttachmentState {
                    blendEnable: false as _,
                    srcColorBlendFactor: br::vk::VK_BLEND_FACTOR_ONE as _,
                    dstColorBlendFactor: br::vk::VK_BLEND_FACTOR_ONE as _,
                    colorBlendOp: br::vk::VK_BLEND_OP_ADD as _,
                    srcAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_ONE as _,
                    dstAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_ONE as _,
                    alphaBlendOp: br::vk::VK_BLEND_OP_ADD as _,
                    colorWriteMask: br::vk::VK_COLOR_COMPONENT_A_BIT
                        | br::vk::VK_COLOR_COMPONENT_B_BIT
                        | br::vk::VK_COLOR_COMPONENT_G_BIT
                        | br::vk::VK_COLOR_COMPONENT_R_BIT,
                })
            }
            Self::Enabled {
                color,
                alpha,
                color_write_mask,
            } => {
                br::PipelineColorBlendAttachmentState(br::vk::VkPipelineColorBlendAttachmentState {
                    blendEnable: true as _,
                    srcColorBlendFactor: color.src_factor as _,
                    dstColorBlendFactor: color.dst_factor as _,
                    colorBlendOp: color.op as _,
                    srcAlphaBlendFactor: alpha.src_factor as _,
                    dstAlphaBlendFactor: alpha.dst_factor as _,
                    alphaBlendOp: alpha.op as _,
                    colorWriteMask: color_write_mask,
                })
            }
        }
    }
}
