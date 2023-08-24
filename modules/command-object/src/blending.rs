use bedrock as br;

pub struct Blending {
    pub src_factor: br::BlendFactor,
    pub dst_factor: br::BlendFactor,
    pub op: br::BlendOp,
}
impl Blending {
    pub const fn new(src: br::BlendFactor, op: br::BlendOp, dst: br::BlendFactor) -> Self {
        Self {
            src_factor: src,
            dst_factor: dst,
            op,
        }
    }

    /// src * factor + dest * 0
    pub const fn source_only(factor: br::BlendFactor) -> Self {
        Self::new(factor, br::BlendOp::Add, br::BlendFactor::Zero)
    }

    /// no factors applied: src * 1 op dest * 1
    pub const fn pure_color_op(op: br::BlendOp) -> Self {
        Self::new(br::BlendFactor::One, op, br::BlendFactor::One)
    }

    /// no factors applied: src * 1 op dest * 1
    pub const fn pure_alpha_op(op: br::BlendOp) -> Self {
        Self::new(br::BlendFactor::One, op, br::BlendFactor::One)
    }

    pub const MAX_COLOR: Self = Self::pure_color_op(br::BlendOp::Max);
    pub const MAX_ALPHA: Self = Self::pure_alpha_op(br::BlendOp::Max);
    pub const MIN_COLOR: Self = Self::pure_color_op(br::BlendOp::Min);
    pub const MIN_ALPHA: Self = Self::pure_color_op(br::BlendOp::Min);
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

    pub const fn into_vk(self) -> br::vk::VkPipelineColorBlendAttachmentState {
        match self {
            Self::Disabled => br::vk::VkPipelineColorBlendAttachmentState {
                blendEnable: false as _,
                srcColorBlendFactor: br::BlendFactor::One as _,
                dstColorBlendFactor: br::BlendFactor::One as _,
                colorBlendOp: br::BlendOp::Add as _,
                srcAlphaBlendFactor: br::BlendFactor::One as _,
                dstAlphaBlendFactor: br::BlendFactor::One as _,
                alphaBlendOp: br::BlendOp::Add as _,
                colorWriteMask: br::vk::VK_COLOR_COMPONENT_A_BIT
                    | br::vk::VK_COLOR_COMPONENT_B_BIT
                    | br::vk::VK_COLOR_COMPONENT_G_BIT
                    | br::vk::VK_COLOR_COMPONENT_R_BIT,
            },
            Self::Enabled {
                color,
                alpha,
                color_write_mask,
            } => br::vk::VkPipelineColorBlendAttachmentState {
                blendEnable: true as _,
                srcColorBlendFactor: color.src_factor as _,
                dstColorBlendFactor: color.dst_factor as _,
                colorBlendOp: color.op as _,
                srcAlphaBlendFactor: alpha.src_factor as _,
                dstAlphaBlendFactor: alpha.dst_factor as _,
                alphaBlendOp: alpha.op as _,
                colorWriteMask: color_write_mask,
            },
        }
    }
}
