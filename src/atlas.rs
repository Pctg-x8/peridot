//! Dynamic Atlas Management

use bedrock as br;
use peridot_math::Vector2;

#[derive(Clone, Debug)]
pub struct TextureSlice {
    pub offset: Vector2<u32>,
    pub size: Vector2<u32>
}

pub struct DynamicTextureAtlas<B: BinningAlgorithm> {
    pub resource_index: usize,
    pub size: Vector2<u32>,
    pub binning: B
}
impl<B: BinningAlgorithm> DynamicTextureAtlas<B> {
    pub fn new(
        g: &crate::Graphics,
        size: Vector2<u32>,
        format: br::vk::VkFormat,
        storage_alloc: &mut crate::BulkedResourceStorageAllocator
    ) -> br::Result<Self> {
        let usage = br::ImageUsage::SAMPLED.transfer_dest();
        let image = br::ImageDesc::new(&size, format, usage, br::ImageLayout::Undefined).create(g)?;
        
        Ok(DynamicTextureAtlas {
            resource_index: storage_alloc.add_image(image),
            binning: B::with_texture_size(size.clone()),
            size
        })
    }
    pub fn resource_entity<'s>(&self, storage: &'s crate::ResourceStorage) -> &'s crate::Image {
        storage.get_image(self.resource_index).expect("invalid storage")
    }

    pub fn request_rect(&mut self, size: &Vector2<u32>) -> Option<TextureSlice> {
        self.binning.request_rect(size)
    }
    pub fn to_uv(&self, p: &Vector2<u32>) -> Vector2<f32> {
        Vector2(p.0 as f32 / self.size.0 as f32, p.1 as f32 / self.size.1 as f32)
    }
}

/// Processes binning of TextureSlices in Dynamic Texture Atlas
pub trait BinningAlgorithm {
    fn with_texture_size(tex_size: Vector2<u32>) -> Self;
    fn request_rect(&mut self, size: &Vector2<u32>) -> Option<TextureSlice>;
}
/// Most simple binning algorithm
pub struct BookshelfBinning {
    limit: Vector2<u32>,
    current_base_y: u32,
    current_max_h: u32,
    current_used_x: u32
}
impl BinningAlgorithm for BookshelfBinning {
    fn with_texture_size(tex_size: Vector2<u32>) -> Self {
        BookshelfBinning {
            current_base_y: 0,
            current_max_h: 0,
            current_used_x: 0,
            limit: tex_size
        }
    }
    fn request_rect(&mut self, size: &Vector2<u32>) -> Option<TextureSlice> {
        if self.limit.0 < size.0 || self.limit.1 < size.1 {
            // too large
            return None;
        }

        if self.current_used_x + size.0 > self.limit.0 {
            // next line
            self.current_base_y += self.current_max_h;
            self.current_max_h = 0;
            self.current_used_x = 0;
        }
        
        if self.current_base_y + size.1 > self.limit.1 {
            // no suitable region
            return None;
        }

        let offs = Vector2(self.current_used_x, self.current_base_y);
        self.current_used_x += size.0;
        self.current_max_h = self.current_max_h.max(size.1);
        Some(TextureSlice { offset: offs, size: size.clone() })
    }
}
