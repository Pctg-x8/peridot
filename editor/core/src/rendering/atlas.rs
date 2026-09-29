use bedrock::{self as br, ImageChild, MemoryBound, VkHandle};

use crate::graphics::Graphics;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AtlasRect {
    pub left: u32,
    pub top: u32,
    pub right: u32,
    pub bottom: u32,
}
impl AtlasRect {
    pub const EMPTY: Self = Self {
        left: 0,
        top: 0,
        right: 0,
        bottom: 0,
    };

    pub const fn width(&self) -> u32 {
        self.right.abs_diff(self.left)
    }

    pub const fn height(&self) -> u32 {
        self.bottom.abs_diff(self.top)
    }

    pub const fn lt_offset(&self) -> br::Offset2D {
        br::Offset2D {
            x: self.left as _,
            y: self.top as _,
        }
    }

    pub const fn extent(&self) -> br::Extent2D {
        br::Extent2D {
            width: self.width(),
            height: self.height(),
        }
    }

    pub const fn vk_rect(&self) -> br::Rect2D {
        self.extent().into_rect(self.lt_offset())
    }
}

pub struct TextureAtlas {
    res: br::vk::VkImage,
    mem: br::vk::VkDeviceMemory,
    view: br::vk::VkImageView,
    max: br::Extent2D,
    internal: DynamicAtlasManager,
}
impl TextureAtlas {
    pub const MULTISAMPLE_LEVEL: u32 = 4;
    const SPACING: u32 = 1;

    pub unsafe fn drop(&mut self, gfx: &Graphics) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(
                gfx.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.view),
                None,
            );
            br::vkfn_wrapper::destroy_image(
                gfx.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.res),
                None,
            );
            br::vkfn_wrapper::free_memory(
                gfx.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.mem),
                None,
            );
        }
    }

    pub fn new(gfx: &Graphics) -> Self {
        let size = br::Extent2D::spread1(4096);

        let mut res = br::ImageObject::new(
            gfx,
            &br::ImageCreateInfo::new(size, br::vk::VK_FORMAT_R8_UNORM).set_usage(
                br::ImageUsageFlags::SAMPLED
                    | br::ImageUsageFlags::COLOR_ATTACHMENT
                    | br::ImageUsageFlags::TRANSFER_DEST,
            ),
        )
        .expect("res create");
        let memory_requirements = res.requirements();
        let mem = br::DeviceMemoryObject::new(
            gfx,
            &br::MemoryAllocateInfo::new(
                memory_requirements.size,
                gfx.find_device_local_memory_index(memory_requirements.memoryTypeBits)
                    .expect("no suitable memory"),
            ),
        )
        .expect("res malloc");
        res.bind(&mem, 0).expect("res mem bind");
        let view = br::ImageViewBuilder::new(
            res,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .expect("res view create");

        gfx.dbg_set_name(view.image(), c"Glyph Atlas");
        gfx.dbg_set_name(&view, c"Glyph Atlas [View]");
        gfx.dbg_set_name(&mem, c"Glyph Atlas [Backing Memory]");

        let mut internal = DynamicAtlasManager::new();
        internal.free(AtlasRect {
            left: 0,
            top: 0,
            right: size.width,
            bottom: size.height,
        });

        let (view, res) = view.unmanage();
        let (res, _, _, _, _) = res.unmanage();
        let (mem, _) = mem.unmanage();
        Self {
            res,
            mem,
            view,
            internal,
            max: size,
        }
    }

    pub fn clear(&mut self) {
        self.internal.clear();
        // register entire region as free
        self.internal.free(AtlasRect {
            left: 0,
            top: 0,
            right: self.max.width,
            bottom: self.max.height,
        });
        // TODO: clear atlas content?
    }

    pub fn acquire(&mut self, width: u32, height: u32) -> AtlasRect {
        self.internal
            .alloc(width + Self::SPACING, height + Self::SPACING)
            .expect("no space left")
    }

    #[inline(always)]
    pub const fn size(&self) -> &br::Extent2D {
        &self.max
    }

    #[inline(always)]
    pub const fn image<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkImage> {
        unsafe { br::VkHandleRef::dangling(self.res) }
    }

    #[inline(always)]
    pub const fn image_range_entire(&self) -> br::ImageSubresourceRange {
        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1)
    }

    #[inline(always)]
    pub const fn view<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkImageView> {
        unsafe { br::VkHandleRef::dangling(self.view) }
    }

    #[inline(always)]
    pub const fn as_image_view<'a>(&'a self) -> &'a impl br::ImageView {
        unsafe { core::mem::transmute::<_, &TextureAtlasAsImageView>(self) }
    }
}

#[repr(transparent)]
pub struct TextureAtlasAsImageView(TextureAtlas);
impl br::VkHandle for TextureAtlasAsImageView {
    type Handle = br::vk::VkImageView;

    #[inline(always)]
    fn native_ptr(&self) -> Self::Handle {
        self.0.view
    }
}
impl br::ImageView for TextureAtlasAsImageView {}

pub struct ColorTextureAtlas {
    res: br::vk::VkImage,
    mem: br::vk::VkDeviceMemory,
    view: br::vk::VkImageView,
    max: br::Extent2D,
    internal: DynamicAtlasManager,
}
impl ColorTextureAtlas {
    const INIT_SIZE: u32 = 512;
    const SPACING: u32 = 1;
    const FORMAT: br::Format = br::vk::VK_FORMAT_R8G8B8A8_UNORM;

    pub unsafe fn drop(&mut self, gfx: &Graphics) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(
                gfx.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.view),
                None,
            );
            br::vkfn_wrapper::destroy_image(
                gfx.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.res),
                None,
            );
            br::vkfn_wrapper::free_memory(
                gfx.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.mem),
                None,
            );
        }
    }

    pub fn new(gfx: &Graphics) -> Self {
        let size = br::Extent2D::spread1(Self::INIT_SIZE);

        let mut res = br::ImageObject::new(
            gfx,
            &br::ImageCreateInfo::new(size, Self::FORMAT)
                .set_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::COLOR_ATTACHMENT),
        )
        .expect("res create");
        let memory_requirements = res.requirements();
        let mem = br::DeviceMemoryObject::new(
            gfx,
            &br::MemoryAllocateInfo::new(
                memory_requirements.size,
                gfx.find_device_local_memory_index(memory_requirements.memoryTypeBits)
                    .expect("no suitable memory"),
            ),
        )
        .expect("res malloc");
        res.bind(&mem, 0).expect("res mem bind");
        let view = br::ImageViewBuilder::new(
            res,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .expect("res view create");

        gfx.dbg_set_name(view.image(), c"Color Texture Atlas");
        gfx.dbg_set_name(&view, c"Color Texture Atlas [View]");
        gfx.dbg_set_name(&mem, c"Color Texture Atlas [Backing Memory]");

        let mut internal = DynamicAtlasManager::new();
        internal.free(AtlasRect {
            left: 0,
            top: 0,
            right: size.width,
            bottom: size.height,
        });

        let (view, res) = view.unmanage();
        let (res, _, _, _, _) = res.unmanage();
        let (mem, _) = mem.unmanage();
        Self {
            res,
            mem,
            view,
            internal,
            max: size,
        }
    }

    pub fn clear(&mut self) {
        self.internal.clear();
        // register entire region as free
        self.internal.free(AtlasRect {
            left: 0,
            top: 0,
            right: self.max.width,
            bottom: self.max.height,
        });
        // TODO: clear atlas content?
    }

    pub fn acquire(&mut self, width: u32, height: u32) -> AtlasRect {
        self.internal
            .alloc(width + Self::SPACING, height + Self::SPACING)
            .expect("no space left")
    }

    #[inline(always)]
    pub const fn format(&self) -> br::Format {
        Self::FORMAT
    }

    #[inline(always)]
    pub const fn size(&self) -> &br::Extent2D {
        &self.max
    }

    #[inline(always)]
    pub const fn image<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkImage> {
        unsafe { br::VkHandleRef::dangling(self.res) }
    }

    #[inline(always)]
    pub const fn image_range_entire(&self) -> br::ImageSubresourceRange {
        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1)
    }

    #[inline(always)]
    pub const fn view<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkImageView> {
        unsafe { br::VkHandleRef::dangling(self.view) }
    }

    #[inline(always)]
    pub const fn as_image_view<'a>(&'a self) -> &'a impl br::ImageView {
        unsafe { core::mem::transmute::<_, &ColorTextureAtlasAsImageView>(self) }
    }
}

#[repr(transparent)]
pub struct ColorTextureAtlasAsImageView(ColorTextureAtlas);
impl br::VkHandle for ColorTextureAtlasAsImageView {
    type Handle = br::vk::VkImageView;

    #[inline(always)]
    fn native_ptr(&self) -> Self::Handle {
        self.0.view
    }
}
impl br::ImageView for ColorTextureAtlasAsImageView {}

pub struct DynamicAtlasManager {
    /// width -> (height -> [rect])
    available_regions: Vec<(u32, Vec<(u32, AtlasRect)>)>,
}
impl DynamicAtlasManager {
    pub fn new() -> Self {
        Self {
            available_regions: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.available_regions.clear();
    }

    #[tracing::instrument(name = "DynamicAtlasManager::alloc", skip(self))]
    pub fn alloc(&mut self, width: u32, height: u32) -> Option<AtlasRect> {
        // tracing::debug!(available_regions = ?self.available_regions, "alloc state");
        // find best match
        let mut match_index_by_width = match self
            .available_regions
            .binary_search_by_key(&width, |&(w, _)| w)
        {
            Ok(exact) => exact,
            Err(large_enough) if large_enough >= self.available_regions.len() => {
                tracing::error!(
                    phase = "matching width",
                    available = ?self.available_regions.iter().map(|&(x, _)| x).collect::<Vec<_>>(),
                    "not found"
                );
                return None;
            }
            Err(large_enough) => large_enough,
        };
        let match_index_by_height = 'height_match_loop: loop {
            match self.available_regions[match_index_by_width]
                .1
                .binary_search_by_key(&height, |&(h, _)| h)
            {
                Ok(exact) => break 'height_match_loop exact,
                Err(large_enough)
                    if large_enough >= self.available_regions[match_index_by_width].1.len() =>
                {
                    tracing::trace!(
                        phase = "matching height",
                        available = ?self.available_regions[match_index_by_width].1.iter().map(|&(x, _)| x).collect::<Vec<_>>(),
                        "not found, trying more wider"
                    );
                    match_index_by_width += 1;
                    if match_index_by_width >= self.available_regions.len() {
                        tracing::error!("no available region found");
                        return None;
                    }
                }
                Err(large_enough) => break 'height_match_loop large_enough,
            }
        };

        let mut rect = self.available_regions[match_index_by_width].1[match_index_by_height].1;
        assert!(rect.width() >= width);
        assert!(rect.height() >= height);
        rect.right = rect.left + width;
        rect.bottom = rect.top + height;

        // for all regions, overlapped rects needs to be splitted
        'split_loop: loop {
            let modification_target = 'find_overlap: {
                for (nw, (_, xs)) in self.available_regions.iter().enumerate() {
                    for (nh, (_, r)) in xs.iter().enumerate() {
                        if r == &rect {
                            // exact match rect
                            break 'find_overlap (nw, nh);
                        }

                        let h_contact = r.left < rect.right && rect.left < r.right;
                        let v_contact = r.top < rect.bottom && rect.top < r.bottom;

                        if h_contact && v_contact {
                            // overlap
                            // tracing::trace!(?r, ?rect, "overlap found");
                            break 'find_overlap (nw, nh);
                        }
                    }
                }

                // no overlap found
                break 'split_loop;
            };

            let (overlap_rect, _) =
                self.unregister_rect_by_index(modification_target.0, modification_target.1);
            if overlap_rect == rect {
                // exact match: take all
                continue;
            }

            let left_gap = if overlap_rect.left < rect.left {
                Some(rect.left - overlap_rect.left)
            } else {
                None
            };
            let right_gap = if overlap_rect.right > rect.right {
                Some(overlap_rect.right - rect.right)
            } else {
                None
            };
            let top_gap = if overlap_rect.top < rect.top {
                Some(rect.top - overlap_rect.top)
            } else {
                None
            };
            let bottom_gap = if overlap_rect.bottom > rect.bottom {
                Some(overlap_rect.bottom - rect.bottom)
            } else {
                None
            };

            // tracing::trace!(left = ?left_gap, right = ?right_gap, top = ?top_gap, bottom = ?bottom_gap, "gaps");

            if let Some(w) = left_gap {
                self.register_rect(AtlasRect {
                    left: overlap_rect.left,
                    right: overlap_rect.left + w,
                    top: overlap_rect.top,
                    bottom: overlap_rect.bottom,
                });
            }
            if let Some(w) = right_gap {
                self.register_rect(AtlasRect {
                    left: overlap_rect.right - w,
                    right: overlap_rect.right,
                    top: overlap_rect.top,
                    bottom: overlap_rect.bottom,
                });
            }
            if let Some(h) = top_gap {
                self.register_rect(AtlasRect {
                    left: overlap_rect.left,
                    right: overlap_rect.right,
                    top: overlap_rect.top,
                    bottom: overlap_rect.top + h,
                });
            }
            if let Some(h) = bottom_gap {
                self.register_rect(AtlasRect {
                    left: overlap_rect.left,
                    right: overlap_rect.right,
                    top: overlap_rect.bottom - h,
                    bottom: overlap_rect.bottom,
                });
            }
        }

        Some(rect)
    }

    pub fn free(&mut self, rect: AtlasRect) {
        let inserted_indices = self.register_rect(rect);
        self.merge_recursively(inserted_indices);
    }

    #[inline(always)]
    fn rect(&self, nw: usize, nh: usize) -> &AtlasRect {
        &self.available_regions[nw].1[nh].1
    }

    fn merge_recursively(&mut self, pointing_rect_indices: (usize, usize)) {
        pub enum MergeOperation {
            ToLeft(usize, usize),
            ToRight(usize, usize),
            ToTop(usize, usize),
            ToBottom(usize, usize),
        }

        let merge_op = 'find_contact: {
            let rect =
                &self.available_regions[pointing_rect_indices.0].1[pointing_rect_indices.1].1;
            for (nw, (_, xs)) in self.available_regions.iter().enumerate() {
                for (nh, (_, r)) in xs.iter().enumerate() {
                    if r.bottom < rect.top
                        || r.right < rect.left
                        || r.top > rect.bottom
                        || r.left > rect.right
                    {
                        // never contacts
                        continue;
                    }

                    if rect.left == r.right && rect.top >= r.top && rect.bottom <= r.bottom {
                        break 'find_contact MergeOperation::ToLeft(nw, nh);
                    }
                    if rect.right == r.left && rect.top >= r.top && rect.bottom <= r.bottom {
                        break 'find_contact MergeOperation::ToRight(nw, nh);
                    }
                    if rect.top == r.bottom && rect.left >= r.left && rect.right <= r.right {
                        break 'find_contact MergeOperation::ToTop(nw, nh);
                    }
                    if rect.bottom == r.top && rect.left >= r.left && rect.right <= r.right {
                        break 'find_contact MergeOperation::ToBottom(nw, nh);
                    }
                }
            }

            // no contact: merging does nothing
            return;
        };

        match merge_op {
            MergeOperation::ToLeft(mut target_nw, mut target_nh) => {
                let target_rect = *self.rect(target_nw, target_nh);
                let (rect, shift_nw) =
                    self.unregister_rect_by_index(pointing_rect_indices.0, pointing_rect_indices.1);

                // adjust target indices
                if target_nw == pointing_rect_indices.0 && target_nh > pointing_rect_indices.1 {
                    target_nh -= 1;
                }
                if shift_nw && target_nw > pointing_rect_indices.0 {
                    target_nw -= 1;
                }

                let merged_rect = AtlasRect {
                    left: target_rect.left,
                    ..rect
                };

                if rect.top == target_rect.top && rect.bottom == target_rect.bottom {
                    // completely overlapped
                    self.unregister_rect_by_index(target_nw, target_nh);
                }
                let inserted_index = self.register_rect(merged_rect);
                self.merge_recursively(inserted_index);
            }
            MergeOperation::ToRight(mut target_nw, mut target_nh) => {
                let target_rect = *self.rect(target_nw, target_nh);
                let (rect, shift_nw) =
                    self.unregister_rect_by_index(pointing_rect_indices.0, pointing_rect_indices.1);

                // adjust target indices
                if target_nw == pointing_rect_indices.0 && target_nh > pointing_rect_indices.1 {
                    target_nh -= 1;
                }
                if shift_nw && target_nw > pointing_rect_indices.0 {
                    target_nw -= 1;
                }

                let merged_rect = AtlasRect {
                    right: target_rect.right,
                    ..rect
                };

                if rect.top == target_rect.top && rect.bottom == target_rect.bottom {
                    // completely overlapped
                    self.unregister_rect_by_index(target_nw, target_nh);
                }
                let inserted_index = self.register_rect(merged_rect);
                self.merge_recursively(inserted_index);
            }
            MergeOperation::ToTop(mut target_nw, mut target_nh) => {
                let target_rect = *self.rect(target_nw, target_nh);
                let (rect, shift_nw) =
                    self.unregister_rect_by_index(pointing_rect_indices.0, pointing_rect_indices.1);

                // adjust target indices
                if target_nw == pointing_rect_indices.0 && target_nh > pointing_rect_indices.1 {
                    target_nh -= 1;
                }
                if shift_nw && target_nw > pointing_rect_indices.0 {
                    target_nw -= 1;
                }

                let merged_rect = AtlasRect {
                    top: target_rect.top,
                    ..rect
                };

                if rect.left == target_rect.left && rect.right == target_rect.right {
                    // completely overlapped
                    self.unregister_rect_by_index(target_nw, target_nh);
                }
                let inserted_index = self.register_rect(merged_rect);
                self.merge_recursively(inserted_index);
            }
            MergeOperation::ToBottom(mut target_nw, mut target_nh) => {
                let target_rect = *self.rect(target_nw, target_nh);
                let (rect, shift_nw) =
                    self.unregister_rect_by_index(pointing_rect_indices.0, pointing_rect_indices.1);

                // adjust target indices
                if target_nw == pointing_rect_indices.0 && target_nh > pointing_rect_indices.1 {
                    target_nh -= 1;
                }
                if shift_nw && target_nw > pointing_rect_indices.0 {
                    target_nw -= 1;
                }

                let merged_rect = AtlasRect {
                    bottom: target_rect.bottom,
                    ..rect
                };

                if rect.left == target_rect.left && rect.right == target_rect.right {
                    // completely overlapped
                    self.unregister_rect_by_index(target_nw, target_nh);
                }
                let inserted_index = self.register_rect(merged_rect);
                self.merge_recursively(inserted_index);
            }
        }
    }

    fn unregister_rect_by_index(&mut self, nw: usize, nh: usize) -> (AtlasRect, bool) {
        let width_line = &mut self.available_regions[nw].1;
        let r = width_line.remove(nh).1;
        let width_line_removed = width_line.is_empty();
        if width_line_removed {
            self.available_regions.remove(nw);
        }

        (r, width_line_removed)
    }

    /// simply insert a rect into correct position(no merging performed)
    fn register_rect(&mut self, rect: AtlasRect) -> (usize, usize) {
        // tracing::trace!(?rect, "insrect");
        match self
            .available_regions
            .binary_search_by_key(&rect.width(), |&(w, _)| w)
        {
            Ok(width_index) => {
                let width_line = &mut self.available_regions[width_index].1;

                let rh = rect.height();
                for (n, p) in width_line.iter().enumerate() {
                    if p.1 == rect {
                        // already registered
                        return (width_index, n);
                    }

                    if p.0 > rh {
                        // insert here
                        width_line.insert(n, (rh, rect));
                        return (width_index, n);
                    }
                }

                // push(tallest block)
                width_line.push((rh, rect));
                return (width_index, width_line.len() - 1);
            }
            Err(insert_point) => {
                // unique for this width
                self.available_regions
                    .insert(insert_point, (rect.width(), vec![(rect.height(), rect)]));

                (insert_point, 0)
            }
        }
    }
}
