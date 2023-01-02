use bedrock as br;
use br::MemoryBound;

pub struct Game<NL> {
    ui: UIContext,
    ui_main: UIPlane,
    res_storage: peridot::ResourceStorage,
    marker: std::marker::PhantomData<*const NL>,
}
impl<NL> Game<NL> {
    pub const NAME: &'static str = "Multiplane UI Demo";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<NL> peridot::FeatureRequests for Game<NL> {}
impl<NL: peridot::NativeLinker> peridot::EngineEvents<NL> for Game<NL> {
    fn init(e: &mut peridot::Engine<NL>) -> Self {
        let mut res_storage_alloc = peridot::BulkedResourceStorageAllocator::new();
        let mut init_cp_batch = peridot::TransferBatch::new();
        let mut ui = UIContext::new(
            e.graphics(),
            &mut res_storage_alloc,
            2048,
            &mut init_cp_batch,
        );
        let uifont = ui
            .load_font("sans-serif", &peridot_vg::FontProperties::default())
            .expect("Failed to load UI Font");
        let mut ui_main = UIPlane::new();
        ui_main.set_root(Box::new(StaticLabel::new(uifont, "test", 12.0)));
        ui_main.prepare_render(&mut ui, e.graphics());
        ui_main.layout_all();

        Game {
            res_storage: res_storage_alloc
                .alloc(e.graphics())
                .expect("Failed to allocate resource storages"),
            ui,
            ui_main,
            marker: std::marker::PhantomData,
        }
    }
}

use std::collections::HashMap;

pub struct DynamicUpdateBufferWriteRange<'a, T> {
    mem: &'a br::DeviceMemory,
    mapped_owner: br::MappedMemoryRange<'a>,
    _back_data: std::marker::PhantomData<*mut T>,
}
impl<T> Drop for DynamicUpdateBufferWriteRange<'_, T> {
    fn drop(&mut self) {
        unsafe { self.mem.unmap() }
    }
}
impl<T> std::ops::Deref for DynamicUpdateBufferWriteRange<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.mapped_owner.get(0) }
    }
}
impl<T> std::ops::DerefMut for DynamicUpdateBufferWriteRange<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.mapped_owner.get_mut(0) }
    }
}

pub struct DynamicUpdateBufferWriteSliceRange<'a, T> {
    mem: &'a br::DeviceMemory,
    mapped_owner: br::MappedMemoryRange<'a>,
    slice_length: usize,
    _back_data: std::marker::PhantomData<*mut T>,
}
impl<T> Drop for DynamicUpdateBufferWriteSliceRange<'_, T> {
    fn drop(&mut self) {
        unsafe { self.mem.unmap() }
    }
}
impl<T> std::ops::Deref for DynamicUpdateBufferWriteSliceRange<'_, T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        unsafe { self.mapped_owner.slice(0, self.slice_length) }
    }
}
impl<T> std::ops::DerefMut for DynamicUpdateBufferWriteSliceRange<'_, T> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { self.mapped_owner.slice_mut(0, self.slice_length) }
    }
}

pub trait StatedBufferRegion {
    const fn byte_length(&self) -> usize;
    const fn byte_range_dev(&self) -> std::ops::Range<u64>;
    const fn byte_range(&self) -> std::ops::Range<usize>;
}

pub struct StatedBufferSliceRegion<T> {
    pub offset: u64,
    pub first_usage_stage: br::PipelineStageFlags,
    pub access_mask: br::vk::VkAccessFlags,
    pub slice_length: usize,
    _element: std::marker::PhantomData<T>,
}
impl<T> StatedBufferSliceRegion<T> {
    pub const fn new(
        offset: u64,
        first_usage_stage: br::PipelineStageFlags,
        access_mask: br::vk::VkAccessFlags,
        slice_length: usize,
    ) -> Self {
        Self {
            offset,
            first_usage_stage,
            access_mask,
            slice_length,
            _element: std::marker::PhantomData,
        }
    }
}
impl<T> StatedBufferRegion for StatedBufferSliceRegion<T> {
    const fn byte_length(&self) -> usize {
        std::mem::size_of::<T>() * self.slice_length
    }
    const fn byte_range_dev(&self) -> std::ops::Range<u64> {
        self.offset..(self.offset + self.byte_length() as u64)
    }
    const fn byte_range(&self) -> std::ops::Range<usize> {
        self.offset as _..(self.offset as usize + self.byte_length())
    }
}

/// Buffer region with execution time states
pub struct StatedBufferRegionSingleElement<T> {
    pub offset: u64,
    pub first_usage_stage: br::PipelineStageFlags,
    pub access_mask: br::vk::VkAccessFlags,
    _buffer_content: std::marker::PhantomData<T>,
}
impl<T> StatedBufferRegionSingleElement<T> {
    pub const fn new(
        offset: u64,
        first_usage_stage: br::PipelineStageFlags,
        access_mask: br::vk::VkAccessFlags,
    ) -> Self {
        Self {
            offset,
            first_usage_stage,
            access_mask,
            _buffer_content: std::marker::PhantomData,
        }
    }
}
impl<T> StatedBufferRegion for StatedBufferRegionSingleElement<T> {
    const fn byte_length(&self) -> usize {
        std::mem::size_of::<T>()
    }
    const fn byte_range_dev(&self) -> std::ops::Range<u64> {
        self.offset..(self.offset + self.byte_length() as u64)
    }
    const fn byte_range(&self) -> std::ops::Range<usize> {
        self.offset as _..self.offset as usize + self.byte_length()
    }
}

pub struct DynamicUpdateBuffer {
    data: peridot::Buffer,
    ops: peridot::TransferBatch,
    copy_regions: Vec<(
        br::vk::VkDeviceSize,
        peridot::Buffer,
        br::vk::VkDeviceSize,
        usize,
    )>,
    cap: usize,
    top: usize,
}
impl DynamicUpdateBuffer {
    const DEFAULT_CAPACITY: usize = 256;

    pub fn new(
        e: &peridot::Graphics,
        init_cp_batch: &mut peridot::TransferBatch,
    ) -> br::Result<Self> {
        let mut bp = peridot::BufferPrealloc::new(e);
        bp.add(peridot::BufferContent::Raw(Self::DEFAULT_CAPACITY as _, 1));
        let mut mb = peridot::MemoryBadget::new(e);
        mb.add(bp.build_upload()?);
        let data = mb
            .alloc_upload()?
            .pop()
            .expect("no object?")
            .unwrap_buffer();

        init_cp_batch.add_buffer_graphics_ready(
            br::PipelineStageFlags::HOST,
            &data,
            0..Self::DEFAULT_CAPACITY as _,
            br::AccessFlags::HOST.write,
        );

        Ok(Self {
            data,
            ops: peridot::TransferBatch::new(),
            copy_regions: Vec::new(),
            cap: Self::DEFAULT_CAPACITY,
            top: 0,
        })
    }

    pub fn reset(&mut self) {
        self.top = 0;
        self.ops.clear();
        self.copy_regions.clear();
    }

    pub fn push_update<'s, T>(
        &'s mut self,
        g: &peridot::Graphics,
        dst_buffer: &peridot::Buffer,
        stated_region: &StatedBufferRegionSingleElement<T>,
    ) -> DynamicUpdateBufferWriteRange<'s, T> {
        let place_offset = self.push_update_ext_copy::<T>(g);

        self.copy_regions.push((
            place_offset as _,
            dst_buffer.clone(),
            stated_region.offset,
            std::mem::size_of::<T>(),
        ));
        self.ops.add_buffer_graphics_ready(
            stated_region.first_usage_stage,
            &dst_buffer,
            stated_region.byte_range_dev(),
            stated_region.access_mask,
        );

        DynamicUpdateBufferWriteRange {
            mem: self.data.memory(),
            mapped_owner: self
                .data
                .memory()
                .map(place_offset..self.top)
                .expect("Failed to mapping memory"),
            _back_data: std::marker::PhantomData,
        }
    }

    pub fn push_update_dyn_array<'s, T>(
        &'s mut self,
        g: &peridot::Graphics,
        dst_buffer: &peridot::Buffer,
        stated_region: &StatedBufferSlicedRegion<T>,
    ) -> DynamicUpdateBufferWriteSliceRange<'s, T> {
        let place_offset = self.push_update_dyn_array_ext_copy::<T>(g, stated_region.slice_length);

        self.copy_regions.push((
            place_offset as _,
            dst_buffer.clone(),
            stated_region.offset,
            std::mem::size_of::<T>(),
        ));
        self.ops.add_buffer_graphics_ready(
            stated_region.first_usage_stage,
            &dst_buffer,
            stated_region.byte_range_dev(),
            stated_region.access_mask,
        );

        DynamicUpdateBufferWriteSliceRange {
            mem: self.data.memory(),
            mapped_owner: self
                .data
                .memory()
                .map(place_offset..self.top)
                .expect("Failed to mapping memory"),
            slice_length: stated_region.slice_length,
            _back_data: std::marker::PhantomData,
        }
    }

    fn push_update_ext_copy<'s, T>(&'s mut self, g: &peridot::Graphics) -> usize {
        let place_offset = std::mem::align_of::<T>()
            * ((self.top + std::mem::align_of::<T>() - 1) / std::mem::align_of::<T>());
        if place_offset + std::mem::size_of::<T>() > self.cap {
            unimplemented!("realloc needed");
        }
        self.top = place_offset + std::mem::size_of::<T>();

        place_offset
    }

    fn push_update_dyn_array_ext_copy<'s, T>(
        &'s mut self,
        g: &peridot::Graphics,
        array_size: usize,
    ) -> usize {
        let place_size = std::mem::size_of::<T>() * array_size;
        let place_offset = std::mem::align_of::<T>()
            * ((self.top + std::mem::align_of::<T>() - 1) / std::mem::align_of::<T>());
        if place_offset + place_size > self.cap {
            unimplemented!("realloc needed");
        }
        self.top = place_offset + place_size;

        place_offset
    }

    pub fn populate_commands(&mut self, cmd: &mut br::CmdRecord) {
        for (src_offset, dst_buf, dst_offset, size) in self.copy_regions.drain(..) {
            self.ops.add_copying_buffer(
                self.data.with_dev_offset(src_offset),
                dst_buf.with_dev_offset(dst_offset),
                size as _,
            );
        }

        self.ops.sink_transfer_commands(cmd);
        self.ops.sink_graphics_ready_commands(cmd);
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct FontId(usize);
pub struct UIContext {
    charatlas: peridot::DynamicTextureAtlas<peridot::BookshelfBinning>,
    font_provider: peridot_vg::FontProvider,
    fonts: Vec<peridot_vg::Font>,
    baked_characters: HashMap<(FontId, char), peridot::TextureSlice>,
    update_buffer: DynamicUpdateBuffer,
}
impl UIContext {
    pub fn new(
        g: &peridot::Graphics,
        res_storage_alloc: &mut peridot::BulkedResourceStorageAllocator,
        character_atlas_size: u32,
        init_cp_batch: &mut peridot::TransferBatch,
    ) -> Self {
        UIContext {
            charatlas: peridot::DynamicTextureAtlas::new(
                g,
                peridot::math::Vector2(character_atlas_size, character_atlas_size),
                br::vk::VK_FORMAT_R8_UNORM,
                res_storage_alloc,
            )
            .expect("Failed to create Dynamic Texture Atlas for Characters"),
            font_provider: peridot_vg::FontProvider::new()
                .expect("Failed to initialize font provider"),
            fonts: Vec::new(),
            baked_characters: HashMap::new(),
            update_buffer: DynamicUpdateBuffer::new(g, init_cp_batch)
                .expect("Failed to create update buffer object"),
        }
    }

    pub fn load_font(
        &mut self,
        families: &str,
        properties: &peridot_vg::FontProperties,
    ) -> Result<FontId, peridot_vg::FontConstructionError> {
        self.font_provider
            .best_match(families, properties, 12.0)
            .map(|f| self.register_font(f))
    }
    pub fn register_font(&mut self, font: peridot_vg::Font) -> FontId {
        self.fonts.push(font);
        FontId(self.fonts.len() - 1)
    }
    pub fn query_baked_character(
        &mut self,
        font: FontId,
        c: char,
    ) -> Option<&peridot::TextureSlice> {
        match self.baked_characters.entry((font, c)) {
            std::collections::hash_map::Entry::Occupied(e) => Some(e.into_mut()),
            std::collections::hash_map::Entry::Vacant(v) => {
                // rasterize sdf and insert to cache
                unimplemented!("Rasterize SDF and insert to cache");
            }
        }
    }
}

#[derive(Clone)]
pub struct Transform {
    pub position: peridot::math::Vector3<f32>,
    pub scale: peridot::math::Vector3<f32>,
    pub rotation: peridot::math::Quaternion<f32>,
}
impl Default for Transform {
    fn default() -> Self {
        Transform {
            position: peridot::math::Zero::ZERO,
            scale: peridot::math::One::ONE,
            rotation: peridot::math::One::ONE,
        }
    }
}
pub struct PartialTransform {
    pub position: Option<peridot::math::Vector3<f32>>,
    pub scale: Option<peridot::math::Vector3<f32>>,
    pub rotation: Option<peridot::math::Quaternion<f32>>,
}
impl Default for PartialTransform {
    fn default() -> Self {
        PartialTransform {
            position: None,
            scale: None,
            rotation: None,
        }
    }
}
pub trait UIElement {
    #[allow(unused_variables)]
    fn layout(&mut self, placed_at: &Transform) {}
    fn layout_size(&self) -> peridot::math::Vector2<f32>;
    fn prepare_render(&mut self, ctx: &mut UIContext, engine: &peridot::Graphics);
}

pub struct UIPlane {
    base: Transform,
    root: Option<Box<dyn UIElement>>,
}
impl UIPlane {
    pub fn new() -> Self {
        UIPlane {
            base: Transform::default(),
            root: None,
        }
    }

    pub fn set_root(&mut self, root: Box<dyn UIElement>) {
        self.root = Some(root);
    }
    pub fn layout_all(&mut self) {
        if let Some(ref mut r) = self.root {
            r.layout(&self.base);
        }
    }
    pub fn prepare_render(&mut self, ctx: &mut UIContext, engine: &peridot::Graphics) {
        if let Some(ref mut r) = self.root {
            r.prepare_render(ctx, engine);
        }
    }
}

/// Layouted uneditable text
pub struct StaticLabelRenderer {
    font: FontId,
    text: String,
    size: f32,
    merged_vertices_view: Option<peridot::DeviceBufferViewHold>,
}
pub struct StaticLabel {
    transform: Transform,
    renderer: StaticLabelRenderer,
}
impl UIElement for StaticLabel {
    fn layout_size(&self) -> peridot::math::Vector2<f32> {
        unimplemented!("calc staticlabel layout metrics");
    }
    fn prepare_render(&mut self, ctx: &mut UIContext, engine: &peridot::Graphics) {
        if self.renderer.merged_vertices_view.is_none() {
            let slices = self
                .renderer
                .text
                .chars()
                .map(|c| ctx.query_baked_character(self.renderer.font, c).cloned())
                .collect::<Option<Vec<_>>>()
                .unwrap_or_else(Vec::new);
            let mut bp = peridot::BufferPrealloc::new(engine);
            bp.add(peridot::BufferContent::vertices::<peridot::VertexUV2D>(
                slices.len() * 6,
            ));
            let mut mb = peridot::MemoryBadget::new(engine);
            mb.add(
                bp.build_transferred()
                    .expect("Failed to build label vertices buffer"),
            );
            let buffer = mb
                .alloc()
                .expect("Failed to allocate label vertices memory")
                .pop()
                .expect("no objects?")
                .unwrap_buffer();
            self.renderer.merged_vertices_view = Some(buffer.hold_with_dev_offset(0));
            unimplemented!("character layouting");

            /*let wref = ctx
            .update_buffer
            .push_update_dyn_array::<peridot::VertexUV2D>(
                engine,
                &buffer,
                &StatedBufferRegion::new(
                    0,
                    br::PipelineStageFlags::VERTEX_INPUT,
                    br::AccessFlags::VERTEX_ATTRIBUTE_READ,
                ),
                slices.len() * 6,
            );*/
        }
    }
}
impl StaticLabel {
    pub fn new(font: FontId, text: &str, size: f32) -> Self {
        Self {
            transform: Transform::default(),
            renderer: StaticLabelRenderer {
                font,
                text: String::from(text),
                size: size / 12.0,
                merged_vertices_view: None,
            },
        }
    }

    pub fn set_text(&mut self, text: &str) {
        self.renderer.merged_vertices_view = None;
        self.renderer.text = String::from(text);
    }
}

pub struct VerticalLayoutGroup {
    children: Vec<(Box<dyn UIElement>, Transform)>,
}

pub struct ListGroup {
    children: Vec<(Box<dyn UIElement>, Transform)>,
}
impl UIElement for ListGroup {
    fn layout(&mut self, placed_at: &Transform) {
        let mut offset_y = 0.0;
        for (e, et) in &mut self.children {
            *et = Transform {
                position: placed_at.position.clone() + peridot::math::Vector3(0.0, offset_y, 0.0),
                ..placed_at.clone()
            };
            e.layout(et);
            offset_y += e.layout_size().1;
        }
    }
    fn layout_size(&self) -> peridot::math::Vector2<f32> {
        self.children
            .iter()
            .map(|e| e.0.layout_size())
            .fold(peridot::math::Vector2(0.0, 0.0), |s, es| {
                peridot::math::Vector2(s.0.max(es.0), s.1 + es.1)
            })
    }
    fn prepare_render(&mut self, ctx: &mut UIContext, engine: &peridot::Graphics) {
        for (e, _) in &mut self.children {
            e.prepare_render(ctx, engine);
        }
    }
}
pub struct InlineGroup {
    children: Vec<(Box<dyn UIElement>, Transform)>,
}
impl UIElement for InlineGroup {
    fn layout(&mut self, placed_at: &Transform) {
        let mut offset_x = 0.0;
        for (e, et) in &mut self.children {
            *et = Transform {
                position: placed_at.position.clone() + peridot::math::Vector3(offset_x, 0.0, 0.0),
                ..placed_at.clone()
            };
            e.layout(et);
            offset_x += e.layout_size().0;
        }
    }
    fn layout_size(&self) -> peridot::math::Vector2<f32> {
        self.children
            .iter()
            .map(|e| e.0.layout_size())
            .fold(peridot::math::Vector2(0.0, 0.0), |s, es| {
                peridot::math::Vector2(s.0 + es.0, s.1.max(es.1))
            })
    }
    fn prepare_render(&mut self, ctx: &mut UIContext, engine: &peridot::Graphics) {
        for (e, _) in &mut self.children {
            e.prepare_render(ctx, engine);
        }
    }
}
