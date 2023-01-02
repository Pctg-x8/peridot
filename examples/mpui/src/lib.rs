use bedrock as br;
use br::MemoryBound;
use peridot::mthelper::{DynamicMut, DynamicMutabilityProvider, SharedRef};

pub struct Game<NL> {
    ui: UIContext,
    ui_main: UIPlane,
    res_storage: peridot::ResourceStorage<
        peridot::Buffer<
            br::BufferObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
        peridot::StdImage,
    >,
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
    mapped_owner:
        peridot::AutocloseMappedMemoryRange<'a, br::DeviceMemoryObject<peridot::DeviceObject>>,
    _back_data: std::marker::PhantomData<*mut T>,
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
    mapped_owner:
        peridot::AutocloseMappedMemoryRange<'a, br::DeviceMemoryObject<peridot::DeviceObject>>,
    slice_length: usize,
    _back_data: std::marker::PhantomData<*mut T>,
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
    fn byte_length(&self) -> usize;
    fn byte_range_dev(&self) -> std::ops::Range<u64>;
    fn byte_range(&self) -> std::ops::Range<usize>;
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
    fn byte_length(&self) -> usize {
        std::mem::size_of::<T>() * self.slice_length
    }
    fn byte_range_dev(&self) -> std::ops::Range<u64> {
        self.offset..(self.offset + self.byte_length() as u64)
    }
    fn byte_range(&self) -> std::ops::Range<usize> {
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
    fn byte_length(&self) -> usize {
        std::mem::size_of::<T>()
    }
    fn byte_range_dev(&self) -> std::ops::Range<u64> {
        self.offset..(self.offset + self.byte_length() as u64)
    }
    fn byte_range(&self) -> std::ops::Range<usize> {
        self.offset as _..self.offset as usize + self.byte_length()
    }
}

pub trait BufferUpdateRequestData<Device: br::Device> {
    fn data_bytes(&self) -> &[u8];
    fn dest_buffer(&self) -> &dyn br::Buffer<ConcreteDevice = Device>;
    fn dest_offset(&self) -> br::vk::VkDeviceSize;
    fn dest_barrier(&self) -> (br::PipelineStageFlags, br::vk::VkAccessFlags);
}

pub struct BufferUpdateRequest<'d, T: Clone, Buffer: br::Buffer> {
    pub data: std::borrow::Cow<'d, T>,
    pub dest: peridot::DeviceBufferView<Buffer>,
    pub dest_barriers: (br::PipelineStageFlags, br::vk::VkAccessFlags),
}
impl<'d, T: Clone, Buffer: br::Buffer> BufferUpdateRequestData<Buffer::ConcreteDevice>
    for BufferUpdateRequest<'d, T, Buffer>
{
    fn data_bytes(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(
                self.data.as_ref() as *const T as _,
                std::mem::size_of::<T>(),
            )
        }
    }

    fn dest_buffer(&self) -> &dyn br::Buffer<ConcreteDevice = Buffer::ConcreteDevice> {
        &self.dest.buffer
    }

    fn dest_offset(&self) -> br::vk::VkDeviceSize {
        self.dest.offset
    }

    fn dest_barrier(&self) -> (br::PipelineStageFlags, br::vk::VkAccessFlags) {
        self.dest_barriers
    }
}

pub struct BufferArrayUpdateRequest<'d, T: Clone, Buffer: br::Buffer> {
    pub data: std::borrow::Cow<'d, [T]>,
    pub dest: peridot::DeviceBufferView<Buffer>,
    pub dest_barriers: (br::PipelineStageFlags, br::vk::VkAccessFlags),
}
impl<'d, T: Clone, Buffer: br::Buffer> BufferUpdateRequestData<Buffer::ConcreteDevice>
    for BufferArrayUpdateRequest<'d, T, Buffer>
{
    fn data_bytes(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(
                self.data.as_ref().as_ptr() as *const _,
                self.data.len() * std::mem::size_of::<T>(),
            )
        }
    }

    fn dest_buffer(&self) -> &dyn br::Buffer<ConcreteDevice = Buffer::ConcreteDevice> {
        &self.dest.buffer
    }

    fn dest_offset(&self) -> br::vk::VkDeviceSize {
        self.dest.offset
    }

    fn dest_barrier(&self) -> (br::PipelineStageFlags, br::vk::VkAccessFlags) {
        self.dest_barriers
    }
}

pub struct BufferUpdateManager {
    stg_data: SharedRef<DynamicMut<peridot::StdBuffer>>,
    cap: usize,
}
impl BufferUpdateManager {
    const DEFAULT_CAPACITY: usize = 256;

    pub fn new(
        e: &peridot::Graphics,
        init_cp_batch: &mut peridot::TransferBatch,
    ) -> br::Result<Self> {
        let bp = peridot::BufferPrealloc::with_entries(
            e,
            std::iter::once(peridot::BufferContent::Raw(Self::DEFAULT_CAPACITY as _, 1)),
        );
        let mut alloc =
            peridot::BulkedResourceStorageAllocator::<_, peridot::StdImageBackend>::new();
        alloc.add_buffer(bp.build_upload()?);
        let peridot::ResourceStorage { mut buffers, .. } = alloc.alloc_upload(e)?;
        let stg_data = SharedRef::new(DynamicMut::new(buffers.pop().expect("no objects?")));

        init_cp_batch.add_buffer_graphics_ready(
            br::PipelineStageFlags::HOST,
            stg_data.clone(),
            0..Self::DEFAULT_CAPACITY as _,
            br::AccessFlags::HOST.write,
        );

        Ok(Self {
            stg_data,
            cap: Self::DEFAULT_CAPACITY,
        })
    }

    fn reserve_enough(
        &mut self,
        e: &peridot::Graphics,
        request_size: usize,
        tfb: &mut peridot::TransferBatch,
    ) -> br::Result<()> {
        if self.cap >= request_size {
            return Ok(());
        }

        let bp = peridot::BufferPrealloc::with_entries(
            e,
            std::iter::once(peridot::BufferContent::Raw(request_size as _, 1)),
        );
        let mut alloc =
            peridot::BulkedResourceStorageAllocator::<_, peridot::StdImageBackend>::new();
        alloc.add_buffer(bp.build_upload()?);
        let peridot::ResourceStorage { mut buffers, .. } = alloc.alloc_upload(e)?;

        self.cap = request_size;
        self.stg_data = SharedRef::new(DynamicMut::new(buffers.pop().expect("no objects?")));

        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::HOST,
            self.stg_data.clone(),
            0..Self::DEFAULT_CAPACITY as _,
            br::AccessFlags::HOST.write,
        );

        Ok(())
    }

    pub fn run(
        &mut self,
        e: &mut peridot::Graphics,
        requests: &[Box<dyn BufferUpdateRequestData<peridot::DeviceObject>>],
    ) -> br::Result<()> {
        let mut pre_tfb = peridot::TransferBatch::new();
        let mut total_size = 0;
        let mut placement_offsets = Vec::with_capacity(requests.len());
        for r in requests {
            placement_offsets.push(total_size);
            total_size += r.data_bytes().len();
        }
        self.reserve_enough(e, total_size, &mut pre_tfb)?;
        pre_tfb.submit(e)?;

        self.stg_data
            .borrow_mut()
            .guard_map(0..total_size as _, |range| {
                for (r, &o) in requests.iter().zip(&placement_offsets) {
                    unsafe {
                        range
                            .slice_mut(o, r.data_bytes().len())
                            .copy_from_slice(r.data_bytes());
                    }
                }
            })?;

        let stg_data = self.stg_data.borrow();
        let mut tfb = peridot::TransferBatch::new();
        for (r, &o) in requests.iter().zip(&placement_offsets) {
            tfb.add_copying_buffer(
                stg_data.with_dev_offset_ref(o as _),
                peridot::DeviceBufferView {
                    buffer: r.dest_buffer(),
                    offset: r.dest_offset(),
                },
                r.data_bytes().len() as _,
            );
            tfb.add_buffer_graphics_ready(
                r.dest_barrier().0,
                r.dest_buffer(),
                o as _..(o + r.data_bytes().len()) as _,
                r.dest_barrier().1,
            );
        }

        tfb.submit(e)
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
    update_buffer: BufferUpdateManager,
}
impl UIContext {
    pub fn new(
        g: &peridot::Graphics,
        res_storage_alloc: &mut peridot::BulkedResourceStorageAllocator<
            br::BufferObject<peridot::DeviceObject>,
            peridot::StdImageBackend,
        >,
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
            update_buffer: BufferUpdateManager::new(g, init_cp_batch)
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
    merged_vertices_view: Option<
        peridot::DeviceBufferView<
            peridot::Buffer<
                br::BufferObject<peridot::DeviceObject>,
                br::DeviceMemoryObject<peridot::DeviceObject>,
            >,
        >,
    >,
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
            let mut alloc =
                peridot::BulkedResourceStorageAllocator::<_, peridot::StdImageBackend>::new();
            alloc.add_buffer(
                bp.build_transferred()
                    .expect("Failed to build label vertices buffer"),
            );
            let peridot::ResourceStorage { mut buffers, .. } = alloc
                .alloc(engine)
                .expect("Failed to allocate label vertices memory");
            let buffer = buffers.pop().expect("no object?");
            self.renderer.merged_vertices_view = Some(buffer.with_dev_offset(0));
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
