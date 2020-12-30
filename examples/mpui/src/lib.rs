
use bedrock as br;

pub struct Game<NL> {
    ui: UIContext,
    ui_main: UIPlane,
    res_storage: peridot::ResourceStorage,
    marker: std::marker::PhantomData<*const NL>
}
impl<NL> Game<NL> {
    pub const NAME: &'static str = "Multiplane UI Demo";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<NL> peridot::FeatureRequests for Game<NL> {}
impl<NL: peridot::NativeLinker> peridot::EngineEvents<NL> for Game<NL> {
    fn init(e: &mut peridot::Engine<NL>) -> Self {
        let mut res_storage_alloc = peridot::BulkedResourceStorageAllocator::new();
        let mut ui = UIContext::new(e.graphics(), &mut res_storage_alloc, 2048);
        let uifont = ui.load_font(&[peridot_vg::FamilyName::SansSerif], &peridot_vg::FontProperties::new())
            .expect("Failed to load UI Font");
        let mut ui_main = UIPlane::new();
        ui_main.set_root(Box::new(StaticLabel::new(&mut ui, uifont, "test", 12.0)));
        ui_main.prepare_render(&mut ui);
        ui_main.layout_all();

        Game {
            res_storage: res_storage_alloc.alloc(e.graphics()).expect("Failed to allocate resource storages"),
            ui,
            ui_main,
            marker: std::marker::PhantomData
        }
    }
}

use std::collections::HashMap;

#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct FontId(usize);
pub struct UIContext {
    charatlas: peridot::DynamicTextureAtlas<peridot::BookshelfBinning>,
    fonts: Vec<peridot_vg::Font>,
    baked_characters: HashMap<(FontId, char), peridot::TextureSlice>
}
impl UIContext {
    pub fn new(
        g: &peridot::Graphics,
        res_storage_alloc: &mut peridot::BulkedResourceStorageAllocator,
        character_atlas_size: u32
    ) -> Self {
        UIContext {
            charatlas: peridot::DynamicTextureAtlas::new(
                g, peridot::math::Vector2(character_atlas_size, character_atlas_size), br::vk::VK_FORMAT_R8_UNORM,
                res_storage_alloc
            ).expect("Failed to create Dynamic Texture Atlas for Characters"),
            fonts: Vec::new(),
            baked_characters: HashMap::new()
        }
    }

    pub fn load_font(
        &mut self, families: &[peridot_vg::FamilyName], properties: &peridot_vg::FontProperties
    ) -> Result<FontId, peridot_vg::FontConstructionError> {
        peridot_vg::Font::best_match(families, properties, 12.0).map(|f| self.register_font(f))
    }
    pub fn register_font(&mut self, font: peridot_vg::Font) -> FontId {
        self.fonts.push(font);
        FontId(self.fonts.len() - 1)
    }
    pub fn query_baked_character(&mut self, font: FontId, c: char) -> Option<&peridot::TextureSlice> {
        match self.baked_characters.entry((font, c)) {
            std::collections::hash_map::Entry::Occupied(e) => Some(e.into_mut()),
            std::collections::hash_map::Entry::Vacant(v) => {
                // rasterize sdf and insert to cache
                None
            }
        }
    }
}

#[derive(Clone)]
pub struct Transform {
    pub position: peridot::math::Vector3<f32>,
    pub scale: peridot::math::Vector3<f32>,
    pub rotation: peridot::math::Quaternion<f32>
}
impl Default for Transform {
    fn default() -> Self {
        Transform {
            position: peridot::math::Zero::ZERO,
            scale: peridot::math::One::ONE,
            rotation: peridot::math::One::ONE
        }
    }
}
pub struct PartialTransform {
    pub position: Option<peridot::math::Vector3<f32>>,
    pub scale: Option<peridot::math::Vector3<f32>>,
    pub rotation: Option<peridot::math::Quaternion<f32>>
}
impl Default for PartialTransform {
    fn default() -> Self {
        PartialTransform {
            position: None, scale: None, rotation: None
        }
    }
}
trait UIElement {
    #[allow(unused_variables)]
    fn layout(&mut self, placed_at: &Transform) {}
    fn layout_size(&self) -> peridot::math::Vector2<f32>;
    fn prepare_render(&mut self, ctx: &mut UIContext);
}

pub struct UIPlane {
    base: Transform,
    root: Option<Box<dyn UIElement>>
}
impl UIPlane {
    pub fn new() -> Self {
        UIPlane {
            base: Transform::default(),
            root: None
        }
    }

    pub fn set_root(&mut self, root: Box<dyn UIElement>) {
        self.root = Some(root);
    }
    pub fn layout_all(&mut self) {
        if let Some(ref mut r) = self.root { r.layout(&self.base); }
    }
    pub fn prepare_render(&mut self, ctx: &mut UIContext) {
        if let Some(ref mut r) = self.root { r.prepare_render(ctx); }
    }
}

/// Layouted uneditable text
pub enum StaticLabel {
    Unprepared(FontId, String, f32),
    Prepared {
        merged_vertices_view: peridot::DeviceBufferViewHold
    }
}
impl UIElement for StaticLabel {
    fn layout_size(&self) -> peridot::math::Vector2<f32> {
        unimplemented!("calc staticlabel layout metrics");
    }
    fn prepare_render(&mut self, ctx: &mut UIContext) {
        if let Self::Unprepared(font, text, scale) = self {
            unimplemented!("Prepare for Rendering StaticLabel");
        }
    }
}
impl StaticLabel {
    pub fn new(ctx: &UIContext, font: FontId, text: &str, size: f32) -> Self {
        StaticLabel::Unprepared(font, String::from(text), size / 12.0)
    }
}

pub struct ListGroup {
    children: Vec<(Box<dyn UIElement>, Transform)>
}
impl UIElement for ListGroup {
    fn layout(&mut self, placed_at: &Transform) {
        let mut offset_y = 0.0;
        for (e, et) in &mut self.children {
            *et = Transform {
                position: placed_at.position.clone() + peridot::math::Vector3(0.0, offset_y, 0.0),
                .. placed_at.clone()
            };
            e.layout(et);
            offset_y += e.layout_size().1;
        }
    }
    fn layout_size(&self) -> peridot::math::Vector2<f32> {
        self.children.iter()
            .map(|e| e.0.layout_size())
            .fold(peridot::math::Vector2(0.0, 0.0), |s, es| peridot::math::Vector2(s.0.max(es.0), s.1 + es.1))
    }
    fn prepare_render(&mut self, ctx: &mut UIContext) {
        for (e, _) in &mut self.children { e.prepare_render(ctx); }
    }
}
pub struct InlineGroup {
    children: Vec<(Box<dyn UIElement>, Transform)>
}
impl UIElement for InlineGroup {
    fn layout(&mut self, placed_at: &Transform) {
        let mut offset_x = 0.0;
        for (e, et) in &mut self.children {
            *et = Transform {
                position: placed_at.position.clone() + peridot::math::Vector3(offset_x, 0.0, 0.0),
                .. placed_at.clone()
            };
            e.layout(et);
            offset_x += e.layout_size().0;
        }
    }
    fn layout_size(&self) -> peridot::math::Vector2<f32> {
        self.children.iter()
            .map(|e| e.0.layout_size())
            .fold(peridot::math::Vector2(0.0, 0.0), |s, es| peridot::math::Vector2(s.0 + es.0, s.1.max(es.1)))
    }
    fn prepare_render(&mut self, ctx: &mut UIContext) {
        for (e, _) in &mut self.children { e.prepare_render(ctx); }
    }
}
