
use bedrock as br;

pub struct Game<NL> {
    charatlas: peridot::atlas::DynamicTextureAtlas<peridot::atlas::BookshelfBinning>,
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
        let charatlas = peridot::atlas::DynamicTextureAtlas::new(
            e.graphics(), peridot::math::Vector2(1024, 1024), br::vk::VK_FORMAT_R8_UNORM,
            &mut res_storage_alloc
        ).expect("Failed to create Dynamic Texture Atlas for Characters");

        Game {
            res_storage: res_storage_alloc.alloc(e.graphics()).expect("Failed to allocate resource storages"),
            charatlas,
            marker: std::marker::PhantomData
        }
    }
}
