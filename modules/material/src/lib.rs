use std::collections::HashMap;

use peridot_math::Vector4F32;
use peridot_types::AssetID;

pub enum MaterialPropertySlot<ExternalAssetRef> {
    Float4(Vector4F32),
    Texture2D(ExternalAssetRef),
}

pub struct MaterialPropertyBlock<ExternalAssetRef> {
    map: HashMap<String, MaterialPropertySlot<ExternalAssetRef>>,
}
impl<ExternalAssetRef> MaterialPropertyBlock<ExternalAssetRef> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn set_float4(&mut self, name: String, value: Vector4F32) {
        self.map.insert(name, MaterialPropertySlot::Float4(value));
    }

    pub fn set_texture2d(&mut self, name: String, value: ExternalAssetRef) {
        self.map
            .insert(name, MaterialPropertySlot::Texture2D(value));
    }
}

#[repr(u8)]
pub enum CullingMode {
    None = 0,
    Front = 1,
    Back = 2,
    Both = 3,
}

#[repr(u8)]
pub enum PrimitiveOrder {
    Clcowise = 0,
    CounterClockwise = 1,
}

pub struct MaterialAsset<ExternalAssetRef> {
    pub shader: AssetID,
    pub properties: MaterialPropertyBlock<ExternalAssetRef>,
    pub wireframed: bool,
    pub culling: CullingMode,
    pub prim_order: PrimitiveOrder,
}
