//! GLTF JSON Properties: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#properties-reference

use std::collections::HashMap;

use serde::Deserialize;

pub type Extension = serde_json::Map<String, serde_json::Value>;
pub type Extras = serde_json::Value;

pub type ComponentType = i32;
pub const COMPONENT_TYPE_BYTE: ComponentType = 5120;
pub const COMPONENT_TYPE_UNSIGNED_BYTE: ComponentType = 5121;
pub const COMPONENT_TYPE_SHORT: ComponentType = 5122;
pub const COMPONENT_TYPE_UNSIGNED_SHORT: ComponentType = 5123;
pub const COMPONENT_TYPE_INT: ComponentType = 5124;
pub const COMPONENT_TYPE_UNSIGNED_INT: ComponentType = 5125;
pub const COMPONENT_TYPE_FLOAT: ComponentType = 5126;

#[derive(Deserialize, Debug, Clone, Copy)]
#[serde(rename_all = "UPPERCASE")]
pub enum AccessorType {
    Scalar,
    Vec2,
    Vec3,
    Vec4,
    Mat2,
    Mat3,
    Mat4,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Accessor {
    pub buffer_view: Option<usize>,
    #[serde(default = "Accessor::default_byte_offset")]
    pub byte_offset: usize,
    pub component_type: ComponentType,
    #[serde(default = "Accessor::default_normalized")]
    pub normalized: bool,
    pub count: usize,
    pub r#type: AccessorType,
    pub max: Option<Vec<f32>>,
    pub min: Option<Vec<f32>>,
    pub sprase: Option<AccessorSparse>,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl Accessor {
    const fn default_byte_offset() -> usize {
        0
    }

    const fn default_normalized() -> bool {
        false
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct AccessorSparse {
    pub count: usize,
    pub indices: AccessorSparseIndices,
    pub values: AccessorSparseValues,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct AccessorSparseIndices {
    pub buffer_view: usize,
    #[serde(default = "AccessorSparseIndices::default_byte_offset")]
    pub byte_offser: usize,
    pub comopnent_type: ComponentType,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl AccessorSparseIndices {
    const fn default_byte_offset() -> usize {
        0
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct AccessorSparseValues {
    pub buffer_view: usize,
    #[serde(default = "AccessorSparseValues::default_byte_offset")]
    pub byte_offset: usize,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl AccessorSparseValues {
    const fn default_byte_offset() -> usize {
        0
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Animation {
    pub channels: Vec<AnimationChannel>,
    pub samplers: Vec<AnimationSampler>,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct AnimationChannel {
    pub sampler: usize,
    pub target: AnimationChannelTarget,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum AnimationChannelTargetPath {
    Transtion,
    Rotation,
    Scale,
    Weights,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct AnimationChannelTarget {
    pub node: Option<usize>,
    pub path: AnimationChannelTargetPath,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "UPPERCASE")]
pub enum AnimationSamplerInterpolation {
    Linear,
    Step,
    CubicSpline,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct AnimationSampler {
    pub input: usize,
    #[serde(default = "AnimationSampler::default_interpolation")]
    pub interpolation: AnimationSamplerInterpolation,
    pub output: usize,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

impl AnimationSampler {
    const fn default_interpolation() -> AnimationSamplerInterpolation {
        AnimationSamplerInterpolation::Linear
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Asset {
    pub copyright: Option<String>,
    pub generator: Option<String>,
    pub version: String,
    pub min_version: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Buffer {
    pub uri: Option<String>,
    pub byte_length: usize,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

pub type BufferViewTarget = i32;
pub const BUFFER_VIEW_TARGET_ARRAY_BUFFER: BufferViewTarget = 34962;
pub const BUFFER_VIEW_TARGET_ELEMENT_ARRAY_BUFFER: BufferViewTarget = 34963;

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct BufferView {
    pub buffer: usize,
    #[serde(default = "BufferView::default_byte_offset")]
    pub byte_offset: usize,
    pub byte_length: usize,
    pub byte_stride: Option<usize>,
    pub target: Option<BufferViewTarget>,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl BufferView {
    const fn default_byte_offset() -> usize {
        0
    }
}

#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum CameraType {
    Perspective,
    Orthographic,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Camera {
    pub orthographic: Option<CameraOrthographic>,
    pub perspective: Option<CameraPerspective>,
    pub r#type: CameraType,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CameraOrthographic {
    pub xmag: f32,
    pub ymag: f32,
    pub zfar: f32,
    pub znear: f32,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct CameraPerspective {
    pub aspect_ratio: Option<f32>,
    pub yfov: f32,
    pub zfar: Option<f32>,
    pub znear: f32,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct GLTF {
    #[serde(default)]
    pub extensions_used: Vec<String>,
    #[serde(default)]
    pub extensions_required: Vec<String>,
    #[serde(default)]
    pub accessors: Vec<Accessor>,
    #[serde(default)]
    pub animations: Vec<Animation>,
    pub asset: Asset,
    #[serde(default)]
    pub buffers: Vec<Buffer>,
    #[serde(default)]
    pub buffer_views: Vec<BufferView>,
    #[serde(default)]
    pub cameras: Vec<Camera>,
    #[serde(default)]
    pub images: Vec<Image>,
    #[serde(default)]
    pub materials: Vec<Material>,
    #[serde(default)]
    pub meshes: Vec<Mesh>,
    #[serde(default)]
    pub nodes: Vec<Node>,
    #[serde(default)]
    pub samplers: Vec<Sampler>,
    pub scene: Option<usize>,
    #[serde(default)]
    pub scenes: Vec<Scene>,
    #[serde(default)]
    pub skins: Vec<Skin>,
    #[serde(default)]
    pub textures: Vec<Texture>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Image {
    pub uri: Option<String>,
    pub mime_type: Option<String>,
    pub buffer_view: Option<usize>,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "UPPERCASE")]
pub enum MaterialAlphaMode {
    Opaque,
    Mask,
    Blend,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Material {
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
    pub pbr_metallic_roughness: Option<MaterialPBRMetallicRoughness>,
    pub normal_texture: Option<MaterialNormalTextureInfo>,
    pub occlusion_texture: Option<MaterialOcclusionTextureInfo>,
    pub emissive_texture: Option<TextureInfo>,
    #[serde(default = "Material::default_emissive_factor")]
    pub emissive_factor: [f32; 3],
    #[serde(default = "Material::default_alpha_mode")]
    pub alpha_mode: MaterialAlphaMode,
    #[serde(default = "Material::default_alpha_cutoff")]
    pub alpha_cutoff: f32,
    #[serde(default = "Material::default_double_sided")]
    pub double_sided: bool,
}
impl Material {
    const fn default_emissive_factor() -> [f32; 3] {
        [0.0, 0.0, 0.0]
    }

    const fn default_alpha_mode() -> MaterialAlphaMode {
        MaterialAlphaMode::Opaque
    }

    const fn default_alpha_cutoff() -> f32 {
        0.5
    }

    const fn default_double_sided() -> bool {
        false
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MaterialNormalTextureInfo {
    pub index: usize,
    #[serde(default = "MaterialNormalTextureInfo::default_tex_coord")]
    pub tex_coord: usize,
    #[serde(default = "MaterialNormalTextureInfo::default_scale")]
    pub scale: f32,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl MaterialNormalTextureInfo {
    const fn default_tex_coord() -> usize {
        0
    }

    const fn default_scale() -> f32 {
        1.0
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MaterialOcclusionTextureInfo {
    pub index: usize,
    #[serde(default = "MaterialOcclusionTextureInfo::default_tex_coord")]
    pub tex_coord: usize,
    #[serde(default = "MaterialOcclusionTextureInfo::default_strength")]
    pub strength: f32,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl MaterialOcclusionTextureInfo {
    const fn default_tex_coord() -> usize {
        0
    }

    const fn default_strength() -> f32 {
        1.0
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MaterialPBRMetallicRoughness {
    #[serde(default = "MaterialPBRMetallicRoughness::default_base_color_factor")]
    pub base_color_factor: [f32; 4],
    pub base_color_texture: Option<TextureInfo>,
    #[serde(default = "MaterialPBRMetallicRoughness::default_metallic_factor")]
    pub metallic_factor: f32,
    #[serde(default = "MaterialPBRMetallicRoughness::default_roughness_factor")]
    pub roughness_factor: f32,
    pub metallic_roughness_texture: Option<TextureInfo>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl MaterialPBRMetallicRoughness {
    const fn default_base_color_factor() -> [f32; 4] {
        [1.0, 1.0, 1.0, 1.0]
    }

    const fn default_metallic_factor() -> f32 {
        1.0
    }

    const fn default_roughness_factor() -> f32 {
        1.0
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Mesh {
    pub primitives: Vec<MeshPrimitive>,
    #[serde(default)]
    pub weights: Vec<f32>,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

pub type MeshPrimitiveMode = u8;
pub const MESH_PRIMITIVE_MODE_POINTS: MeshPrimitiveMode = 0;
pub const MESH_PRIMITIVE_MODE_LINES: MeshPrimitiveMode = 1;
pub const MESH_PRIMITIVE_MODE_LINE_LOOP: MeshPrimitiveMode = 2;
pub const MESH_PRIMITIVE_MODE_LINE_STRIP: MeshPrimitiveMode = 3;
pub const MESH_PRIMITIVE_MODE_TRIANGLES: MeshPrimitiveMode = 4;
pub const MESH_PRIMITIVE_MODE_TRIANGLE_STRIP: MeshPrimitiveMode = 5;
pub const MESH_PRIMITIVE_MODE_TRIANGLE_FAN: MeshPrimitiveMode = 6;

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MeshPrimitive {
    pub attributes: HashMap<String, usize>,
    pub indices: Option<usize>,
    pub material: Option<usize>,
    #[serde(default = "MeshPrimitive::default_mode")]
    pub mode: MeshPrimitiveMode,
    pub targets: Option<Vec<serde_json::Map<String, serde_json::Value>>>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl MeshPrimitive {
    const fn default_mode() -> MeshPrimitiveMode {
        MESH_PRIMITIVE_MODE_TRIANGLES
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Node {
    pub camera: Option<usize>,
    #[serde(default)]
    pub children: Vec<usize>,
    pub skin: Option<usize>,
    #[serde(default = "Node::default_matrix")]
    pub matrix: [f32; 16],
    pub mesh: Option<usize>,
    #[serde(default = "Node::default_rotation")]
    pub rotation: [f32; 4],
    #[serde(default = "Node::default_scale")]
    pub scale: [f32; 3],
    #[serde(default = "Node::default_translation")]
    pub translation: [f32; 3],
    #[serde(default)]
    pub weights: Vec<usize>,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl Node {
    const fn default_matrix() -> [f32; 16] {
        [
            1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ]
    }

    const fn default_rotation() -> [f32; 4] {
        [0.0, 0.0, 0.0, 1.0]
    }

    const fn default_scale() -> [f32; 3] {
        [1.0, 1.0, 1.0]
    }

    const fn default_translation() -> [f32; 3] {
        [0.0, 0.0, 0.0]
    }
}

pub type SamplerFilter = u16;
pub const SAMPLER_FILTER_NEAREST: SamplerFilter = 9728;
pub const SAMPLER_FILTER_LINEAR: SamplerFilter = 9729;
pub const SAMPLER_FILTER_NEAREST_MIPMAP_NEAREST: SamplerFilter = 9984;
pub const SAMPLER_FILTER_LINEAR_MIPMAP_NEAREST: SamplerFilter = 9985;
pub const SAMPLER_FILTER_NEAREST_MIPMAP_LINEAR: SamplerFilter = 9986;
pub const SAMPLER_FILTER_LINEAR_MIPMAP_LINEAR: SamplerFilter = 9987;

pub type SamplerWrap = u16;
pub const SAMPLER_WRAP_CLAMP_TO_EDGE: SamplerWrap = 33071;
pub const SAMPLER_WRAP_MIRRORED_REPEAT: SamplerWrap = 33648;
pub const SAMPLER_WRAP_REPEAT: SamplerWrap = 10497;

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Sampler {
    pub mag_filter: Option<SamplerFilter>,
    pub min_filter: Option<SamplerFilter>,
    #[serde(default = "Sampler::default_wrap_s")]
    pub wrap_s: SamplerWrap,
    #[serde(default = "Sampler::default_wrap_t")]
    pub wrap_t: SamplerWrap,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl Sampler {
    const fn default_wrap_s() -> SamplerWrap {
        SAMPLER_WRAP_REPEAT
    }

    const fn default_wrap_t() -> SamplerWrap {
        SAMPLER_WRAP_REPEAT
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Scene {
    #[serde(default)]
    pub nodes: Vec<usize>,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Skin {
    pub inverse_bind_matrices: Option<usize>,
    pub skeleton: Option<usize>,
    pub joints: Vec<usize>,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Texture {
    pub sampler: Option<usize>,
    pub source: Option<usize>,
    pub name: Option<String>,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct TextureInfo {
    pub index: usize,
    #[serde(default = "TextureInfo::default_tex_coord")]
    pub tex_coord: usize,
    #[serde(default)]
    pub extensions: Extension,
    #[serde(default)]
    pub extras: Extras,
}
impl TextureInfo {
    const fn default_tex_coord() -> usize {
        0
    }
}
