#![warn(clippy::all)]

//! Peridot MMD(Polygon Model eXtended/Vocaloid(!?) Motion Data) Loader

extern crate encoding;

use std::fs::File;
use std::io::{Read, BufReader};
use std::path::{Path, PathBuf};

pub mod pmx;
pub mod vmd;
pub use vmd::MotionData;

pub struct PolygonModelExtended {
    pub base_components: Vec<String>,
    pub header: pmx::Header, pub vertices: Vec<pmx::Vertex>, pub surfaces: pmx::SurfaceSection,
    pub textures: Vec<PathBuf>, pub materials: Vec<pmx::Material>, pub bones: Vec<pmx::Bone>,
    pub morphs: Vec<pmx::Morph>, pub display_frames: Vec<pmx::DisplayFrame>, rigid_bodies: Vec<pmx::RigidBody>,
    joints: Vec<pmx::Joint>, softbodies: Vec<pmx::Softbody>
}
impl PolygonModelExtended
{
    pub fn from_file<P: AsRef<Path> + ?Sized>(filepath: &P) -> Result<Self, pmx::LoadingError>
    {
        let mut base_components = filepath.as_ref().components()
            .map(|x| x.as_os_str().to_string_lossy().into_owned()).collect::<Vec<_>>();
        base_components.pop();
        File::open(filepath).map(BufReader::new).map_err(From::from).and_then(|r| Self::load(base_components, r))
    }
    pub fn load<R: Read>(base_components: Vec<String>, mut reader: R) -> Result<Self, pmx::LoadingError>
    {
        let header = pmx::Header::load(&mut reader)?;
        let vertices = pmx::read_array(&mut reader,
            |r| pmx::Vertex::read(r, header.additional_vec4_count as _, header.index_sizes.bone))?;
        let surfaces = pmx::SurfaceSection::read(&mut reader, header.index_sizes.vertex)?;
        let textures = pmx::read_array(&mut reader,
            |r| header.string_reader.read_string(r).map(PathBuf::from).map_err(From::from))?;
        let materials = pmx::read_array(&mut reader, |r| pmx::Material::read(r, &header))?;
        let bones = pmx::read_array(&mut reader, |r| pmx::Bone::read(r, &header))?;
        let morphs = pmx::read_array(&mut reader, |r| pmx::Morph::read(r, &header))?;
        let display_frames = pmx::read_array(&mut reader, |r| pmx::DisplayFrame::read(r, &header))?;
        let rigid_bodies = pmx::read_array(&mut reader, |r| pmx::RigidBody::read(r, &header))?;
        let joints = pmx::read_array(&mut reader, |r| pmx::Joint::read(r, &header))?;
        let softbodies = if header.version > 2.0 {
            pmx::read_array(&mut reader, |r| pmx::Softbody::read(r, &header))?
        }
        else { Vec::new() };

        Ok(PolygonModelExtended
        {
            base_components,
            header, vertices, surfaces, textures, materials, bones, morphs, display_frames, rigid_bodies,
            joints, softbodies
        })
    }
}
/// Exports
impl PolygonModelExtended {
    /// Name of the model, in Japanese.
    pub fn name_jp(&self) -> &str { &self.header.model_name.jp }
    /// Name of the model
    pub fn name(&self) -> &str { &self.header.model_name.univ }
}
