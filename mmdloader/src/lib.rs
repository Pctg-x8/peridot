#![warn(clippy::all)]

//! Peridot MMD(Polygon Model eXtended/Vocaloid(!?) Motion Data) Loader

extern crate encoding;

use std::fs::File;
use std::io::{Read, BufReader};
use std::env::args;
use std::path::{Path, PathBuf};
#[macro_use] extern crate log;

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
impl PolygonModelExtended {
    pub fn from_file<P: AsRef<Path> + ?Sized>(filepath: &P) -> Result<Self, pmx::LoadingError> {
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

fn main() {
    let filename = args().nth(1).map(PathBuf::from).expect("Filename");
    println!("Loading {}...", filename.display());
    // let mut vmdfile = File::open(filename).unwrap();
    // let vmd = vmd::MotionData::read(&mut vmdfile).unwrap();
    // println!("VMD Name: {:?}", vmd.name);
    let model = PolygonModelExtended::from_file(&filename).unwrap();
    println!("<<Polygon Model Extended v{}>>", model.header.version);
    println!("{}/{}", model.header.model_name.jp, model.header.model_name.univ);
    println!("*** Comments ***");
    println!("{}", model.header.comment.jp);
    println!("Vertices:");
    println!("{:?}", model.vertices);
    println!("SurfaceSection:");
    println!("{:?}", model.surfaces);
    println!("Textures:");
    for t in &model.textures { println!("* {}", t.display()); }
    println!("Materials:");
    for m in &model.materials {
        let mut flags_displayed = Vec::new();
        if (m.drawing_flags & 0x01) != 0 { flags_displayed.push("No-cull"); }
        if (m.drawing_flags & 0x02) != 0 { flags_displayed.push("GroundShadow"); }
        if (m.drawing_flags & 0x04) != 0 { flags_displayed.push("DrawShadow"); }
        if (m.drawing_flags & 0x08) != 0 { flags_displayed.push("ReceiveShadow"); }
        if (m.drawing_flags & 0x10) != 0 { flags_displayed.push("HasEdge"); }
        if (m.drawing_flags & 0x20) != 0 { flags_displayed.push("VertexColor"); }
        if (m.drawing_flags & 0x40) != 0 { flags_displayed.push("PointDrawing"); }
        if (m.drawing_flags & 0x80) != 0 { flags_displayed.push("LineDrawing"); }

        println!("* {}/{} [{}]", m.name.jp, m.name.univ, flags_displayed.join("/"));
        println!("  diffuse={:?} specular={:?}(str={})", m.diffuse_color, m.specular_color, m.specular_strength);
        println!("  ambient={:?}", m.ambient_color);
        if let Some(ref t) = m.texture_index {
            println!("  texture={}({})", t.as_index(), model.textures[t.as_index() as usize].display());
        }
        if let Some(ref t) = m.envmap_texture_index {
            println!("  envmap={}({}) blendmode={:?}", t.as_index(), model.textures[t.as_index() as usize].display(), m.envmap_blend_mode);
        }
        println!("  EdgeRendering: color={:?} scale={}", m.edge_color, m.edge_scale);
        match m.toon_reference {
            pmx::ToonReference::Internal(n) => println!("  ToonRef: Internal index={}", n),
            pmx::ToonReference::Texture(Some(ref t)) =>
                println!("  ToonRef: Texture #{}({})", t.as_index(), model.textures[t.as_index() as usize].display()),
            pmx::ToonReference::Texture(None) => println!("  ToonRef: None")
        }
        println!("  surface affection count={}", m.surface_affects);
        println!("  metadata: {}", m.meta);
    }
    println!("Bones: ");
    for (n, b) in model.bones.iter().enumerate() {
        println!("* {}: {}/{}", n, b.name.jp, b.name.univ);
        if let Some(ref ik) = b.ik {
            if let Some(ref v) = ik.target {
                println!("  BoneIK: Target={} loopcount={} limit_radian={}", v.as_index(), ik.loopcount, ik.limit_radian);
            }
            else {
                println!("  BoneIK: [NoTarget] loopcount={} limit_radian={}", ik.loopcount, ik.limit_radian);
            }
        }
    }
    println!("Morphs: ");
    for (n, m) in model.morphs.iter().enumerate() {
        println!("* {}: {}/{}", n, m.name.jp, m.name.univ);
    }
    println!("Display-Frames: ");
    for df in &model.display_frames {
        println!("* {}/{}", df.name.jp, df.name.univ);
        println!("  Frames: {}", df.frames.iter().map(|x| format!("{:?}", x)).collect::<Vec<_>>().join(" "));
    }
    /*println!("Rigid Bodies: ");
    for rb in &model.rigid_bodies {
        println!("* {}/{}", rb.name.jp, rb.name.univ);
    }
    println!("Joints: ");
    for j in &model.joints {
        println!("* {}/{}", j.name.jp, j.name.univ);
    }
    println!("SoftBodies: ");
    for sb in &model.softbodies {
        println!("* {}/{}", sb.name.jp, sb.name.univ);
    }*/
    /*println!("Loaded Model: {} (PMX Version {})", model.header.model_name.jp, model.header.version);
    println!("---Comments---");
    println!("{}", model.header.comment.jp);*/
}
