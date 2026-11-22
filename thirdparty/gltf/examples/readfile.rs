use std::{
    fs::File,
    io::{BufReader, Read},
};

use peridot_tp_gltf::{binary, json};

fn main() {
    let mut f = BufReader::new(
        File::open(std::env::args().nth(1).expect("input file")).expect("file.open"),
    );
    if !binary::try_verify_magic(&mut f) {
        panic!("cannot verify glb magic");
    }
    let hdr = binary::Header::read(&mut f).expect("glb.header.read");
    assert_eq!(hdr.version, 2);
    println!("hdr: {hdr:?}");

    let chunk0_hdr = binary::ChunkHeader::read(&mut f).expect("glb.chunk_header.read");
    assert_eq!(chunk0_hdr.r#type, binary::ChunkType::Json);
    println!("chunk0_hdr: {chunk0_hdr:?}");
    let mut content = Vec::<u8>::with_capacity(chunk0_hdr.length as _);
    f.read_exact(unsafe { core::mem::transmute(content.spare_capacity_mut()) })
        .expect("glb.chunk0.read");
    unsafe {
        content.set_len(chunk0_hdr.length as _);
    }
    let json_content: json::GLTF =
        serde_json::from_str(str::from_utf8(&content).expect("glb.chunk0.content.invalid"))
            .expect("glb.chunk0.parse");
    println!("{json_content:#?}");
}
