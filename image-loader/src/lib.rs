
use std::io::prelude::Read;

pub struct ImagePng<R: Read>(image::png::PNGDecoder<R>);
