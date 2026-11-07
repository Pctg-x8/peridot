use super::*;

unsafe extern "system" {
    pub fn FT_Stream_OpenBzip2(stream: FT_Stream, source: FT_Stream) -> FT_Error;
}
