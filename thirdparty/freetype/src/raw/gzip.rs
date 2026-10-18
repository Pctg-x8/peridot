use super::*;

unsafe extern "system" {
    pub fn FT_Stream_OpenGzip(stream: FT_Stream, source: FT_Stream) -> FT_Error;
    pub fn FT_Gzip_Uncompress(
        memory: FT_Memory,
        output: *mut FT_Byte,
        output_len: *mut FT_ULong,
        input: *const FT_Byte,
        input_len: FT_ULong,
    ) -> FT_Error;
}
