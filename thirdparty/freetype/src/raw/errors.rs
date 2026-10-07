use super::*;

pub const FT_Err_Ok: FT_Error = 0;
pub const FT_Err_Cannot_Open_Resource: FT_Error = 0x01;
pub const FT_Err_Unknown_File_Format: FT_Error = 0x02;
pub const FT_Err_Invalid_File_Format: FT_Error = 0x03;
pub const FT_Err_Invalid_Version: FT_Error = 0x04;
pub const FT_Err_Lower_Module_Version: FT_Error = 0x05;
pub const FT_Err_Invalid_Argument: FT_Error = 0x06;
pub const FT_Err_Unimplemented_Feature: FT_Error = 0x07;
pub const FT_Err_Invalid_Table: FT_Error = 0x08;
pub const FT_Err_Invalid_Offset: FT_Error = 0x09;
pub const FT_Err_Array_Too_Large: FT_Error = 0x0A;
pub const FT_Err_Missing_Module: FT_Error = 0x0B;
pub const FT_Err_Missing_Property: FT_Error = 0x0C;

pub const FT_Err_Invalid_Glyph_Index: FT_Error = 0x10;
pub const FT_Err_Invalid_Character_Code: FT_Error = 0x11;
pub const FT_Err_Invalid_Glyph_Format: FT_Error = 0x12;
pub const FT_Err_Cannot_Render_Glyph: FT_Error = 0x13;
pub const FT_Err_Invalid_Outline: FT_Error = 0x14;
pub const FT_Err_Invalid_Composite: FT_Error = 0x15;
pub const FT_Err_Too_Many_Hints: FT_Error = 0x16;
pub const FT_Err_Invalid_Pixel_Size: FT_Error = 0x17;

pub const FT_Err_Invalid_Handle: FT_Error = 0x20;
pub const FT_Err_Invalid_Library_Handle: FT_Error = 0x21;
pub const FT_Err_Invalid_Driver_Handle: FT_Error = 0x22;
pub const FT_Err_Invalid_Face_Handle: FT_Error = 0x23;
pub const FT_Err_Invalid_Size_Handle: FT_Error = 0x24;
pub const FT_Err_Invalid_Slot_Handle: FT_Error = 0x25;
pub const FT_Err_Invalid_CharMap_Handle: FT_Error = 0x26;
pub const FT_Err_Invalid_Cache_Handle: FT_Error = 0x27;
pub const FT_Err_Invalid_Stream_Handle: FT_Error = 0x28;

pub const FT_Err_Too_Many_Drivers: FT_Error = 0x30;
pub const FT_Err_Too_Many_Extensions: FT_Error = 0x31;

pub const FT_Err_Out_Of_Memory: FT_Error = 0x40;
pub const FT_Err_Unlisted_Object: FT_Error = 0x41;

pub const FT_Err_Cannot_Open_Stream: FT_Error = 0x51;
pub const FT_Err_Invalid_Stream_Seek: FT_Error = 0x52;
pub const FT_Err_Invalid_Stream_Skip: FT_Error = 0x53;
pub const FT_Err_Invalid_Stream_Read: FT_Error = 0x54;
pub const FT_Err_Invalid_Stream_Operation: FT_Error = 0x55;
pub const FT_Err_Invalid_Frame_Operation: FT_Error = 0x56;
pub const FT_Err_Nested_Frame_Access: FT_Error = 0x57;
pub const FT_Err_Invalid_Frame_Read: FT_Error = 0x58;

pub const FT_Err_Raster_Uninitialized: FT_Error = 0x60;
pub const FT_Err_Raster_Corrupted: FT_Error = 0x61;
pub const FT_Err_Raster_Overflow: FT_Error = 0x62;
pub const FT_Err_Raster_Negative_Height: FT_Error = 0x63;

pub const FT_Err_Too_Many_Caches: FT_Error = 0x70;

pub const FT_Err_Invalid_Opcode: FT_Error = 0x80;
pub const FT_Err_Too_Few_Arguments: FT_Error = 0x81;
pub const FT_Err_Stack_Overflow: FT_Error = 0x82;
pub const FT_Err_Code_Overflow: FT_Error = 0x83;
pub const FT_Err_Bad_Argument: FT_Error = 0x84;
pub const FT_Err_Divide_By_Zero: FT_Error = 0x85;
pub const FT_Err_Invalid_Reference: FT_Error = 0x86;
pub const FT_Err_Debug_OpCode: FT_Error = 0x87;
pub const FT_Err_ENDF_In_Exec_Stream: FT_Error = 0x88;
pub const FT_Err_Nested_DEFS: FT_Error = 0x89;
pub const FT_Err_Invalid_CodeRange: FT_Error = 0x8A;
pub const FT_Err_Execution_Too_Long: FT_Error = 0x8B;
pub const FT_Err_Too_Many_Function_Defs: FT_Error = 0x8C;
pub const FT_Err_Too_Many_Instruction_Defs: FT_Error = 0x8D;
pub const FT_Err_Table_Missing: FT_Error = 0x8E;
pub const FT_Err_Horiz_Header_Missing: FT_Error = 0x8F;
pub const FT_Err_Locations_Missing: FT_Error = 0x90;
pub const FT_Err_Name_Table_Missing: FT_Error = 0x91;
pub const FT_Err_CMap_Table_Missing: FT_Error = 0x92;
pub const FT_Err_Hmtx_Table_Missing: FT_Error = 0x93;
pub const FT_Err_Post_Table_Missing: FT_Error = 0x94;
pub const FT_Err_Invalid_Horiz_Metrics: FT_Error = 0x95;
pub const FT_Err_Invalid_CharMap_Format: FT_Error = 0x96;
pub const FT_Err_Invalid_PPem: FT_Error = 0x97;
pub const FT_Err_Invalid_Vert_Metrics: FT_Error = 0x98;
pub const FT_Err_Could_Not_Find_Context: FT_Error = 0x99;
pub const FT_Err_Invalid_Post_Table_Format: FT_Error = 0x9A;
pub const FT_Err_Invalid_Post_Table: FT_Error = 0x9B;
pub const FT_Err_DEF_In_Glyf_Bytecode: FT_Error = 0x9C;
pub const FT_Err_Missing_Bitmap: FT_Error = 0x9D;

pub const FT_Err_Syntax_Error: FT_Error = 0xA0;
pub const FT_Err_Stack_Underflow: FT_Error = 0xA1;
pub const FT_Err_Ignore: FT_Error = 0xA2;
pub const FT_Err_No_Unicode_Glyph_Name: FT_Error = 0xA3;
pub const FT_Err_Glyph_Too_Big: FT_Error = 0xA4;

pub const FT_Err_Missing_Startfont_Field: FT_Error = 0xB0;
pub const FT_Err_Missing_Font_Field: FT_Error = 0xB1;
pub const FT_Err_Missing_Size_Field: FT_Error = 0xB2;
pub const FT_Err_Missing_Fontboundingbox_Field: FT_Error = 0xB3;
pub const FT_Err_Missing_Chars_Field: FT_Error = 0xB4;
pub const FT_Err_Missing_Startchar_Field: FT_Error = 0xB5;
pub const FT_Err_Missing_Encoding_Field: FT_Error = 0xB6;
pub const FT_Err_Missing_Bbx_Field: FT_Error = 0xB7;
pub const FT_Err_Bbx_Too_Big: FT_Error = 0xB8;
pub const FT_Err_Corrupted_Font_Header: FT_Error = 0xB9;
pub const FT_Err_Corrupted_Font_Glyphs: FT_Error = 0xBA;

unsafe extern "system" {
    pub fn FT_Error_String(error_code: FT_Error) -> *const core::ffi::c_char;
}
