use crate::FFIOpaqueStruct;

#[repr(C)]
pub struct __CFCharacterSet(FFIOpaqueStruct);
pub type CFCharacterSetRef = *const __CFCharacterSet;
pub type CFMutableCharacterSetRef = *mut __CFCharacterSet;
