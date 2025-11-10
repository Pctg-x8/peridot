use crate::{CFDataRef, CTFontDescriptorRef};

unsafe extern "C" {
    pub fn CTFontManagerCreateFontDescriptorFromData(data: CFDataRef) -> CTFontDescriptorRef;
}
