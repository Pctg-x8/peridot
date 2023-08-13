//! CoreText Font Provider impl

use crate::{Font, FontConstructionError, FontProperties, FontProvider};

const CTFONT_DEFAULT_SIZE: f32 = 12.0;

pub struct CoreTextFontProvider;
impl FontProvider for CoreTextFontProvider {
    fn new() -> Result<Self, FontConstructionError> {
        Ok(Self)
    }

    fn best_match(
        &self,
        family_name: &str,
        properties: &FontProperties,
        size: f32,
    ) -> Result<Font, FontConstructionError> {
        let traits = appkit::NSMutableDictionary::with_capacity(2).map_err(|_| {
            FontConstructionError::SysAPICallError("NSMutableDictionary::with_capacity")
        })?;
        let weight_num = appkit::NSNumber::from_float(properties.native_weight())
            .map_err(|_| FontConstructionError::SysAPICallError("NSNumber::from_float"))?;
        let symbolic_traits = appkit::NSNumber::from_uint(if properties.italic {
            appkit::CTFontSymbolicTraits::ItalicTrait as u32
        } else {
            0u32
        } as _)
        .map_err(|_| FontConstructionError::SysAPICallError("NSNumber::from_uint"))?;
        traits.set(
            AsRef::as_ref(unsafe { &*appkit::kCTFontWeightTrait }),
            weight_num.as_id(),
        );
        traits.set(
            AsRef::as_ref(unsafe { &*appkit::kCTFontSymbolicTrait }),
            symbolic_traits.as_id(),
        );
        let attrs = appkit::NSMutableDictionary::with_capacity(2).map_err(|_| {
            FontConstructionError::SysAPICallError("NSMutableDictionary::with_capacity")
        })?;
        let family_name_nsstr = appkit::NSString::from_str(family_name)
            .map_err(|_| FontConstructionError::SysAPICallError("NSString::from_str"))?;
        attrs.set(
            AsRef::as_ref(unsafe { &*appkit::kCTFontFamilyNameAttribute }),
            family_name_nsstr.as_id(),
        );
        attrs.set(
            AsRef::as_ref(unsafe { &*appkit::kCTFontTraitsAttribute }),
            traits.as_id(),
        );

        let fd =
            appkit::CTFontDescriptor::with_attributes(AsRef::as_ref(&**attrs)).map_err(|_| {
                FontConstructionError::SysAPICallError("CTFontDescriptor::with_attributes")
            })?;
        appkit::CTFont::from_font_descriptor(&fd, CTFONT_DEFAULT_SIZE as _, None)
            .map_err(|_| FontConstructionError::SysAPICallError("CTFont::from_font_descriptor"))
            .map(|x| Font(x, size))
    }

    fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Font, FontConstructionError> {
        let a: TTFBlob = e.load(asset_path)?;
        let d = appkit::CFData::new(&a.0)
            .ok_or(FontConstructionError::SysAPICallError("CFData::new"))?;
        let fd = appkit::CTFontDescriptor::from_data(&d).ok_or(
            FontConstructionError::SysAPICallError("CTFontDescriptor::from_data"),
        )?;

        appkit::CTFont::from_font_descriptor(&fd, CTFONT_DEFAULT_SIZE as _, None)
            .map_err(|_| FontConstructionError::SysAPICallError("CTFont::from_font_descriptor"))
            .map(|x| Font(x, size))
    }
}
