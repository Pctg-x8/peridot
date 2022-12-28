use euclid::Rect;
use lyon_path::builder::PathBuilder;
use peridot::math::Vector2;

#[cfg(all(target_os = "macos", not(feature = "use-freetype")))]
use objc_ext::ObjcObject;
#[cfg(all(target_os = "windows", not(feature = "use-freetype")))]
use windows::Win32::Graphics::DirectWrite::{
    DWriteCreateFactory, IDWriteFactory, IDWriteFontFace, DWRITE_FACTORY_TYPE_SHARED,
};
#[cfg(all(target_os = "windows", not(feature = "use-freetype")))]
use windows::Win32::Graphics::DirectWrite::{
    DWRITE_FONT_SIMULATIONS_NONE, DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STYLE_ITALIC,
    DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_WEIGHT,
};
#[cfg(all(target_os = "windows", not(feature = "use-freetype")))]
mod dwrite_driver;
#[cfg(all(target_os = "windows", not(feature = "use-freetype")))]
use self::dwrite_driver::*;
#[cfg(feature = "use-fontconfig")]
mod fc_drivers;
#[cfg(feature = "use-freetype")]
mod ft_drivers;

pub struct GlyphBound<T> {
    left: T,
    top: T,
    right: T,
    bottom: T,
}
impl<T: Copy> GlyphBound<T> {
    pub fn offset(&self) -> Vector2<T> {
        Vector2(self.left, self.top)
    }
    pub fn size(&self) -> Vector2<T>
    where
        T: std::ops::Sub<Output = T>,
    {
        Vector2(self.right - self.left, self.bottom - self.top)
    }
}
#[repr(C)]
#[derive(Clone, Debug)]
pub(crate) struct GlyphTransform {
    pub st: [f32; 4],
    pub ext: [f32; 2],
    pub pad: [f32; 2],
}

pub struct FontProperties {
    pub italic: bool,
    pub weight: u16,
}
impl Default for FontProperties {
    fn default() -> Self {
        FontProperties {
            italic: false,
            weight: 400,
        }
    }
}
#[cfg(target_os = "macos")]
impl FontProperties {
    pub fn native_weight(&self) -> f32 {
        2.0 * self.weight as f32 / 1000.0 - 1.0
    }
}

#[derive(Debug)]
pub enum FontConstructionError {
    SysAPICallError(&'static str),
    MatcherUnavailable,
    IO(std::io::Error),
    UnsupportedFontFile,
    #[cfg(feature = "use-freetype")]
    FT2(freetype2::FT_Error),
    #[cfg(target_os = "windows")]
    WindowsSysError(windows::core::Error),
}
impl From<std::io::Error> for FontConstructionError {
    fn from(v: std::io::Error) -> Self {
        Self::IO(v)
    }
}
#[cfg(target_os = "windows")]
impl From<windows::core::Error> for FontConstructionError {
    fn from(value: windows::core::Error) -> Self {
        Self::WindowsSysError(value)
    }
}

#[derive(Debug)]
pub enum GlyphLoadingError {
    SysAPICallError(&'static str),
    IO(std::io::Error),
    #[cfg(feature = "use-freetype")]
    FT2(freetype2::FT_Error),
    #[cfg(target_os = "windows")]
    WindowsSysError(windows::core::Error),
}
impl From<std::io::Error> for GlyphLoadingError {
    fn from(v: std::io::Error) -> Self {
        Self::IO(v)
    }
}
#[cfg(feature = "use-freetype")]
impl From<freetype2::FT_Error> for GlyphLoadingError {
    fn from(v: freetype2::FT_Error) -> Self {
        Self::FT2(v)
    }
}
#[cfg(target_os = "windows")]
impl From<windows::core::Error> for GlyphLoadingError {
    fn from(value: windows::core::Error) -> Self {
        Self::WindowsSysError(value)
    }
}

pub struct FontProvider {
    #[cfg(target_os = "windows")]
    factory: IDWriteFactory,
    #[cfg(feature = "use-freetype")]
    ftlib: self::ft_drivers::System,
    #[cfg(feature = "use-fontconfig")]
    fc: self::fc_drivers::Config,
}
impl FontProvider {
    pub fn new() -> Result<Self, FontConstructionError> {
        Ok(FontProvider {
            #[cfg(target_os = "windows")]
            factory: unsafe { DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED)? },
            #[cfg(feature = "use-freetype")]
            ftlib: self::ft_drivers::System::new(),
            #[cfg(feature = "use-fontconfig")]
            fc: self::fc_drivers::Config::init(),
        })
    }
}

#[cfg(all(target_os = "macos", not(feature = "use-freetype")))]
type UnderlyingHandle = appkit::ExternalRc<appkit::CTFont>;
#[cfg(all(target_os = "windows", not(feature = "use-freetype")))]
type UnderlyingHandle = IDWriteFontFace;
#[cfg(feature = "use-freetype")]
type UnderlyingHandle = self::ft_drivers::FaceGroup;
pub struct Font(UnderlyingHandle, f32);
#[cfg(all(target_os = "macos", not(feature = "use-freetype")))]
impl FontProvider {
    const CTFONT_DEFAULT_SIZE: f32 = 12.0;

    pub fn best_match(
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
        appkit::CTFont::from_font_descriptor(&fd, Self::CTFONT_DEFAULT_SIZE as _, None)
            .map_err(|_| FontConstructionError::SysAPICallError("CTFont::from_font_descriptor"))
            .map(|x| Font(x, size))
    }

    pub fn load<NL: peridot::NativeLinker>(
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

        appkit::CTFont::from_font_descriptor(&fd, Self::CTFONT_DEFAULT_SIZE as _, None)
            .map_err(|_| FontConstructionError::SysAPICallError("CTFont::from_font_descriptor"))
            .map(|x| Font(x, size))
    }
}
#[cfg(all(target_os = "macos", not(feature = "use-freetype")))]
impl Font {
    pub fn set_em_size(&mut self, size: f32) {
        self.1 = size;
    }
    pub fn size(&self) -> f32 {
        self.1
    }
    pub fn scale_value(&self) -> f32 {
        self.1 / FontProvider::CTFONT_DEFAULT_SIZE
    }
    pub fn ascent(&self) -> f32 {
        self.0.ascent() as f32 * self.scale_value()
    }

    pub fn glyph_id(&self, c: char) -> Option<u32> {
        let mut u16s = [0u16; 2];
        c.encode_utf16(&mut u16s);
        // remove surrogate paired codepoint
        self.0
            .glyphs_for_characters(&u16s[..1])
            .ok()
            .map(|x| x[0] as _)
    }
    pub fn advance_h(&self, glyph: u32) -> Result<f32, GlyphLoadingError> {
        Ok(self
            .0
            .advances_for_glyphs(appkit::CTFontOrientation::Horizontal, &[glyph as _], None)
            as _)
    }
    pub fn bounds(&self, glyph: u32) -> Result<Rect<f32>, GlyphLoadingError> {
        let r = self.0.bounding_rects_for_glyphs(
            appkit::CTFontOrientation::Horizontal,
            &[glyph as _],
            None,
        );
        Ok(Rect::new(
            euclid::point2(
                r.origin.x as f32 * self.scale_value(),
                r.origin.y as f32 * self.scale_value(),
            ),
            euclid::size2(
                r.size.width as f32 * self.scale_value(),
                r.size.height as f32 * self.scale_value(),
            ),
        ))
    }
    pub fn outline<B: PathBuilder>(
        &self,
        glyph: u32,
        builder: &mut B,
    ) -> Result<(), GlyphLoadingError> {
        let path = self
            .0
            .create_path_for_glyph(glyph as _, None)
            .map_err(|_| GlyphLoadingError::SysAPICallError("CTFont::create_path_for_glyph"))?;
        path.apply(|e| match e.type_ {
            appkit::CGPathElementType::MoveToPoint => unsafe {
                builder.move_to(euclid::point2(
                    (*e.points).x as f32 * self.scale_value(),
                    (*e.points).y as f32 * self.scale_value() - self.ascent(),
                ));
            },
            appkit::CGPathElementType::CloseSubpath => builder.close(),
            appkit::CGPathElementType::AddLineToPoint => unsafe {
                builder.line_to(euclid::point2(
                    (*e.points).x as f32 * self.scale_value(),
                    (*e.points).y as f32 * self.scale_value() - self.ascent(),
                ));
            },
            appkit::CGPathElementType::AddCurveToPoint => unsafe {
                let points = std::slice::from_raw_parts(e.points, 3);
                builder.cubic_bezier_to(
                    euclid::point2(
                        points[0].x as f32 * self.scale_value(),
                        points[0].y as f32 * self.scale_value() - self.ascent(),
                    ),
                    euclid::point2(
                        points[1].x as f32 * self.scale_value(),
                        points[1].y as f32 * self.scale_value() - self.ascent(),
                    ),
                    euclid::point2(
                        points[2].x as f32 * self.scale_value(),
                        points[2].y as f32 * self.scale_value() - self.ascent(),
                    ),
                );
            },
            appkit::CGPathElementType::AddQuadCurveToPoint => unsafe {
                let points = std::slice::from_raw_parts(e.points, 2);
                builder.quadratic_bezier_to(
                    euclid::point2(
                        points[0].x as f32 * self.scale_value(),
                        points[0].y as f32 * self.scale_value() - self.ascent(),
                    ),
                    euclid::point2(
                        points[1].x as f32 * self.scale_value(),
                        points[1].y as f32 * self.scale_value() - self.ascent(),
                    ),
                );
            },
        });

        Ok(())
    }

    pub fn units_per_em(&self) -> u32 {
        self.0.units_per_em()
    }
}

#[cfg(all(target_os = "windows", not(feature = "use-freetype")))]
impl FontProvider {
    pub fn best_match(
        &self,
        family_name: &str,
        properties: &FontProperties,
        size: f32,
    ) -> Result<Font, FontConstructionError> {
        let mut collection = None;
        unsafe {
            self.factory
                .GetSystemFontCollection(&mut collection, false)?
        };
        let collection = collection.expect("no system font collection");
        let mut family_index = 0;
        let mut exists = Default::default();
        let family_name = widestring::WideCString::from_str(family_name).expect("invalid sequence");
        unsafe {
            collection.FindFamilyName(
                windows::core::PCWSTR(family_name.as_ptr()),
                &mut family_index,
                &mut exists,
            )?;
        }
        let family_index = if exists.as_bool() { family_index } else { 0 };
        let family = unsafe { collection.GetFontFamily(family_index)? };
        let font_style = if properties.italic {
            DWRITE_FONT_STYLE_ITALIC
        } else {
            DWRITE_FONT_STYLE_NORMAL
        };
        let font = unsafe {
            family.GetFirstMatchingFont(
                DWRITE_FONT_WEIGHT(properties.weight as _),
                DWRITE_FONT_STRETCH_NORMAL,
                font_style,
            )?
        };

        unsafe {
            font.CreateFontFace()
                .map(|x| Font(x, size))
                .map_err(From::from)
        }
    }

    pub fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Font, FontConstructionError> {
        let a: TTFBlob = e.load(asset_path)?;
        let conv = ATFRegisterScope::register(&self.factory, AssetToFontConverter::new(a))?;
        let fntfile = unsafe {
            self.factory.CreateCustomFontFileReference(
                &1u32 as *const u32 as _,
                std::mem::size_of::<u32>() as _,
                conv.object(),
            )?
        };
        let (mut is_supported, mut file_type, mut face_type, mut face_count) = Default::default();
        unsafe {
            fntfile.Analyze(
                &mut is_supported,
                &mut file_type,
                Some(&mut face_type),
                &mut face_count,
            )?
        };
        if !is_supported.as_bool() {
            return Err(FontConstructionError::UnsupportedFontFile);
        }

        unsafe {
            self.factory
                .CreateFontFace(face_type, &[Some(fntfile)], 0, DWRITE_FONT_SIMULATIONS_NONE)
                .map(|x| Font(x, size))
                .map_err(From::from)
        }
    }
}
#[cfg(all(target_os = "windows", not(feature = "use-freetype")))]
impl Font {
    pub fn set_em_size(&mut self, size: f32) {
        self.1 = size;
    }
    pub fn size(&self) -> f32 {
        self.1
    }
    pub fn scale_value(&self) -> f32 {
        (96.0 * self.1 / 72.0) / self.units_per_em() as f32
    }
    /// Returns a scaled ascent metric value
    pub fn ascent(&self) -> f32 {
        let mut fm = std::mem::MaybeUninit::uninit();
        unsafe {
            self.0.GetMetrics(fm.as_mut_ptr());
            fm.assume_init_ref().ascent as _
        }
    }

    pub fn glyph_id(&self, c: char) -> Option<u32> {
        unsafe { self.0.GetGlyphIndices(&(c as u32), 1).ok().map(|x| x as _) }
    }
    pub fn advance_h(&self, glyph: u32) -> Result<f32, GlyphLoadingError> {
        let mut gm = std::mem::MaybeUninit::uninit();
        unsafe {
            self.0
                .GetDesignGlyphMetrics(&(glyph as u16), 1, gm.as_mut_ptr(), false)?;
            Ok(gm.assume_init_ref().advanceWidth as _)
        }
    }
    /// in dip
    pub fn bounds(&self, glyph: u32) -> Result<Rect<f32>, GlyphLoadingError> {
        let mut gm = std::mem::MaybeUninit::uninit();
        let gm = unsafe {
            self.0
                .GetDesignGlyphMetrics(&(glyph as u16), 1, gm.as_mut_ptr(), false)?;
            gm.assume_init()
        };

        Ok(Rect::new(
            euclid::point2(
                gm.leftSideBearing as f32 * self.scale_value(),
                (gm.verticalOriginY - gm.topSideBearing) as f32 * self.scale_value(),
            ),
            euclid::size2(
                (gm.leftSideBearing + gm.rightSideBearing + gm.advanceWidth as i32) as f32
                    * self.scale_value(),
                (gm.topSideBearing + gm.bottomSideBearing + gm.advanceHeight as i32) as f32
                    * self.scale_value(),
            ),
        ))
    }
    pub fn outline<B: PathBuilder>(
        &self,
        glyph: u32,
        builder: &mut B,
    ) -> Result<(), GlyphLoadingError> {
        let mut sink = PathEventReceiver::new();

        unsafe {
            self.0.GetGlyphRunOutline(
                96.0 * self.1 as f32 / 72.0,
                &(glyph as _),
                None,
                None,
                1,
                false,
                false,
                &sink,
            )?
        }
        for pe in sink.drain_all_paths() {
            builder.path_event(pe);
        }

        Ok(())
    }

    pub fn units_per_em(&self) -> u32 {
        let mut fm = std::mem::MaybeUninit::uninit();
        unsafe {
            self.0.GetMetrics(fm.as_mut_ptr());
            fm.assume_init_ref().designUnitsPerEm as _
        }
    }
}

#[cfg(feature = "use-freetype")]
impl FontProvider {
    #[cfg(feature = "use-fontconfig")]
    pub fn best_match(
        &self,
        family_name: &str,
        properties: &FontProperties,
        size: f32,
    ) -> Result<Font, FontConstructionError> {
        let c_family_name = std::ffi::CString::new(family_name).expect("FFI Conversion failure");
        let mut pat = fc_drivers::Pattern::with_name_weight_style_size(
            c_family_name.as_ptr() as *const _,
            properties.weight as _,
            properties.italic,
            size,
        )
        .ok_or(FontConstructionError::SysAPICallError("FcPatternBuild"))?;
        self.fc.substitute_pattern(&mut pat);
        pat.default_substitute();
        let fonts = self
            .fc
            .sort_fonts(&pat)
            .ok_or(FontConstructionError::SysAPICallError("FcFontSort"))?;

        let group_desc = fonts
            .iter()
            .map(|f| {
                let font_path = f
                    .get_filepath()
                    .ok_or(FontConstructionError::SysAPICallError("FcPatternGetString"))?;
                let face_index =
                    f.get_face_index()
                        .ok_or(FontConstructionError::SysAPICallError(
                            "FcPatternGetInteger",
                        ))?;

                Ok(ft_drivers::FaceGroupEntry::unloaded(
                    font_path,
                    face_index as _,
                ))
            })
            .collect::<Result<_, FontConstructionError>>()?;
        let face = self.ftlib.new_face_group(group_desc);
        face.set_size(size);

        Ok(Font(face, size))
    }
    #[cfg(not(feature = "use-fontconfig"))]
    pub fn best_match(
        &self,
        _: &str,
        _: &FontProperties,
        _: f32,
    ) -> Result<Font, FontConstructionError> {
        // no matching algorithm is available!

        Err(FontConstructionError::MatcherUnavailable)
    }

    pub fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Font, FontConstructionError> {
        let a: TTFBlob = e.load(asset_path)?;
        let f = self
            .ftlib
            .new_face_from_mem(&a.0, 0)
            .map_err(FontConstructionError::FT2)?;
        let face = self
            .ftlib
            .new_face_group(vec![ft_drivers::FaceGroupEntry::LoadedMem(f, a.0.into())]);
        face.set_size(size);

        Ok(Font(face, size))
    }
}
#[cfg(feature = "use-freetype")]
impl Font {
    pub fn set_em_size(&mut self, size: f32) {
        self.1 = size;
        self.0.set_size(size);
    }
    pub fn size(&self) -> f32 {
        self.1
    }
    pub fn scale_value(&self) -> f32 {
        self.1 / self.units_per_em() as f32
    }
    pub fn ascent(&self) -> f32 {
        self.0.ascender() as _
    }

    pub fn glyph_id(&self, c: char) -> Option<(usize, u32)> {
        self.0.char_index(c)
    }
    pub fn advance_h(&self, glyph: (usize, u32)) -> Result<f32, GlyphLoadingError> {
        self.0.get(glyph.0).load_glyph(glyph.1)?;

        Ok(self.0.get(glyph.0).glyph_advance().x as f32 / 64.0)
    }
    pub fn bounds(&self, glyph: (usize, u32)) -> Result<Rect<f32>, GlyphLoadingError> {
        let fnt = self.0.get(glyph.0);
        fnt.load_glyph(glyph.1)?;
        let m = fnt.glyph_metrics();

        Ok(Rect::new(
            euclid::point2(m.horiBearingX as f32 / 64.0, m.horiBearingY as f32 / 64.0),
            euclid::size2(m.width as f32 / 64.0, m.height as f32 / 64.0),
        ))
    }
    pub fn outline<B: PathBuilder>(
        &self,
        glyph: (usize, u32),
        builder: &mut B,
    ) -> Result<(), GlyphLoadingError> {
        self.0.get(glyph.0).load_glyph(glyph.1)?;
        self.0.get(glyph.0).decompose_outline(builder);

        Ok(())
    }

    pub fn units_per_em(&self) -> u32 {
        self.0.units_per_em() as _
    }
}

/// An asset represents ttf blob
pub struct TTFBlob(pub(crate) Vec<u8>);
impl peridot::LogicalAssetData for TTFBlob {
    const EXT: &'static str = "ttf";
}
impl peridot::FromAsset for TTFBlob {
    type Error = std::io::Error;

    fn from_asset<Asset: std::io::Read + std::io::Seek + 'static>(
        mut asset: Asset,
    ) -> Result<Self, Self::Error> {
        let mut bin = Vec::new();
        asset.read_to_end(&mut bin).map(move |_| TTFBlob(bin))
    }
}
