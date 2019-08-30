
use euclid::Rect;
use peridot_math::Vector2;
use lyon_path::builder::PathBuilder;

#[cfg(target_os = "macos")] use appkit::ObjcObjectBase;
#[cfg(target_os = "windows")] mod dwrite_driver;
#[cfg(target_os = "windows")] use self::dwrite_driver::*;

pub struct GlyphBound<T> { left: T, top: T, right: T, bottom: T }
impl<T: Copy> GlyphBound<T>
{
    pub fn offset(&self) -> Vector2<T> { Vector2(self.left, self.top) }
    pub fn size(&self) -> Vector2<T> where T: std::ops::Sub<Output = T>
    {
        Vector2(self.right - self.left, self.bottom - self.top)
    }
}
#[repr(C)] #[derive(Clone, Debug)]
pub(crate) struct GlyphTransform { pub st: [f32; 4], pub ext: [f32; 2], pub pad: [f32; 2] }

pub struct FontProperties
{
    pub italic: bool, pub weight: u16
}
impl Default for FontProperties
{
    fn default() -> Self
    {
        FontProperties { italic: false, weight: 400 }
    }
}
#[cfg(target_os = "macos")]
impl FontProperties
{
    pub fn native_weight(&self) -> f32 { 2.0 * self.weight as f32 / 1000.0 - 1.0 }
}

#[derive(Debug)]
pub enum FontConstructionError { SysAPICallError(&'static str), IO(std::io::Error) }
impl From<std::io::Error> for FontConstructionError
{
    fn from(v: std::io::Error) -> Self { FontConstructionError::IO(v) }
}
#[derive(Debug)]
pub enum GlyphLoadingError { SysAPICallError(&'static str), IO(std::io::Error) }
impl From<std::io::Error> for GlyphLoadingError
{
    fn from(v: std::io::Error) -> Self { GlyphLoadingError::IO(v) }
}
#[cfg(target_os = "macos")] type UnderlyingHandle = appkit::ExternalRc<appkit::CTFont>;
#[cfg(target_os = "windows")] type UnderlyingHandle = comdrive::dwrite::FontFace;
pub struct Font(UnderlyingHandle, f32);
#[cfg(target_os = "macos")]
impl Font
{
    pub fn best_match(family_name: &str, properties: &FontProperties, size: f32)
        -> Result<Self, FontConstructionError>
    {
        let traits = appkit::NSMutableDictionary::with_capacity(2)
            .map_err(|_| FontConstructionError::SysAPICallError("NSMutableDictionary::with_capacity"))?;
        let weight_num = appkit::NSNumber::from_float(properties.native_weight())
            .map_err(|_| FontConstructionError::SysAPICallError("NSNumber::from_float"))?;
        let symbolic_traits = appkit::NSNumber::from_uint(
            if properties.italic { appkit::CTFontSymbolicTraits::ItalicTrait as u32 } else { 0u32 } as _)
            .map_err(|_| FontConstructionError::SysAPICallError("NSNumber::from_uint"))?;
        traits.set(AsRef::as_ref(unsafe { &*appkit::kCTFontWeightTrait }), weight_num.objid());
        traits.set(AsRef::as_ref(unsafe { &*appkit::kCTFontSymbolicTrait }), symbolic_traits.objid());
        let attrs = appkit::NSMutableDictionary::with_capacity(2)
            .map_err(|_| FontConstructionError::SysAPICallError("NSMutableDictionary::with_capacity"))?;
        let family_name_nsstr = appkit::NSString::from_str(family_name)
            .map_err(|_| FontConstructionError::SysAPICallError("NSString::from_str"))?;
        attrs.set(AsRef::as_ref(unsafe { &*appkit::kCTFontFamilyNameAttribute }), family_name_nsstr.objid());
        attrs.set(AsRef::as_ref(unsafe { &*appkit::kCTFontTraitsAttribute }), traits.objid());

        let fd = appkit::CTFontDescriptor::with_attributes(AsRef::as_ref(&**attrs))
            .map_err(|_| FontConstructionError::SysAPICallError("CTFontDescriptor::with_attributes"))?;
        appkit::CTFont::from_font_descriptor(&fd, size as _, None)
            .map_err(|_| FontConstructionError::SysAPICallError("CTFont::from_font_descriptor"))
            .map(|x| Font(x, size))
    }
    pub fn set_em_size(&mut self, size: f32) { self.1 = size; }
    pub(crate) fn scale_value(&self) -> f32 { self.1 / self.units_per_em() as f32 }
    /// Returns a scaled ascent metric value
    pub fn ascent(&self) -> f32 { self.0.ascent() as _ }

    pub(crate) fn glyph_id(&self, c: char) -> Option<u32>
    {
        let mut u16s = [0u16; 2];
        c.encode_utf16(&mut u16s);
        // remove surrogate paired codepoint
        self.0.glyphs_for_characters(&u16s[..1]).ok().map(|x| x[0] as _)
    }
    pub(crate) fn advance_h(&self, glyph: u32) -> Result<f32, GlyphLoadingError>
    {
        Ok(self.0.advances_for_glyphs(appkit::CTFontOrientation::Horizontal, &[glyph as _], None) as _)
    }
    pub(crate) fn bounds(&self, glyph: u32) -> Result<Rect<f32>, GlyphLoadingError>
    {
        let r = self.0.bounding_rects_for_glyphs(appkit::CTFontOrientation::Horizontal, &[glyph as _], None);
        Ok(Rect::new(euclid::point2(r.origin.x as _, r.origin.y as _),
            euclid::size2(r.size.width as _, r.size.height as _)))
    }
    pub(crate) fn outline<B: PathBuilder>(&self, glyph: u32, builder: &mut B) -> Result<(), GlyphLoadingError>
    {
        let path = self.0.create_path_for_glyph(glyph as _, None)
            .map_err(|_| GlyphLoadingError::SysAPICallError("CTFont::create_path_for_glyph"))?;
        path.apply(|e| match e.type_
        {
            appkit::CGPathElementType::MoveToPoint => unsafe
            {
                builder.move_to(euclid::point2((*e.points).x as _, (*e.points).y as _));
            },
            appkit::CGPathElementType::CloseSubpath => builder.close(),
            appkit::CGPathElementType::AddLineToPoint => unsafe
            {
                builder.line_to(euclid::point2((*e.points).x as _, (*e.points).y as _));
            },
            appkit::CGPathElementType::AddCurveToPoint => unsafe
            {
                let points = std::slice::from_raw_parts(e.points, 3);
                builder.cubic_bezier_to(
                    euclid::point2(points[0].x as _, points[0].y as _),
                    euclid::point2(points[1].x as _, points[1].y as _),
                    euclid::point2(points[2].x as _, points[2].y as _));
            },
            appkit::CGPathElementType::AddQuadCurveToPoint => unsafe
            {
                let points = std::slice::from_raw_parts(e.points, 2);
                builder.quadratic_bezier_to(
                    euclid::point2(points[0].x as _, points[0].y as _),
                    euclid::point2(points[1].x as _, points[1].y as _));
            }
        });

        Ok(())
    }

    pub fn units_per_em(&self) -> u32 { self.0.units_per_em() }
}

#[cfg(target_os = "windows")]
impl Font
{
    pub fn best_match(family_name: &str, properties: &FontProperties, size: f32)
        -> Result<Self, FontConstructionError>
    {
        let factory = comdrive::dwrite::Factory::new()?;
        let collection = factory.system_font_collection(false)?;
        let family_index = collection.find_family_name(family_name)?.unwrap_or(0);
        let family = collection.font_family(family_index)?;
        let font_style = if properties.italic
        {
            comdrive::dwrite::FontStyle::Italic
        }
        else
        {
            comdrive::dwrite::FontStyle::None
        };
        let font = family.first_matching_font(properties.weight as _, comdrive::dwrite::FONT_STRETCH_NORMAL,
            font_style)?;
        
        font.new_font_face().map(|x| Font(x, size)).map_err(From::from)
    }
    pub fn set_em_size(&mut self, size: f32) { self.1 = size; }
    fn scale_value(&self) -> f32 { self.1 / self.units_per_em() as f32 }
    /// Returns a scaled ascent metric value
    pub fn ascent(&self) -> f32 { self.0.metrics().ascent as f32 * self.scale_value() }

    pub(crate) fn glyph_id(&self, c: char) -> Option<u32>
    {
        self.0.glyph_indices(&[c]).ok().map(|x| x[0] as _)
    }
    pub(crate) fn advance_h(&self, glyph: u32) -> Result<f32, GlyphLoadingError>
    {
        self.0.design_glyph_metrics(&[glyph as _], false)
            .map(|m| self.scale_value() * m[0].advanceWidth as f32).map_err(From::from)
    }
    pub(crate) fn bounds(&self, glyph: u32) -> Result<Rect<f32>, GlyphLoadingError>
    {
        let m = self.0.design_glyph_metrics(&[glyph as _], false)?[0];
        
        Ok(Rect::new(euclid::point2(m.leftSideBearing as _, m.topSideBearing as _), euclid::size2(
            (m.leftSideBearing + m.rightSideBearing + m.advanceWidth as i32) as f32,
            (m.topSideBearing + m.bottomSideBearing + m.advanceHeight as i32) as f32,
        )))
    }
    pub(crate) fn outline<B: PathBuilder>(&self, glyph: u32, builder: &mut B) -> Result<(), GlyphLoadingError>
    {
        let sink = comdrive::ComPtr(PathEventReceiver::new());
        self.0.sink_glyph_run_outline(self.1 as _, &[glyph as _], None, None, false, false, unsafe { &mut *sink.0 })?;
        for pe in unsafe { sink.0.as_mut().expect("null sink").drain_all_paths() }
        {
            builder.path_event(pe);
        }

        Ok(())
    }

    pub fn units_per_em(&self) -> u32 { self.0.metrics().designUnitsPerEm as _ }
}
