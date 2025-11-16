use apple_sdk_port::foundation;
use apple_sdk_port::raw::core_graphics::{
    kCGPathElementAddCurveToPoint, kCGPathElementAddLineToPoint, kCGPathElementAddQuadCurveToPoint,
    kCGPathElementCloseSubpath, kCGPathElementMoveToPoint,
};
use apple_sdk_port::text as native_text;
use euclid::Rect;
use lyon_path::builder::PathBuilder;

use crate::{Font, GlyphLoadingError};

#[repr(transparent)]
pub struct CoreTextFont(pub(crate) apple_sdk_port::Owned<native_text::Font>);
impl Font for CoreTextFont {
    type GlyphID = core::num::NonZero<foundation::UniChar>;

    #[inline(always)]
    fn set_em_size(&mut self, size: f32) {
        self.0 = self
            .0
            .clone_with_attributes(size as _, None, None)
            .expect("Failed to create copy font");
    }

    #[inline(always)]
    fn size(&self) -> f32 {
        self.0.size() as _
    }

    #[inline(always)]
    fn ascent(&self) -> f32 {
        self.0.ascent() as _
    }

    #[inline(always)]
    fn units_per_em(&self) -> u32 {
        self.0.units_per_em() as _
    }

    fn glyph_id(&self, c: char) -> Option<Self::GlyphID> {
        let mut u16s = [0u16; 2];
        let u16s = c.encode_utf16(&mut u16s);

        self.0.glyph_for_character(if u16s.len() == 1 {
            u16s[0]
        } else {
            // remove surrogate paired codepoint
            u16s[1]
        })
    }

    fn advance_h(&self, glyph: &Self::GlyphID) -> Result<f32, GlyphLoadingError> {
        // TODO: 縦書き？
        Ok(self
            .0
            .advance_for_glyph(native_text::FontOrientation::Horizontal, glyph.get())
            .width as _)
    }

    fn bounds(&self, glyph: &Self::GlyphID) -> Result<Rect<f32>, GlyphLoadingError> {
        // TODO: 縦書き？
        let r = self
            .0
            .bounding_rect_for_glyph(native_text::FontOrientation::Horizontal, glyph.get());

        Ok(Rect::new(
            euclid::point2(r.origin.x as _, r.origin.y as _),
            euclid::size2(r.size.width as _, r.size.height as _),
        ))
    }

    fn outline<B: PathBuilder>(
        &self,
        glyph: &Self::GlyphID,
        transform: &euclid::Transform2D<f32>,
        builder: &mut B,
    ) -> Result<(), GlyphLoadingError> {
        self.0
            .create_path_for_glyph(glyph.get(), None)
            .ok_or(GlyphLoadingError::SysAPICallError(
                "CTFontCreatePathForGlyph",
            ))?
            .apply(|e| match e.r#type {
                #[allow(non_upper_case_globals)]
                kCGPathElementMoveToPoint => unsafe {
                    builder.move_to(
                        transform.transform_point(&euclid::point2(
                            (*e.points).x as _,
                            (*e.points).y as _,
                        )),
                    );
                },
                #[allow(non_upper_case_globals)]
                kCGPathElementCloseSubpath => builder.close(),
                #[allow(non_upper_case_globals)]
                kCGPathElementAddLineToPoint => unsafe {
                    builder.line_to(
                        transform.transform_point(&euclid::point2(
                            (*e.points).x as _,
                            (*e.points).y as _,
                        )),
                    );
                },
                #[allow(non_upper_case_globals)]
                kCGPathElementAddCurveToPoint => unsafe {
                    let points = std::slice::from_raw_parts(e.points, 3);
                    builder.cubic_bezier_to(
                        transform
                            .transform_point(&euclid::point2(points[0].x as _, points[0].y as _)),
                        transform
                            .transform_point(&euclid::point2(points[1].x as _, points[1].y as _)),
                        transform
                            .transform_point(&euclid::point2(points[2].x as _, points[2].y as _)),
                    );
                },
                #[allow(non_upper_case_globals)]
                kCGPathElementAddQuadCurveToPoint => unsafe {
                    let points = std::slice::from_raw_parts(e.points, 2);
                    builder.quadratic_bezier_to(
                        transform
                            .transform_point(&euclid::point2(points[0].x as _, points[0].y as _)),
                        transform
                            .transform_point(&euclid::point2(points[1].x as _, points[1].y as _)),
                    );
                },
                _ => unreachable!(),
            });

        Ok(())
    }
}
