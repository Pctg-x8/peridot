use appkit::{CTFont, CTFontOrientation, ExternalRc};
use euclid::Rect;
use lyon_path::builder::PathBuilder;

use crate::{Font, GlyphLoadingError};

#[repr(transparent)]
pub struct CoreTextFont(pub(crate) ExternalRc<CTFont>);
impl Font for CoreTextFont {
    type GlyphID = u32;

    fn set_em_size(&mut self, size: f32) {
        self.0 = self
            .0
            .create_copy_with_attributes(size as _, None, None)
            .expect("Failed to create copy font");
    }
    fn size(&self) -> f32 {
        self.0.size() as _
    }

    fn ascent(&self) -> f32 {
        self.0.ascent() as _
    }
    fn units_per_em(&self) -> u32 {
        self.0.units_per_em()
    }

    fn glyph_id(&self, c: char) -> Option<Self::GlyphID> {
        let mut u16s = [0u16; 2];
        c.encode_utf16(&mut u16s);
        // remove surrogate paired codepoint
        self.0
            .glyphs_for_characters(&u16s[..1])
            .ok()
            .map(|x| x[0] as _)
    }
    fn advance_h(&self, glyph: &Self::GlyphID) -> Result<f32, GlyphLoadingError> {
        Ok(self
            .0
            .advances_for_glyphs(CTFontOrientation::Horizontal, &[*glyph as _], None)
            as _)
    }
    fn bounds(&self, glyph: &Self::GlyphID) -> Result<Rect<f32>, GlyphLoadingError> {
        let r = self.0.bounding_rects_for_glyphs(
            appkit::CTFontOrientation::Horizontal,
            &[*glyph as _],
            None,
        );

        Ok(Rect::new(
            euclid::point2(r.origin.x as _, r.origin.y as _),
            euclid::size2(r.size.width as _, r.size.height as _),
        ))
    }
    fn outline(
        &self,
        glyph: &Self::GlyphID,
        transform: &euclid::Transform2D<f32>,
        builder: &mut impl PathBuilder,
    ) -> Result<(), GlyphLoadingError> {
        let path = self
            .0
            .create_path_for_glyph(*glyph as _, None)
            .map_err(|_| GlyphLoadingError::SysAPICallError("CTFont::create_path_for_glyph"))?;
        path.apply(|e| match e.type_ {
            appkit::CGPathElementType::MoveToPoint => unsafe {
                builder.move_to(
                    transform
                        .transform_point(&euclid::point2((*e.points).x as _, (*e.points).y as _)),
                );
            },
            appkit::CGPathElementType::CloseSubpath => builder.close(),
            appkit::CGPathElementType::AddLineToPoint => unsafe {
                builder.line_to(
                    transform
                        .transform_point(&euclid::point2((*e.points).x as _, (*e.points).y as _)),
                );
            },
            appkit::CGPathElementType::AddCurveToPoint => unsafe {
                let points = std::slice::from_raw_parts(e.points, 3);
                builder.cubic_bezier_to(
                    transform.transform_point(&euclid::point2(points[0].x as _, points[0].y as _)),
                    transform.transform_point(&euclid::point2(points[1].x as _, points[1].y as _)),
                    transform.transform_point(&euclid::point2(points[2].x as _, points[2].y as _)),
                );
            },
            appkit::CGPathElementType::AddQuadCurveToPoint => unsafe {
                let points = std::slice::from_raw_parts(e.points, 2);
                builder.quadratic_bezier_to(
                    transform.transform_point(&euclid::point2(points[0].x as _, points[0].y as _)),
                    transform.transform_point(&euclid::point2(points[1].x as _, points[1].y as _)),
                );
            },
        });

        Ok(())
    }
}
