use euclid::Rect;
use lyon_path::builder::PathBuilder;

use crate::{Font, GlyphLoadingError};

const CTFONT_DEFAULT_SIZE: f32 = 12.0;

pub struct CoreTextFont(pub(crate) appkit::ExternalRc<CTFont>, pub(crate) f32);
impl Font for CoreTextFont {
    type GlyphID = u32;

    fn set_em_size(&mut self, size: f32) {
        self.1 = size;
    }
    fn size(&self) -> f32 {
        self.1
    }

    fn scale_value(&self) -> f32 {
        self.1 / CTFONT_DEFAULT_SIZE
    }
    fn ascent(&self) -> f32 {
        self.0.ascent() as f32 * self.scale_value()
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
            .advances_for_glyphs(appkit::CTFontOrientation::Horizontal, &[*glyph as _], None)
            as _)
    }
    fn bounds(&self, glyph: &Self::GlyphID) -> Result<Rect<f32>, GlyphLoadingError> {
        let r = self.0.bounding_rects_for_glyphs(
            appkit::CTFontOrientation::Horizontal,
            &[*glyph as _],
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
    fn outline<B: PathBuilder>(
        &self,
        glyph: &Self::GlyphID,
        builder: &mut B,
    ) -> Result<(), GlyphLoadingError> {
        let path = self
            .0
            .create_path_for_glyph(*glyph as _, None)
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
}
