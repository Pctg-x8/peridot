use apple_sdk_port::{
    CFOwned, CGPathApply, CGPathElement, CTFontCreateCopyWithAttributes, CTFontCreatePathForGlyph,
    CTFontGetAdvancesForGlyphs, CTFontGetAscent, CTFontGetBoundingRectsForGlyphs,
    CTFontGetGlyphsForCharacters, CTFontGetSize, CTFontGetUnitsPerEm, UniChar, __CTFont,
    kCGPathElementAddCurveToPoint, kCGPathElementAddLineToPoint, kCGPathElementAddQuadCurveToPoint,
    kCGPathElementCloseSubpath, kCGPathElementMoveToPoint, kCTFontOrientationHorizontal,
};
use euclid::Rect;
use lyon_path::builder::PathBuilder;

use crate::{Font, GlyphLoadingError};

#[repr(transparent)]
pub struct CoreTextFont(pub(crate) CFOwned<__CTFont>);
impl Font for CoreTextFont {
    type GlyphID = u32;

    fn set_em_size(&mut self, size: f32) {
        self.0 = unsafe {
            CFOwned::from_ptr(
                CTFontCreateCopyWithAttributes(
                    self.0.as_ptr(),
                    size as _,
                    core::ptr::null(),
                    core::ptr::null(),
                )
                .cast_mut(),
            )
            .expect("Failed to create copy font")
        };
    }
    fn size(&self) -> f32 {
        unsafe { CTFontGetSize(self.0.as_ptr()) as _ }
    }

    fn ascent(&self) -> f32 {
        unsafe { CTFontGetAscent(self.0.as_ptr()) as _ }
    }
    fn units_per_em(&self) -> u32 {
        unsafe { CTFontGetUnitsPerEm(self.0.as_ptr()) }
    }

    fn glyph_id(&self, c: char) -> Option<Self::GlyphID> {
        let mut u16s = [0u16; 2];
        let u16s = c.encode_utf16(&mut u16s);
        println!("encu16 {c} -> {u16s:?}");
        let mut glyphs = core::mem::MaybeUninit::uninit();
        unsafe {
            // remove surrogate paired codepoint
            if !CTFontGetGlyphsForCharacters(
                self.0.as_ptr(),
                u16s.as_ptr().add(1),
                glyphs.as_mut_ptr(),
                1,
            ) {
                // TODO: err?
                return None;
            }
        }

        Some(unsafe { glyphs.assume_init() as _ })
    }
    fn advance_h(&self, glyph: &Self::GlyphID) -> Result<f32, GlyphLoadingError> {
        let mut advances = core::mem::MaybeUninit::uninit();
        unsafe {
            CTFontGetAdvancesForGlyphs(
                self.0.as_ptr(),
                // TODO: 縦書き？
                kCTFontOrientationHorizontal,
                &(*glyph as UniChar) as _,
                advances.as_mut_ptr(),
                1,
            )
        };

        Ok(unsafe { advances.assume_init_ref().width as _ })
    }
    fn bounds(&self, glyph: &Self::GlyphID) -> Result<Rect<f32>, GlyphLoadingError> {
        let mut bounding_rects = core::mem::MaybeUninit::uninit();
        unsafe {
            CTFontGetBoundingRectsForGlyphs(
                self.0.as_ptr(),
                // TODO: 縦書き？
                kCTFontOrientationHorizontal,
                &(*glyph as UniChar) as _,
                bounding_rects.as_mut_ptr(),
                1,
            );
        }
        let r = unsafe { bounding_rects.assume_init_ref() };

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
        let path = unsafe {
            CFOwned::from_ptr(
                CTFontCreatePathForGlyph(self.0.as_ptr(), *glyph as UniChar, core::ptr::null())
                    .cast_mut(),
            )
            .ok_or_else(|| GlyphLoadingError::SysAPICallError("CTFontCreatePathForGlyph"))?
        };
        unsafe {
            extern "C" fn path_applier<B: PathBuilder>(
                ctx: *mut core::ffi::c_void,
                e: *const CGPathElement,
            ) {
                let e = unsafe { &*e };
                let &mut (ref mut builder, ref transform) =
                    unsafe { &mut *ctx.cast::<(&mut B, &euclid::Transform2D<f32>)>() };

                match e.r#type {
                    #[allow(non_upper_case_globals)]
                    kCGPathElementMoveToPoint => unsafe {
                        builder.move_to(transform.transform_point(&euclid::point2(
                            (*e.points).x as _,
                            (*e.points).y as _,
                        )));
                    },
                    #[allow(non_upper_case_globals)]
                    kCGPathElementCloseSubpath => builder.close(),
                    #[allow(non_upper_case_globals)]
                    kCGPathElementAddLineToPoint => unsafe {
                        builder.line_to(transform.transform_point(&euclid::point2(
                            (*e.points).x as _,
                            (*e.points).y as _,
                        )));
                    },
                    #[allow(non_upper_case_globals)]
                    kCGPathElementAddCurveToPoint => unsafe {
                        let points = std::slice::from_raw_parts(e.points, 3);
                        builder.cubic_bezier_to(
                            transform.transform_point(&euclid::point2(
                                points[0].x as _,
                                points[0].y as _,
                            )),
                            transform.transform_point(&euclid::point2(
                                points[1].x as _,
                                points[1].y as _,
                            )),
                            transform.transform_point(&euclid::point2(
                                points[2].x as _,
                                points[2].y as _,
                            )),
                        );
                    },
                    #[allow(non_upper_case_globals)]
                    kCGPathElementAddQuadCurveToPoint => unsafe {
                        let points = std::slice::from_raw_parts(e.points, 2);
                        builder.quadratic_bezier_to(
                            transform.transform_point(&euclid::point2(
                                points[0].x as _,
                                points[0].y as _,
                            )),
                            transform.transform_point(&euclid::point2(
                                points[1].x as _,
                                points[1].y as _,
                            )),
                        );
                    },
                    _ => unreachable!(),
                }
            }

            CGPathApply(
                path.as_ptr(),
                &mut (builder, transform) as *mut _ as _,
                path_applier::<B>,
            );
        }

        Ok(())
    }
}
