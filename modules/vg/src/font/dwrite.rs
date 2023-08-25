use euclid::{point2, Rect};
use log::*;
use lyon_path::{builder::PathBuilder, PathEvent};
use std::cell::RefCell;
use std::slice::from_raw_parts;
use windows::{
    core::AsImpl,
    Win32::Graphics::{
        Direct2D::Common::{
            ID2D1SimplifiedGeometrySink, ID2D1SimplifiedGeometrySink_Impl, D2D1_FIGURE_END_CLOSED,
            D2D_POINT_2F,
        },
        DirectWrite::IDWriteFontFace,
    },
};

use crate::{Font, GlyphLoadingError};

pub struct DirectWriteFont(pub(crate) IDWriteFontFace, pub(crate) f32);
impl Font for DirectWriteFont {
    type GlyphID = u16;

    fn set_em_size(&mut self, size: f32) {
        self.1 = size;
    }
    fn size(&self) -> f32 {
        self.1
    }

    fn scale_value(&self) -> f32 {
        (96.0 * self.1 / 72.0) / self.units_per_em() as f32
    }
    /// Returns a scaled ascent metric value
    fn ascent(&self) -> f32 {
        let mut fm = core::mem::MaybeUninit::uninit();
        unsafe {
            self.0.GetMetrics(fm.as_mut_ptr());
            fm.assume_init_ref().ascent as f32 * self.scale_value()
        }
    }
    fn units_per_em(&self) -> u32 {
        let mut fm = std::mem::MaybeUninit::uninit();
        unsafe {
            self.0.GetMetrics(fm.as_mut_ptr());
            fm.assume_init_ref().designUnitsPerEm as _
        }
    }

    fn glyph_id(&self, c: char) -> Option<Self::GlyphID> {
        unsafe { self.0.GetGlyphIndices(&(c as u32), 1).ok() }
    }
    fn advance_h(&self, glyph: &Self::GlyphID) -> Result<f32, GlyphLoadingError> {
        let mut gm = core::mem::MaybeUninit::uninit();
        unsafe {
            self.0
                .GetDesignGlyphMetrics(glyph, 1, gm.as_mut_ptr(), false)?;
            Ok(gm.assume_init_ref().advanceWidth as f32 * self.scale_value())
        }
    }
    fn bounds(&self, glyph: &Self::GlyphID) -> Result<Rect<f32>, GlyphLoadingError> {
        let mut gm = core::mem::MaybeUninit::uninit();
        let gm = unsafe {
            self.0
                .GetDesignGlyphMetrics(glyph, 1, gm.as_mut_ptr(), false)?;
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
    fn outline(
        &self,
        glyph: &Self::GlyphID,
        transform: &euclid::Transform2D<f32>,
        builder: &mut impl PathBuilder,
    ) -> Result<(), GlyphLoadingError> {
        let sink = ID2D1SimplifiedGeometrySink::from(PathEventReceiver::new(transform.clone()));

        unsafe {
            self.0.GetGlyphRunOutline(
                96.0 * self.1 as f32 / 72.0,
                glyph,
                None,
                None,
                1,
                false,
                false,
                &sink,
            )?
        }
        for pe in sink.as_impl().drain_all_paths() {
            builder.path_event(pe);
        }

        Ok(())
    }
}

#[windows::core::implement(ID2D1SimplifiedGeometrySink)]
pub struct PathEventReceiver {
    transform: euclid::Transform2D<f32>,
    paths: RefCell<Vec<PathEvent>>,
}
impl<'t> PathEventReceiver {
    pub const fn new(transform: euclid::Transform2D<f32>) -> Self {
        PathEventReceiver {
            transform,
            paths: RefCell::new(Vec::new()),
        }
    }

    pub fn drain_all_paths(&self) -> Vec<PathEvent> {
        std::mem::replace(&mut self.paths.borrow_mut(), Vec::new())
    }
}
#[allow(non_snake_case)]
impl ID2D1SimplifiedGeometrySink_Impl for PathEventReceiver {
    fn AddLines(&self, p: *const D2D_POINT_2F, count: u32) {
        for p in unsafe { from_raw_parts(p, count as _) } {
            self.paths.borrow_mut().push(PathEvent::LineTo(
                self.transform.transform_point(&point2(p.x, -p.y)),
            ));
        }
    }

    fn AddBeziers(
        &self,
        beziers: *const windows::Win32::Graphics::Direct2D::Common::D2D1_BEZIER_SEGMENT,
        beziers_count: u32,
    ) {
        for p in unsafe { from_raw_parts(beziers, beziers_count as _) } {
            let (p1, p2) = (
                self.transform
                    .transform_point(&point2(p.point1.x, -p.point1.y)),
                self.transform
                    .transform_point(&point2(p.point2.x, -p.point2.y)),
            );
            let p3 = self
                .transform
                .transform_point(&point2(p.point3.x, -p.point3.y));
            self.paths.borrow_mut().push(PathEvent::CubicTo(p1, p2, p3));
        }
    }

    fn BeginFigure(
        &self,
        start_point: &D2D_POINT_2F,
        _figure_begin: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_BEGIN,
    ) {
        let p = self
            .transform
            .transform_point(&point2(start_point.x, -start_point.y));
        self.paths.borrow_mut().push(PathEvent::MoveTo(p));
    }

    fn EndFigure(&self, figure_end: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_END) {
        if figure_end == D2D1_FIGURE_END_CLOSED {
            self.paths.borrow_mut().push(PathEvent::Close);
        }
    }

    fn Close(&self) -> windows::core::Result<()> {
        Ok(())
    }

    fn SetFillMode(&self, fill_mode: windows::Win32::Graphics::Direct2D::Common::D2D1_FILL_MODE) {
        trace!("*UNIMPLEMENTED* SetFillMode with {fill_mode:?}");
    }

    fn SetSegmentFlags(
        &self,
        vertex_flags: windows::Win32::Graphics::Direct2D::Common::D2D1_PATH_SEGMENT,
    ) {
        trace!("*UNIMPLEMENTED* SetSegmentFlags with {vertex_flags:?}");
    }
}
