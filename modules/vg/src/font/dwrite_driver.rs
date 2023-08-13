use euclid::point2;
use log::*;
use lyon_path::PathEvent;
use std::cell::RefCell;
use std::slice::from_raw_parts;
use windows::Win32::Graphics::Direct2D::Common::{
    ID2D1SimplifiedGeometrySink, ID2D1SimplifiedGeometrySink_Impl, D2D1_FIGURE_END_CLOSED,
    D2D_POINT_2F,
};

#[windows::core::implement(ID2D1SimplifiedGeometrySink)]
pub struct PathEventReceiver {
    paths: RefCell<Vec<PathEvent>>,
}
impl PathEventReceiver {
    pub fn new() -> Self {
        PathEventReceiver {
            paths: RefCell::new(Vec::new()),
        }
    }

    pub fn drain_all_paths(&mut self) -> std::vec::Drain<PathEvent> {
        self.paths.get_mut().drain(..)
    }
}
impl ID2D1SimplifiedGeometrySink_Impl for PathEventReceiver {
    fn AddLines(&self, p: *const D2D_POINT_2F, count: u32) {
        for p in unsafe { from_raw_parts(p, count as _) } {
            self.paths
                .borrow_mut()
                .push(PathEvent::LineTo(point2(p.x, -p.y)));
        }
    }

    fn AddBeziers(
        &self,
        beziers: *const windows::Win32::Graphics::Direct2D::Common::D2D1_BEZIER_SEGMENT,
        bezierscount: u32,
    ) {
        for p in unsafe { from_raw_parts(beziers, bezierscount as _) } {
            let (p1, p2) = (
                point2(p.point1.x, -p.point1.y),
                point2(p.point2.x, -p.point2.y),
            );
            let p3 = point2(p.point3.x, -p.point3.y);
            self.paths.borrow_mut().push(PathEvent::CubicTo(p1, p2, p3));
        }
    }

    fn BeginFigure(
        &self,
        startpoint: &D2D_POINT_2F,
        _figurebegin: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_BEGIN,
    ) {
        let p = point2(startpoint.x, -startpoint.y);
        self.paths.borrow_mut().push(PathEvent::MoveTo(p));
    }

    fn EndFigure(&self, figureend: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_END) {
        if figureend == D2D1_FIGURE_END_CLOSED {
            self.paths.borrow_mut().push(PathEvent::Close);
        }
    }

    fn Close(&self) -> windows::core::Result<()> {
        Ok(())
    }

    fn SetFillMode(&self, fillmode: windows::Win32::Graphics::Direct2D::Common::D2D1_FILL_MODE) {
        trace!("*UNIMPLEMENTED* SetFillMode with {fillmode:?}");
    }

    fn SetSegmentFlags(
        &self,
        vertexflags: windows::Win32::Graphics::Direct2D::Common::D2D1_PATH_SEGMENT,
    ) {
        trace!("*UNIMPLEMENTED* SetSegmentFlags with {vertexflags:?}");
    }
}
impl From<&'_ PathEventReceiver> for &'_ ID2D1SimplifiedGeometrySink {
    fn from(value: &'_ PathEventReceiver) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}
