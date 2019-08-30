
use libc::c_void;
use winapi::Interface;
use winapi::shared::minwindef::{ULONG};
use winapi::shared::winerror::{S_OK, E_NOINTERFACE, HRESULT};
use winapi::shared::guiddef::REFIID;
use winapi::um::unknwnbase::{IUnknown, IUnknownVtbl};
use winapi::um::d2d1::{
    ID2D1SimplifiedGeometrySink, ID2D1SimplifiedGeometrySinkVtbl, D2D1_FIGURE_BEGIN, D2D1_FIGURE_END,
    D2D1_FILL_MODE, D2D1_PATH_SEGMENT,
    D2D1_FIGURE_END_CLOSED
};
use std::ptr::null_mut;
use lyon_path::PathEvent;
use euclid::point2;
use comdrive::d2;

#[repr(C)] pub struct ComBase<VTable: 'static> { vtbl: &'static VTable, refcount: ULONG }
impl<VTable: 'static> ComBase<VTable>
{
    pub fn new(vt: &'static VTable) -> Self { ComBase { vtbl: vt, refcount: 1 } }

    // Default Impls for IUnknown //
    unsafe extern "system" fn add_ref(this: *mut IUnknown) -> ULONG
    {
        let this = &mut *(this as *mut Self);
        this.refcount += 1;
        return this.refcount;
    }
    unsafe extern "system" fn release(this: *mut IUnknown) -> ULONG
    {
        let thisref = &mut *(this as *mut Self);
        thisref.refcount -= 1; let last_refcount = thisref.refcount;
        if thisref.refcount == 0 { drop(Box::from_raw(this as *mut Self)); }
        return last_refcount;
    }
}
#[repr(C)] pub struct PathEventReceiver { base: ComBase<ID2D1SimplifiedGeometrySinkVtbl>, paths: Vec<PathEvent> }
impl PathEventReceiver
{
    const VTABLE: &'static ID2D1SimplifiedGeometrySinkVtbl = &ID2D1SimplifiedGeometrySinkVtbl
    {
        SetFillMode: Self::set_fill_mode, SetSegmentFlags: Self::set_segment_flags,
        BeginFigure: Self::begin_figure, EndFigure: Self::end_figure,
        AddLines: Self::add_lines, AddBeziers: Self::add_beziers, Close: Self::close,
        parent: IUnknownVtbl
        {
            QueryInterface: Self::query_interface,
            AddRef: ComBase::<ID2D1SimplifiedGeometrySinkVtbl>::add_ref,
            Release: ComBase::<ID2D1SimplifiedGeometrySinkVtbl>::release
        }
    };
    pub fn new() -> *mut Self
    {
        Box::into_raw(Box::new(PathEventReceiver { base: ComBase::new(Self::VTABLE), paths: Vec::new() }))
    }

    unsafe extern "system" fn query_interface(this: *mut IUnknown, iid: REFIID, objret: *mut *mut c_void) -> HRESULT
    {
        *objret = null_mut();
        if iid == &ID2D1SimplifiedGeometrySink::uuidof() ||
            iid == &IUnknown::uuidof() { *objret = this as *mut _; S_OK }
        else { E_NOINTERFACE }
    }

    pub fn drain_all_paths(&mut self) -> std::vec::Drain<PathEvent> { self.paths.drain(..) }
}
use std::slice::from_raw_parts;
/// SimplifiedGeometrySink
impl PathEventReceiver
{
    unsafe extern "system" fn add_lines(this: *mut ID2D1SimplifiedGeometrySink, p: *const d2::Point2F, count: u32)
    {
        for p in from_raw_parts(p, count as _)
        {
            (*(this as *mut Self)).paths.push(PathEvent::LineTo(point2(p.x, -p.y)));
        }
    }
    unsafe extern "system" fn add_beziers(this: *mut ID2D1SimplifiedGeometrySink, p: *const d2::BezierSegment, count: u32)
    {
        for p in from_raw_parts(p, count as _)
        {
            let (p1, p2) = (point2(p.point1.x, -p.point1.y), point2(p.point2.x, -p.point2.y));
            let p3 = point2(p.point3.x, -p.point3.y);
            (*(this as *mut Self)).paths.push(PathEvent::CubicTo(p1, p2, p3));
        }
    }
    unsafe extern "system" fn begin_figure(this: *mut ID2D1SimplifiedGeometrySink, p: d2::Point2F, _begin: D2D1_FIGURE_BEGIN)
    {
        let p = point2(p.x, -p.y);
        (*(this as *mut Self)).paths.push(PathEvent::MoveTo(p));
    }
    unsafe extern "system" fn end_figure(this: *mut ID2D1SimplifiedGeometrySink, end: D2D1_FIGURE_END)
    {
        if end == D2D1_FIGURE_END_CLOSED
        {
            (*(this as *mut Self)).paths.push(PathEvent::Close);
        }
    }
    unsafe extern "system" fn close(_this: *mut ID2D1SimplifiedGeometrySink) -> HRESULT { S_OK }
    unsafe extern "system" fn set_fill_mode(_this: *mut ID2D1SimplifiedGeometrySink, _mode: D2D1_FILL_MODE)
    {
        trace!("*UNIMPLEMENTED* SetFillMode with {}", _mode);
    }
    unsafe extern "system" fn set_segment_flags(_this: *mut ID2D1SimplifiedGeometrySink, _segment: D2D1_PATH_SEGMENT)
    {
        trace!("*UNIMPLEMENTED* SetSegmentFlags with {}", _segment);
    }
}
unsafe impl comdrive::AsRawHandle<ID2D1SimplifiedGeometrySink> for PathEventReceiver
{
    fn as_raw_handle(&self) -> *mut ID2D1SimplifiedGeometrySink { self as *const _ as _ }
}
