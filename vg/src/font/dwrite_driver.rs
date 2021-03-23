
use libc::c_void;
use winapi::Interface;
use winapi::shared::minwindef::ULONG;
use winapi::shared::winerror::{S_OK, E_NOINTERFACE, HRESULT};
use winapi::shared::guiddef::REFIID;
use winapi::um::unknwnbase::{IUnknown, IUnknownVtbl};
use winapi::um::d2d1::{
    ID2D1SimplifiedGeometrySink, ID2D1SimplifiedGeometrySinkVtbl, D2D1_FIGURE_BEGIN, D2D1_FIGURE_END,
    D2D1_FILL_MODE, D2D1_PATH_SEGMENT,
    D2D1_FIGURE_END_CLOSED
};
use winapi::um::dwrite::{
    IDWriteFontFileLoader, IDWriteFontFileLoaderVtbl,
    IDWriteFontFileStream, IDWriteFontFileStreamVtbl
};
use std::ptr::null_mut;
use lyon_path::PathEvent;
use euclid::point2;
use comdrive::d2;
use log::*;

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
    unsafe extern "system"
    fn add_beziers(this: *mut ID2D1SimplifiedGeometrySink, p: *const d2::BezierSegment, count: u32)
    {
        for p in from_raw_parts(p, count as _)
        {
            let (p1, p2) = (point2(p.point1.x, -p.point1.y), point2(p.point2.x, -p.point2.y));
            let p3 = point2(p.point3.x, -p.point3.y);
            (*(this as *mut Self)).paths.push(PathEvent::CubicTo(p1, p2, p3));
        }
    }
    unsafe extern "system"
    fn begin_figure(this: *mut ID2D1SimplifiedGeometrySink, p: d2::Point2F, _begin: D2D1_FIGURE_BEGIN)
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

pub struct ATFRegisterScope<'a>(&'a comdrive::dwrite::Factory, comdrive::ComPtr<AssetToFontConverter<'a>>);
impl<'a> ATFRegisterScope<'a> {
    pub fn register(factory: &'a comdrive::dwrite::Factory, atf: comdrive::ComPtr<AssetToFontConverter<'a>>) -> std::io::Result<Self> {
        factory.register_font_file_loader(unsafe { &*atf.0 }).map(|_| Self(factory, atf))
    }

    pub fn object(&self) -> &AssetToFontConverter<'a> { unsafe { &*(self.1).0 } }
}
impl Drop for ATFRegisterScope<'_> {
    fn drop(&mut self) {
        self.0.unregister_font_file_loader(unsafe { &*self.1 .0 }).expect("Failed to unregister FontFileLoader");
    }
}

#[repr(C)]
pub struct AssetToFontConverter<'a> {
    base: ComBase<IDWriteFontFileLoaderVtbl>,
    asset: &'a super::TTFBlob
}
pub static ATF_VTABLE: IDWriteFontFileLoaderVtbl = IDWriteFontFileLoaderVtbl {
    CreateStreamFromKey: AssetToFontConverter::create_stream_from_key,
    parent: IUnknownVtbl {
        QueryInterface: AssetToFontConverter::query_interface,
        AddRef: ComBase::<IDWriteFontFileLoaderVtbl>::add_ref,
        Release: ComBase::<IDWriteFontFileLoaderVtbl>::release
    }
};
impl<'a> AssetToFontConverter<'a> {
    pub unsafe extern "system" fn query_interface(this: *mut IUnknown, iid: REFIID, objret: *mut *mut c_void) -> HRESULT {
        *objret = null_mut();
        if iid == &IDWriteFontFileLoader::uuidof() || iid == &IUnknown::uuidof() {
            *objret = this as *mut _;
            return S_OK;
        }
        
        E_NOINTERFACE
    }

    pub fn new(asset: &'a super::TTFBlob) -> *mut Self {
        println!("vtbl: {:p}", &ATF_VTABLE);
        Box::into_raw(Box::new(Self {
            base: ComBase::new(&ATF_VTABLE),
            asset
        }))
    }
}
/// IDWritFontFileLoader
impl AssetToFontConverter<'_> {
    unsafe extern "system" fn create_stream_from_key(
        this: *mut IDWriteFontFileLoader,
        _refkey: *const c_void,
        _keysize: u32,
        stream: *mut *mut IDWriteFontFileStream
    ) -> HRESULT {
        *stream = AssetStreamBridge::new((*(this as *mut Self)).asset) as *mut _;

        S_OK
    }
}
unsafe impl comdrive::AsRawHandle<IDWriteFontFileLoader> for AssetToFontConverter<'_> {
    fn as_raw_handle(&self) -> *mut IDWriteFontFileLoader { self as *const Self as _ }
}

#[repr(C)]
pub struct AssetStreamBridge<'a> {
    base: ComBase<IDWriteFontFileStreamVtbl>,
    asset: &'a super::TTFBlob
}
impl<'a> AssetStreamBridge<'a> {
    const VTABLE: &'static IDWriteFontFileStreamVtbl = &IDWriteFontFileStreamVtbl {
        GetFileSize: Self::get_size,
        GetLastWriteTime: Self::get_last_write_time,
        ReadFileFragment: Self::read_file_fragment,
        ReleaseFileFragment: Self::release_file_fragment,
        parent: IUnknownVtbl {
            QueryInterface: Self::query_interface,
            AddRef: ComBase::<IDWriteFontFileStreamVtbl>::add_ref,
            Release: ComBase::<IDWriteFontFileStreamVtbl>::release
        }
    };
    unsafe extern "system" fn query_interface(this: *mut IUnknown, iid: REFIID, objret: *mut *mut c_void) -> HRESULT {
        *objret = null_mut();
        if iid == &IDWriteFontFileStream::uuidof() || iid == &IUnknown::uuidof() {
            *objret = this as *mut _;
            return S_OK;
        }

        E_NOINTERFACE
    }

    fn new(asset: &'a super::TTFBlob) -> *mut Self {
        Box::into_raw(Box::new(Self {
            base: ComBase::new(Self::VTABLE),
            asset
        }))
    }
}
/// IDWriteFontFileStream
impl AssetStreamBridge<'_> {
    unsafe extern "system" fn get_size(this: *mut IDWriteFontFileStream, size: *mut u64) -> HRESULT {
        *size = (*(this as *mut Self)).asset.0.len() as _;
        S_OK
    }
    unsafe extern "system" fn get_last_write_time(
        _this: *mut IDWriteFontFileStream, last_write_time: *mut u64
    ) -> HRESULT {
        *last_write_time = 0;
        S_OK
    }
    unsafe extern "system" fn read_file_fragment(
        this: *mut IDWriteFontFileStream,
        fragment_start: *mut *const c_void,
        file_offset: u64,
        _fragment_size: u64,
        fragment_context: *mut *mut c_void
    ) -> HRESULT {
        *fragment_context = null_mut();
        *fragment_start = (*(this as *mut Self)).asset.0.as_ptr().add(file_offset as _) as *const _;

        S_OK
    }
    unsafe extern "system" fn release_file_fragment(
        _this: *mut IDWriteFontFileStream, _fragment_context: *mut c_void
    ) {}
}
