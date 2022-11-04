use euclid::point2;
use log::*;
use lyon_path::PathEvent;
use std::ptr::null_mut;
use std::slice::from_raw_parts;
use windows::Win32::Graphics::{
    Direct2D::Common::{
        ID2D1SimplifiedGeometrySink, ID2D1SimplifiedGeometrySink_Impl, D2D1_FIGURE_END_CLOSED,
        D2D_POINT_2F,
    },
    DirectWrite::{
        IDWriteFactory, IDWriteFontFileLoader, IDWriteFontFileLoader_Impl, IDWriteFontFileStream,
        IDWriteFontFileStream_Impl,
    },
};

#[windows::core::implement(ID2D1SimplifiedGeometrySink)]
pub struct PathEventReceiver {
    paths: Vec<PathEvent>,
}
impl PathEventReceiver {
    pub fn new() -> Self {
        PathEventReceiver { paths: Vec::new() }
    }

    pub fn drain_all_paths(&mut self) -> std::vec::Drain<PathEvent> {
        self.paths.drain(..)
    }
}
impl ID2D1SimplifiedGeometrySink_Impl for PathEventReceiver {
    fn AddLines(&self, p: *const D2D_POINT_2F, count: u32) {
        for p in unsafe { from_raw_parts(p, count as _) } {
            self.paths.push(PathEvent::LineTo(point2(p.x, -p.y)));
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
            self.paths.push(PathEvent::CubicTo(p1, p2, p3));
        }
    }

    fn BeginFigure(
        &self,
        startpoint: &D2D_POINT_2F,
        figurebegin: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_BEGIN,
    ) {
        let p = point2(startpoint.x, -startpoint.y);
        self.paths.push(PathEvent::MoveTo(p));
    }

    fn EndFigure(&self, figureend: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_END) {
        if figureend == D2D1_FIGURE_END_CLOSED {
            self.paths.push(PathEvent::Close);
        }
    }

    fn Close(&self) -> windows::core::Result<()> {
        Ok(())
    }

    fn SetFillMode(&self, fillmode: windows::Win32::Graphics::Direct2D::Common::D2D1_FILL_MODE) {
        trace!("*UNIMPLEMENTED* SetFillMode with {fillmode}");
    }

    fn SetSegmentFlags(
        &self,
        vertexflags: windows::Win32::Graphics::Direct2D::Common::D2D1_PATH_SEGMENT,
    ) {
        trace!("*UNIMPLEMENTED* SetSegmentFlags with {vertexflags}");
    }
}

pub struct ATFRegisterScope<'a>(&'a IDWriteFactory, AssetToFontConverter);
impl<'a> ATFRegisterScope<'a> {
    pub fn register(
        factory: &'a IDWriteFactory,
        atf: AssetToFontConverter,
    ) -> std::io::Result<Self> {
        unsafe {
            factory
                .RegisterFontFileLoader(&atf)
                .map(|_| Self(factory, atf))
        }
    }

    pub fn object(&self) -> &AssetToFontConverter {
        &self.1
    }
}
impl Drop for ATFRegisterScope<'_> {
    fn drop(&mut self) {
        unsafe {
            self.0
                .UnregisterFontFileLoader(&self.1)
                .expect("Failed to unregister FontFileLoader")
        };
    }
}

#[windows::core::implement(IDWriteFontFileLoader)]
pub struct AssetToFontConverter {
    asset: Option<super::TTFBlob>,
}
impl AssetToFontConverter {
    pub fn new(asset: super::TTFBlob) -> Self {
        Self { asset: Some(asset) }
    }
}
impl IDWriteFontFileLoader_Impl for AssetToFontConverter {
    fn CreateStreamFromKey(
        &self,
        fontfilereferencekey: *const core::ffi::c_void,
        fontfilereferencekeysize: u32,
    ) -> windows::core::Result<windows::Win32::Graphics::DirectWrite::IDWriteFontFileStream> {
        unsafe {
            AssetStreamBridge::new(self.asset.take().expect("ATF create stream called twice?"))
                .cast()
        }
    }
}

#[windows::core::implement(IDWriteFontFileStream)]
pub struct AssetStreamBridge {
    asset: super::TTFBlob,
}
impl AssetStreamBridge {
    fn new(asset: super::TTFBlob) -> Self {
        Self { asset }
    }
}
impl IDWriteFontFileStream_Impl for AssetStreamBridge {
    fn GetFileSize(&self) -> windows::core::Result<u64> {
        Ok(self.asset.0.len() as _)
    }

    fn GetLastWriteTime(&self) -> windows::core::Result<u64> {
        Ok(0)
    }

    fn ReadFileFragment(
        &self,
        fragmentstart: *mut *mut core::ffi::c_void,
        fileoffset: u64,
        fragmentsize: u64,
        fragmentcontext: *mut *mut core::ffi::c_void,
    ) -> windows::core::Result<()> {
        unsafe {
            *fragmentcontext = null_mut();
            *fragmentstart = self.asset.0.as_ptr().add(fileoffset as _) as *mut _;
        }

        Ok(())
    }

    fn ReleaseFileFragment(&self, fragmentcontext: *mut core::ffi::c_void) {}
}
