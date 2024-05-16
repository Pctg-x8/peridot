use windows::{
    core::implement,
    Graphics::{IGeometrySource2D, IGeometrySource2D_Impl},
    Win32::{
        Foundation::E_NOTIMPL,
        Graphics::Direct2D::ID2D1Geometry,
        System::WinRT::Graphics::Direct2D::{
            IGeometrySource2DInterop, IGeometrySource2DInterop_Impl,
        },
    },
};

#[implement(IGeometrySource2D, IGeometrySource2DInterop)]
pub struct GeometryInterop(pub ID2D1Geometry);
impl IGeometrySource2D_Impl for GeometryInterop {}
impl IGeometrySource2DInterop_Impl for GeometryInterop {
    fn GetGeometry(
        &self,
    ) -> windows_core::Result<windows::Win32::Graphics::Direct2D::ID2D1Geometry> {
        Ok(self.0.clone())
    }

    fn TryGetGeometryUsingFactory(
        &self,
        _factory: Option<&windows::Win32::Graphics::Direct2D::ID2D1Factory>,
    ) -> windows_core::Result<windows::Win32::Graphics::Direct2D::ID2D1Geometry> {
        Err(windows::core::Error::new(E_NOTIMPL, "unimplemented"))
    }
}
