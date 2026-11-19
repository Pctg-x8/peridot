use windows::{
    Win32::{
        Foundation::{HWND, LPARAM, RECT},
        Graphics::Gdi::{
            EnumDisplayMonitors, HDC, HMONITOR, MONITOR_DEFAULTTOPRIMARY, MonitorFromWindow,
        },
    },
    core::BOOL,
};

#[inline(always)]
pub fn primary_monitor() -> HMONITOR {
    unsafe { MonitorFromWindow(HWND(core::ptr::null_mut()), MONITOR_DEFAULTTOPRIMARY) }
}

#[derive(Clone, Copy)]
pub enum EnumerateDisplayMonitorContinuous {
    Continue,
    Stop,
}

#[inline(always)]
pub fn enumerate_display_monitors<F>(mut f: F)
where
    F: FnMut(HMONITOR, &RECT) -> EnumerateDisplayMonitorContinuous,
{
    unsafe extern "system" fn callback<F>(
        mon: HMONITOR,
        _dc: HDC,
        rect: *mut RECT,
        param: LPARAM,
    ) -> BOOL
    where
        F: FnMut(HMONITOR, &RECT) -> EnumerateDisplayMonitorContinuous,
    {
        match (unsafe {
            &mut *core::ptr::with_exposed_provenance_mut::<F>(param.0.cast_unsigned())
        })(mon, unsafe { &*rect })
        {
            EnumerateDisplayMonitorContinuous::Continue => BOOL(1),
            EnumerateDisplayMonitorContinuous::Stop => BOOL(0),
        }
    }
    let _ = unsafe {
        EnumDisplayMonitors(
            None,
            None,
            Some(callback::<F>),
            LPARAM(core::ptr::from_mut(&mut f).addr().cast_signed()),
        )
    };
}
