use std::os::fd::RawFd;

use bedrock as br;
use x11::xlib::{
    AllocNone, CWBackPixel, CWColormap, CWEventMask, CWOverrideRedirect, DirectColor, ExposureMask,
    InputOutput, ResizeRedirectMask, ResizeRequest, XBlackPixel, XCloseDisplay, XConnectionNumber,
    XCreateColormap, XCreateWindow, XDefaultScreen, XErrorEvent, XFlush, XGetErrorText,
    XGetInputFocus, XMapWindow, XMatchVisualInfo, XNextEvent, XOpenDisplay, XPending,
    XQueryPointer, XRootWindow, XSetErrorHandler, XSetWindowAttributes, XVisualInfo,
};

const fn id<T>(x: T) -> T {
    x
}

#[repr(transparent)]
struct Display(std::ptr::NonNull<x11::xlib::Display>);
impl Display {
    pub fn open(dpy: Option<*const i8>) -> Option<Self> {
        std::ptr::NonNull::new(unsafe { XOpenDisplay(dpy.map_or_else(std::ptr::null, id)) })
            .map(Self)
    }

    pub fn default_screen(&self) -> std::ffi::c_int {
        unsafe { XDefaultScreen(self.0.as_ptr()) }
    }

    pub fn root_window(&self, screen: std::ffi::c_int) -> x11::xlib::Window {
        unsafe { XRootWindow(self.0.as_ptr(), screen) }
    }

    pub fn black_pixel(&self, screen: std::ffi::c_int) -> std::ffi::c_ulong {
        unsafe { XBlackPixel(self.0.as_ptr(), screen) }
    }

    pub fn flush(&self) {
        unsafe { XFlush(self.0.as_ptr()) };
    }

    pub fn pending(&self) -> std::ffi::c_int {
        unsafe { XPending(self.0.as_ptr()) }
    }

    pub fn next_event(&mut self) -> x11::xlib::XEvent {
        let mut e = std::mem::MaybeUninit::uninit();
        unsafe { XNextEvent(self.0.as_ptr(), e.as_mut_ptr()) };
        unsafe { e.assume_init() }
    }

    pub fn connection_number(&self) -> std::ffi::c_int {
        unsafe { XConnectionNumber(self.0.as_ptr()) }
    }

    pub fn match_visual_info(
        &self,
        screen: std::ffi::c_int,
        depth: std::ffi::c_int,
        class: std::ffi::c_int,
    ) -> Option<XVisualInfo> {
        let mut sink = std::mem::MaybeUninit::uninit();
        let r =
            unsafe { XMatchVisualInfo(self.0.as_ptr(), screen, depth, class, sink.as_mut_ptr()) };
        if r == 0 {
            None
        } else {
            Some(unsafe { sink.assume_init() })
        }
    }
}
impl Drop for Display {
    fn drop(&mut self) {
        unsafe { XCloseDisplay(self.0.as_ptr()) };
    }
}

pub struct WindowSystem {
    display: Display,
    w: x11::xlib::Window,
    vid: x11::xlib::VisualID,
    mainwnd_size: peridot::math::Vector2<usize>,
}
impl super::WindowSystemBackend for WindowSystem {
    fn init() -> Self {
        let display = Display::open(None).expect("Failed to open x11 display");
        unsafe { XSetErrorHandler(Some(error_handler)) };

        let screen = display.default_screen();
        let root_window = display.root_window(screen);
        let vi = display
            .match_visual_info(screen, 24, DirectColor)
            .expect("no matching visual for 24bit direct-color");
        let cmap =
            unsafe { XCreateColormap(display.0.as_ptr(), root_window, vi.visual, AllocNone) };

        let mut xattr = XSetWindowAttributes {
            colormap: cmap,
            event_mask: ExposureMask | ResizeRedirectMask,
            background_pixel: display.black_pixel(screen),
            override_redirect: true as _,
            ..unsafe { std::mem::MaybeUninit::zeroed().assume_init() }
        };
        let attr_mask = CWColormap | CWEventMask | CWBackPixel | CWOverrideRedirect;
        let w = unsafe {
            XCreateWindow(
                display.0.as_ptr(),
                root_window,
                0,
                0,
                640,
                480,
                1,
                24,
                InputOutput as _,
                vi.visual,
                attr_mask,
                &mut xattr,
            )
        };
        display.flush();

        Self {
            display,
            w,
            vid: vi.visualid,
            mainwnd_size: peridot::math::Vector2(640, 480),
        }
    }

    fn fd(&self) -> RawFd {
        self.display.connection_number()
    }

    fn flush(&self) {
        self.display.flush();
    }

    fn show(&self) {
        unsafe { XMapWindow(self.display.0.as_ptr(), self.w) };
        self.display.flush();
    }

    fn process_all_events(&mut self) -> bool {
        while self.display.pending() > 0 {
            let event = self.display.next_event();
            let ty = unsafe { event.type_ };

            if ty == ResizeRequest {
                let e = unsafe { &event.resize_request };
                self.mainwnd_size = peridot::math::Vector2(e.width as _, e.height as _);
            } else {
                log::debug!("unhandled event: 0x{ty:02x}")
            }
        }

        true
    }

    fn mainwnd_geometry(&self) -> &peridot::math::Vector2<usize> {
        &self.mainwnd_size
    }
}
impl super::VulkanPresentable for WindowSystem {
    const REQUIRED_INSTANCE_EXTENSION: &'static str = "VK_KHR_xlib_surface";

    fn presentation_support(
        &self,
        adapter: &impl br::PhysicalDevice,
        render_queue_family: u32,
    ) -> bool {
        adapter.xlib_presentation_support(render_queue_family, self.display.0.as_ptr(), self.vid)
    }

    fn create_surface<
        PhysicalDevice: br::PhysicalDevice + br::InstanceChild + br::InstanceChildTransferrable,
    >(
        &self,
        adapter: PhysicalDevice,
    ) -> br::Result<br::SurfaceObject<PhysicalDevice::ConcreteInstance>> {
        adapter.new_surface_xlib(self.display.0.as_ptr(), self.w)
    }
}
impl super::InputSystemBackend for WindowSystem {
    fn get_pointer_position(&self) -> Option<(f32, f32)> {
        let (mut x, mut y) = (0, 0);
        let r = unsafe {
            XQueryPointer(
                self.display.0.as_ptr(),
                self.w,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                &mut x,
                &mut y,
                std::ptr::null_mut(),
            )
        };
        let in_same_screen = r != 0;

        if in_same_screen {
            Some((x as _, y as _))
        } else {
            debug!("Fixme: Handle in_same_screen = false");
            None
        }
    }

    fn is_focused(&self) -> bool {
        let mut focus_wid = 0;
        unsafe {
            XGetInputFocus(
                self.display.0.as_ptr(),
                &mut focus_wid,
                std::ptr::null_mut(),
            )
        };
        focus_wid == self.w
    }

    fn query_states_batched(&self) -> (bool, (i16, i16)) {
        let (mut x, mut y) = (0, 0);
        unsafe {
            XQueryPointer(
                self.display.0.as_ptr(),
                self.w,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                &mut x,
                &mut y,
                std::ptr::null_mut(),
            )
        };

        (self.is_focused(), (x as _, y as _))
    }
}

unsafe extern "C" fn error_handler(
    d: *mut x11::xlib::Display,
    e: *mut XErrorEvent,
) -> std::ffi::c_int {
    let e = unsafe { e.as_ref().expect("null error event") };
    let mut error_text = [0i8; 256];
    unsafe {
        XGetErrorText(
            d,
            e.error_code as _,
            error_text.as_mut_ptr(),
            error_text.len() as _,
        )
    };

    log::error!(
        "[req #{}({}.{})] Err {}: {}",
        e.serial,
        e.request_code,
        e.minor_code,
        e.error_code,
        unsafe {
            std::ffi::CStr::from_ptr(error_text.as_ptr())
                .to_str()
                .expect("invalid sequence")
        }
    );

    0
}
