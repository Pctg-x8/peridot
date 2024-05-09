use windows::{
    core::PCSTR,
    Foundation::TimeSpan,
    Win32::{
        Foundation::{HINSTANCE, HWND},
        UI::WindowsAndMessaging::{
            CreateWindowExA, RegisterClassExA, CW_USEDEFAULT, WINDOW_EX_STYLE, WINDOW_STYLE,
            WNDCLASSEXA, WS_EX_APPWINDOW, WS_EX_LAYERED, WS_EX_NOACTIVATE,
            WS_EX_NOREDIRECTIONBITMAP, WS_EX_TOPMOST, WS_EX_TRANSPARENT, WS_OVERLAPPEDWINDOW,
            WS_POPUP,
        },
    },
};

mod ui_composition;
pub use self::ui_composition::*;

#[derive(Clone, Copy)]
pub enum WindowClass {
    Text(PCSTR),
    Atom(u16),
}
impl From<u16> for WindowClass {
    fn from(value: u16) -> Self {
        Self::Atom(value)
    }
}
impl From<PCSTR> for WindowClass {
    fn from(value: PCSTR) -> Self {
        Self::Text(value)
    }
}
impl WindowClass {
    #[inline]
    fn as_pcstr(&self) -> PCSTR {
        match self {
            Self::Text(x) => *x,
            Self::Atom(x) => PCSTR(*x as _),
        }
    }
}

pub struct WindowBuilder {
    cls: WindowClass,
    title: PCSTR,
    x: Option<i32>,
    y: Option<i32>,
    width: Option<i32>,
    height: Option<i32>,
    instance: HINSTANCE,
    style: WINDOW_STYLE,
    ex_style: WINDOW_EX_STYLE,
}
impl WindowBuilder {
    #[inline]
    pub fn new(instance: HINSTANCE, cls: impl Into<WindowClass>, title: PCSTR) -> Self {
        Self {
            cls: cls.into(),
            title,
            x: None,
            y: None,
            width: None,
            height: None,
            instance,
            style: WINDOW_STYLE(0),
            ex_style: WINDOW_EX_STYLE(0),
        }
    }

    #[inline]
    pub fn create(self) -> windows::core::Result<HWND> {
        let h = unsafe {
            CreateWindowExA(
                self.ex_style,
                self.cls.as_pcstr(),
                self.title,
                self.style,
                self.x.unwrap_or(CW_USEDEFAULT),
                self.y.unwrap_or(CW_USEDEFAULT),
                self.width.unwrap_or(CW_USEDEFAULT),
                self.height.unwrap_or(CW_USEDEFAULT),
                None,
                None,
                self.instance,
                None,
            )
        };

        if h.0 == 0 {
            Err(windows::core::Error::from_win32())
        } else {
            Ok(h)
        }
    }

    #[inline]
    pub fn popup(mut self) -> Self {
        self.style |= WS_POPUP;
        self
    }
    #[inline]
    pub fn transparent(mut self) -> Self {
        self.ex_style |= WS_EX_LAYERED | WS_EX_TRANSPARENT;
        self
    }
    #[inline]
    pub fn topmost(mut self) -> Self {
        self.ex_style |= WS_EX_TOPMOST;
        self
    }
    #[inline]
    pub fn no_activate(mut self) -> Self {
        self.ex_style |= WS_EX_NOACTIVATE;
        self
    }
    #[inline]
    pub fn no_redirection_bitmap(mut self) -> Self {
        self.ex_style |= WS_EX_NOREDIRECTIONBITMAP;
        self
    }
    #[inline]
    pub fn app_window(mut self) -> Self {
        self.ex_style |= WS_EX_APPWINDOW;
        self
    }
    #[inline]
    pub fn overlapped_window(mut self) -> Self {
        self.style |= WS_OVERLAPPEDWINDOW;
        self
    }
}

#[inline]
pub fn register_window_class(cls: &WNDCLASSEXA) -> windows::core::Result<u16> {
    match unsafe { RegisterClassExA(cls) } {
        0 => Err(windows::core::Error::from_win32()),
        x => Ok(x),
    }
}

#[inline]
pub const fn timespan_ms(ms: u32) -> TimeSpan {
    TimeSpan {
        Duration: (10_000 * ms) as _,
    }
}
