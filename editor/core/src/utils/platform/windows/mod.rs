mod extensions;
mod log_writer;

use std::path::PathBuf;

use windows::{
    Win32::{
        Foundation::{HINSTANCE, HWND},
        System::{Diagnostics::Debug::OutputDebugStringA, LibraryLoader::GetModuleHandleW},
        UI::WindowsAndMessaging::{
            FindWindowExW, GCW_ATOM, GetClassLongW, RegisterClassExW, WNDCLASSEXW,
        },
    },
    core::PCSTR,
};
use windows_core::{HSTRING, PCWSTR};

pub use self::extensions::*;
pub use self::log_writer::DebugOutputWriter;

pub fn set_panic_hook() {
    std::panic::set_hook(Box::new(|panic| unsafe {
        let panic_msg = match std::ffi::CString::new(panic.to_string()) {
            Ok(x) => x,
            Err(_) => c"<<Could not convert panic message!>>".into(),
        };

        OutputDebugStringA(PCSTR(panic_msg.as_ptr().cast()));
        std::process::abort();
    }));
}

#[inline(always)]
pub fn current_instance_handle() -> HINSTANCE {
    unsafe { GetModuleHandleW(None).expect("GetModuleHandleW").into() }
}

#[inline(always)]
pub unsafe fn register_class(x: &WNDCLASSEXW) -> std::io::Result<u16> {
    match unsafe { RegisterClassExW(x) } {
        r if r == 0 => Err(std::io::Error::last_os_error()),
        r => Ok(r),
    }
}

#[inline(always)]
pub unsafe fn window_class_atom(w: HWND) -> u16 {
    unsafe { GetClassLongW(w, GCW_ATOM) as _ }
}

pub struct WindowByClassIter {
    class: PCWSTR,
    window_after: Option<HWND>,
}
impl WindowByClassIter {
    pub fn new(class: PCWSTR) -> Self {
        Self {
            class,
            window_after: None,
        }
    }
}
impl Iterator for WindowByClassIter {
    type Item = windows_core::Result<HWND>;

    fn next(&mut self) -> Option<Self::Item> {
        match unsafe { FindWindowExW(None, self.window_after, self.class, None) } {
            Ok(x) => {
                self.window_after = Some(x);
                Some(Ok(x))
            }
            Err(e) if e == windows_core::Error::empty() => None,
            Err(e) => Some(Err(e)),
        }
    }
}

#[inline(always)]
pub fn local_app_data_dir() -> PathBuf {
    PathBuf::from(
        std::env::var_os("LOCALAPPDATA").expect("no LOCALAPPDATA defined in the environment"),
    )
}

pub fn user_language() -> HSTRING {
    windows::System::UserProfile::GlobalizationPreferences::Languages()
        .expect("globalization_preferences.languages")
        .First()
        .expect("vector_view.first")
        .Current()
        .expect("iterator.current")
}

pub fn find_word_segment(input: &str, byte_pos: usize) -> core::range::Range<usize> {
    let text_pos = input
        .char_indices()
        .take_while(|&(i, _)| i < byte_pos)
        .count();

    let ws = windows::Data::Text::WordsSegmenter::CreateWithLanguage(&user_language())
        .expect("words_segmenter.create")
        .GetTokenAt(
            &HSTRING::from_wide(&input.encode_utf16().collect::<Vec<_>>()),
            text_pos as _,
        )
        .expect("word_segmenter.get_token_at");
    let text_segment = ws
        .SourceTextSegment()
        .expect("word_segment.source_text_segment");

    let start = input
        .chars()
        .take(text_segment.StartPosition as _)
        .map(char::len_utf8)
        .sum();
    let end = input
        .chars()
        .take((text_segment.StartPosition + text_segment.Length) as _)
        .map(char::len_utf8)
        .sum();
    (start..end).into()
}
