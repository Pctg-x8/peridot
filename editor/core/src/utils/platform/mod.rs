//! Platform specific functionalities

#[cfg(target_os = "linux")]
pub mod linux;
#[cfg(unix)]
pub mod unix;
#[cfg(windows)]
pub mod windows;
