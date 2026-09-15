//! Platform-specific functionalities

#[cfg(unix)]
pub mod unix;
#[cfg(unix)]
pub use self::unix::*;
#[cfg(target_os = "linux")]
pub mod linux;
#[cfg(target_os = "linux")]
pub use self::linux::*;
#[cfg(windows)]
pub mod windows;
#[cfg(windows)]
pub use self::windows::*;
