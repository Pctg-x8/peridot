#[macro_export]
macro_rules! peridot_build_watch {
    ($name: literal) => {
        println!(concat!("cargo::rerun-if-env-changed=PERIDOT_BUILD_", $name));
    };
}

/// Asserts the specified build environment variable is defined and has the value.
#[macro_export]
macro_rules! peridot_build_defined_and {
    ($name: literal, $value: expr) => {
        std::env::var_os(concat!("PERIDOT_BUILD_", $name)).is_some_and(|x| x == $value)
    };
}

/// Asserts the specified build environment variable is defined and set to "1".
#[macro_export]
macro_rules! peridot_build_switch_enable {
    ($name: literal) => {
        $crate::peridot_build_defined_and!($name, "1")
    };
}

mod common;
pub use self::common::*;
