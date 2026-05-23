#![allow(non_snake_case)]

#[repr(C)]
pub(self) struct OpaqueStruct(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

macro_rules! symbol_rename {
    ($name: ident) => {
        concat!(stringify!($name), "_76")
    };
}
pub(self) use symbol_rename;

mod umachine;
pub use self::umachine::*;

mod utypes;
pub use self::utypes::*;

mod parseerr;
pub use self::parseerr::*;

mod ubrk;
pub use self::ubrk::*;

mod utext;
pub use self::utext::*;

mod uloc;
pub use self::uloc::*;
