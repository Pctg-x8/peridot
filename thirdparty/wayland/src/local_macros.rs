#[macro_export]
macro_rules! EventFnTable {
    { for $tyvar: ident : $tr: path { $($name: ident ( $($an: ident: $act: ty => $aconv: expr),* $(,)? )),* $(,)? } } => {
        {
            $(extern "C" fn $name<L: $tr>(
                data_: *mut core::ffi::c_void,
                sender_: *mut ffi::Proxy,
                $($an: $act),*
            ) {
                L::$name(
                    unsafe { &mut *(data_ as *mut _) },
                    unsafe { &mut *(sender_ as *mut _) },
                    $($aconv),*
                )
            })*

            #[repr(C)]
            struct FPTable { $($name: extern "C" fn(*mut core::ffi::c_void, *mut ffi::Proxy, $($act),*)),* }
            &const { FPTable { $($name: $name::<$tyvar>),* } } as &'static FPTable
        }
    }
}
