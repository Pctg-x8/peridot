use std::{
    io::{IoSlice, IoSliceMut, Read, Write},
    mem::MaybeUninit,
};

/// Read into all slices
pub fn read_vectored_all(
    o: &mut (impl Read + ?Sized),
    mut bufs: &mut [IoSliceMut],
) -> std::io::Result<()> {
    IoSliceMut::advance_slices(&mut bufs, 0); // ensure actually empty

    while !bufs.is_empty() {
        let b = o.read_vectored(bufs)?;
        IoSliceMut::advance_slices(&mut bufs, b);
    }

    Ok(())
}

/// Write from all slices
pub fn write_vectored_all(
    o: &mut (impl Write + ?Sized),
    mut bufs: &mut [IoSlice],
) -> std::io::Result<()> {
    IoSlice::advance_slices(&mut bufs, 0); // ensure actually empty

    while !bufs.is_empty() {
        let b = o.write_vectored(bufs)?;
        IoSlice::advance_slices(&mut bufs, b);
    }

    Ok(())
}

/// Create a [`IoSlice`] from a reference to a value
///
/// preserving native byte order
#[inline(always)]
pub fn mkioslice<'a, T>(value: &'a T) -> IoSlice<'a> {
    IoSlice::new(unsafe {
        core::slice::from_raw_parts(core::ptr::from_ref(value).cast(), size_of::<T>())
    })
}

/// Create a [`IoSliceMut`] from a uninitialized reference to a value
///
/// preserving native byte order
#[inline(always)]
pub fn mkioslicem<'a, T>(sink: &'a mut MaybeUninit<T>) -> IoSliceMut<'a> {
    IoSliceMut::new(unsafe {
        core::slice::from_raw_parts_mut(sink.as_mut_ptr().cast(), size_of::<T>())
    })
}

/// Creates a [`IoSliceMut`] from a uninitialized reference to a member in a struct
///
/// preserving native byte order
#[macro_export]
macro_rules! mkioslicem_field {
    ($sink: expr, $ty: ty, $field: tt) => {{
        #[allow(invalid_value)]
        let size: usize =
            unsafe { core::mem::size_of_val(&core::mem::transmute::<usize, &$ty>(0).$field) };

        std::io::IoSliceMut::new(unsafe {
            core::slice::from_raw_parts_mut(
                $sink
                    .as_mut_ptr()
                    .byte_add(core::mem::offset_of!($ty, $field))
                    .cast(),
                size,
            )
        })
    }};
}
