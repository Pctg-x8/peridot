use std::io::{IoSlice, IoSliceMut, Read, Write};

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
