use std::{collections::HashMap, os::windows::ffi::OsStrExt};

fn main() {
    let fh = unsafe {
        windows::Win32::Storage::FileSystem::CreateFileW(
            windows_core::PCWSTR(
                std::env::args_os()
                    .nth(1)
                    .expect("input file")
                    .encode_wide()
                    .chain(core::iter::once(0))
                    .collect::<Vec<_>>()
                    .as_ptr(),
            ),
            windows::Win32::Foundation::GENERIC_READ.0,
            windows::Win32::Storage::FileSystem::FILE_SHARE_READ,
            None,
            windows::Win32::Storage::FileSystem::OPEN_EXISTING,
            windows::Win32::Storage::FileSystem::FILE_ATTRIBUTE_NORMAL,
            None,
        )
        .expect("open file")
    };
    let filemap = unsafe {
        windows::Win32::System::Memory::CreateFileMappingW(
            fh,
            None,
            windows::Win32::System::Memory::PAGE_READONLY,
            0,
            0,
            None,
        )
        .expect("createfilemapping")
    };
    let content_ptr = unsafe {
        windows::Win32::System::Memory::MapViewOfFile(
            filemap,
            windows::Win32::System::Memory::FILE_MAP_READ,
            0,
            0,
            0,
        )
    };
    let mut file_size = core::mem::MaybeUninit::uninit();
    unsafe {
        windows::Win32::Storage::FileSystem::GetFileSizeEx(fh, file_size.as_mut_ptr())
            .expect("getfilesize")
    };
    let file_size = unsafe { usize::try_from(file_size.assume_init()).expect("invalid file size") };

    let bom = unsafe { core::ptr::read(content_ptr.Value.cast::<u16>()) };
    assert!(bom == 0x0102 || bom == 0x0201);
    let needs_reverse = bom != 0x0102;
    let target_pointer_size =
        unsafe { core::ptr::read(content_ptr.Value.byte_add(2).cast::<u8>()) };
    let timestamp_frequency =
        unsafe { core::ptr::read_unaligned(content_ptr.Value.byte_add(3).cast::<i64>()) };
    let timestamp_frequency = if needs_reverse {
        timestamp_frequency.swap_bytes()
    } else {
        timestamp_frequency
    };
    println!(
        "needs_reverse: {needs_reverse}\ntarget_pointer_size: {target_pointer_size}\ntimestamp_frequency: {timestamp_frequency}"
    );
    let reader = ProfileBinReader {
        target_pointer_size,
        needs_reverse,
    };

    let marker_addr_to_name_start =
        reader.read_u64(unsafe { content_ptr.Value.byte_add(file_size - 8) });
    let marker_count =
        reader.read_usize(unsafe { content_ptr.Value.byte_add(marker_addr_to_name_start as _) });
    let mut marker_addr_to_name = HashMap::with_capacity(marker_count as _);
    let mut read_ptr = marker_addr_to_name_start + target_pointer_size as u64;
    for _ in 0..marker_count {
        let addr = reader.read_usize(unsafe { content_ptr.Value.byte_add(read_ptr as _) });
        read_ptr += target_pointer_size as u64;
        let name_len = (read_ptr..)
            .take_while(|&p| unsafe {
                core::ptr::read(content_ptr.Value.byte_add(p as _).cast::<u8>()) != 0
            })
            .count();
        let name = unsafe {
            str::from_utf8_unchecked(core::slice::from_raw_parts(
                content_ptr.Value.byte_add(read_ptr as _).cast::<u8>(),
                name_len,
            ))
        };
        read_ptr += name_len as u64 + 1;

        marker_addr_to_name.insert(addr, name);
    }

    println!("{marker_addr_to_name:#?}");

    let mut readptr = 11;
    loop {
        let marker_tag =
            unsafe { core::ptr::read(content_ptr.Value.byte_add(readptr).cast::<u8>()) };
        readptr += 1;

        match marker_tag {
            0x00 => {
                // terminal marker
                break;
            }
            0x01 => {
                // event marker
                let ts = reader.read_i64(unsafe { content_ptr.Value.byte_add(readptr) });
                readptr += 8;
                let marker_ident =
                    reader.read_usize(unsafe { content_ptr.Value.byte_add(readptr) });
                readptr += target_pointer_size as usize;

                println!(
                    "{}: {:.3} ms",
                    marker_addr_to_name[&marker_ident],
                    ts as f64 * 1000.0 / timestamp_frequency as f64
                );
            }
            0x02 => {
                // section begin marker
                let ts = reader.read_i64(unsafe { content_ptr.Value.byte_add(readptr) });
                readptr += 8;
                let marker_ident =
                    reader.read_usize(unsafe { content_ptr.Value.byte_add(readptr) });
                readptr += target_pointer_size as usize;
                let section_id = reader.read_u64(unsafe { content_ptr.Value.byte_add(readptr) });
                readptr += 8;

                println!(
                    "{}[#{section_id} Begin]: {:.3} ms",
                    marker_addr_to_name[&marker_ident],
                    ts as f64 * 1000.0 / timestamp_frequency as f64
                );
            }
            0x03 => {
                // section end marker
                let ts = reader.read_i64(unsafe { content_ptr.Value.byte_add(readptr) });
                readptr += 8;
                let section_id = reader.read_u64(unsafe { content_ptr.Value.byte_add(readptr) });
                readptr += 8;

                println!(
                    "#{section_id} End: {:.3} ms",
                    ts as f64 * 1000.0 / timestamp_frequency as f64
                );
            }
            _ => unreachable!("unknown marker tag: {marker_tag:#02x}"),
        }
    }
}

struct ProfileBinReader {
    target_pointer_size: u8,
    needs_reverse: bool,
}
impl ProfileBinReader {
    #[inline(always)]
    pub fn read_i64(&self, ptr: *mut core::ffi::c_void) -> i64 {
        let n = unsafe { core::ptr::read_unaligned(ptr.cast::<i64>()) };
        if self.needs_reverse {
            n.swap_bytes()
        } else {
            n
        }
    }

    #[inline(always)]
    pub fn read_u64(&self, ptr: *mut core::ffi::c_void) -> u64 {
        let n = unsafe { core::ptr::read_unaligned(ptr.cast::<u64>()) };
        if self.needs_reverse {
            n.swap_bytes()
        } else {
            n
        }
    }

    #[inline(always)]
    pub fn read_usize(&self, ptr: *mut core::ffi::c_void) -> usize {
        let n = match self.target_pointer_size {
            4 => unsafe { core::ptr::read_unaligned(ptr.cast::<u32>()) as usize },
            8 => unsafe { core::ptr::read_unaligned(ptr.cast::<u64>()) as usize },
            _ => panic!("unsupported target_pointer_size"),
        };
        if self.needs_reverse {
            n.swap_bytes()
        } else {
            n
        }
    }
}
