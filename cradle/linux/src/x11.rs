//! X11Client PureRustImplementation

use std::io::{Read, Write, Result as IOResult, ErrorKind, Cursor};
use std::mem::{transmute, uninitialized, size_of};
use std::os::unix::net::UnixStream;

fn pad<T>(v: T, four: T) -> T where T: Copy + std::ops::Rem<Output = T> + std::ops::Sub<Output = T> {
    (four - (v % four)) % four
}

#[repr(C, packed)] struct ConnectionSetupRequest {
    pub major_version: u16, pub minor_version: u16,
    pub authorization_protocol_name_len: u16, pub authorization_protocol_data_len: u16,
    pub unused: u16
}
#[repr(C, packed)] struct ConnectionFailedFixedFields {
    pub reason_length: u8, pub major_version: u16, pub minor_version: u16,
    pub additional_data_length_shl2: u16
}
impl ConnectionFailedFixedFields {
    fn read_from<R: Read>(reader: &mut R) -> IOResult<(Self, Vec<u8>)> {
        let mut err_hdr: Self = unsafe { uninitialized() };
        reader.read_exact(unsafe { transmute::<_, &mut [u8; 7]>(&mut err_hdr) })?;
        let mut reply = vec![0u8; (err_hdr.additional_data_length_shl2 as usize) << 2];
        reader.read_exact(&mut reply)?;

        return Ok((err_hdr, reply));
    }
    fn reason<'d>(&self, data: &'d [u8]) -> &'d str {
        unsafe { std::str::from_utf8_unchecked(&data[..self.reason_length as usize]) }
    }
}
#[allow(dead_code)]
#[repr(u8)] #[derive(Debug)] enum ImageByteOrder { LSBFirst = 0, MSBFirst }
#[allow(dead_code)]
#[repr(u8)] #[derive(Debug)] enum BitmapFormatBitOrder { LeastSignificant = 0, MostSignificant }
#[repr(C, packed)] struct ProtocolFormatData { pub depth: u8, pub bpp: u8, pub scanline_pad: u8, _unused: [u8; 5] }
#[repr(C, packed)] struct ConnectionSuccessFixedFields {
    _unused0: u8, pub major_version: u16, pub minor_version: u16,
    pub additional_data_length: u16, pub release_number: u32, pub resource_id_base: u32,
    pub resource_id_mask: u32, pub motion_buffer_size: u32,
    pub vendor_len: u16, pub maximum_request_length: u16, pub root_screen_count: u8, pub pixmap_format_count: u8,
    pub image_byte_order: ImageByteOrder, pub bitmap_format_bit_order: BitmapFormatBitOrder,
    pub bitmap_format_scanline_unit: u8, pub bitmap_format_scanline_pad: u8,
    pub min_keycode: u8, pub max_keycode: u8, _unused1: u32
}
impl ConnectionSuccessFixedFields {
    fn read_from<R: Read>(reader: &mut R) -> IOResult<(Self, Vec<u8>)> {
        let mut v: Self = unsafe { uninitialized() };
        reader.read_exact(unsafe { transmute::<_, &mut [u8; 39]>(&mut v) })?;
        let mut additional_bytes = vec![0u8; ((v.additional_data_length as usize) - 8) << 2];
        reader.read_exact(&mut additional_bytes)?;
        
        return Ok((v, additional_bytes));
    }
	fn additional_byte_offsets(&self) -> (usize, usize, usize) {
        let vpad = pad(self.vendor_len, 4);
        let format_start = (self.vendor_len + vpad) as usize;
        let screens_start = format_start + (self.pixmap_format_count as usize) * size_of::<ProtocolFormatData>();

		(0, format_start, screens_start)
	}
    /*fn decomposite_additional_bytes<'a>(&self, bytes: &'a [u8]) -> (&'a str, &'a [ProtocolFormatData], &'a [u8]) {
        let vendor = unsafe { std::str::from_utf8_unchecked(&bytes[..self.vendor_len as usize]) };
        let vpad = pad(self.vendor_len, 4);
        let format_start = (self.vendor_len + vpad) as usize;
        let format_bytes = unsafe {
            std::slice::from_raw_parts(bytes.as_ptr().offset(format_start as isize) as *const _, self.pixmap_format_count as _)
        };
        let screens_start = format_start + (self.pixmap_format_count as usize) * size_of::<ProtocolFormatData>();
        let screens = &bytes[screens_start..];

        (vendor, format_bytes, screens)
    }*/
}

enum ConnectionResponses {
    Success(ConnectionSuccessFixedFields, Vec<u8>), Failed(ConnectionFailedFixedFields, Vec<u8>)
}
impl ConnectionResponses {
    fn ensure_success(self) -> (ConnectionSuccessFixedFields, Vec<u8>) {
        match self {
            ConnectionResponses::Success(a, b) => (a, b),
            ConnectionResponses::Failed(e, d) => panic!("X11 Connection Error!: {}", e.reason(&d))
        }
    }
}

#[allow(dead_code)]
#[repr(u8)] pub enum BackingStore { Never = 0, WhenMapped, Always }
#[repr(C, packed)] pub struct ScreenStruct {
    pub root_window_id: u32, pub default_colormap: u32, pub white_pixel: u32, pub black_pixel: u32,
    pub current_input_mask: u32, pub width_in_pixels: u16, pub height_in_pixels: u16,
    pub width_in_mm: u16, pub height_in_mm: u16, pub min_installed_maps: u16, pub max_installed_maps: u16,
    pub root_visual_id: u32, pub backing_stores: BackingStore,
    pub save_unders: u8, pub root_depth: u8, pub allowed_depth_count: u8
}
pub struct ScreenIterator<'d> { slice: &'d [u8], offset: isize, left: usize }
impl<'d> Iterator for ScreenIterator<'d> {
    type Item = &'d ScreenStruct;

    fn next(&mut self) -> Option<&'d ScreenStruct> {
        if self.slice.len() <= self.offset as usize { return None; }
        let v = unsafe { &*std::mem::transmute::<_, *const ScreenStruct>(self.slice.as_ptr().offset(self.offset)) };
        self.offset += std::mem::size_of::<ScreenStruct>() as isize + v.allowed_depth_count as isize;
        self.left -= 1;

        return Some(v);
    }
}
impl<'d> ExactSizeIterator for ScreenIterator<'d> {
    fn len(&self) -> usize { self.left }
}

#[allow(dead_code)]
#[repr(u16)] pub enum WindowClass { CopyFromParent = 0, InputOutput, InputOnly }
#[repr(C, packed)] pub struct RequestCreateWindowFixedFields {
    pub opcode: u8, pub depth: u8, pub request_length: u16,
    pub target_id: u32, pub parent_id: u32, pub x: i16, pub y: i16, pub width: u16, pub height: u16,
    pub border_width: u16, pub class: WindowClass, pub visual: u32, pub valuemask: u32
}
impl Default for RequestCreateWindowFixedFields {
    fn default() -> Self {
        RequestCreateWindowFixedFields {
            opcode: 1, depth: 0, request_length: 8,
            target_id: 0, parent_id: 0, x: 0, y: 0, width: 0, height: 0, border_width: 0,
            class: WindowClass::CopyFromParent, visual: 0, valuemask: 0
        }
    }
}
impl RequestCreateWindowFixedFields {
    pub fn send_with<W: Write>(&mut self, data: &[u8], writer: &mut W) -> IOResult<()> {
        self.request_length = (8 + (data.len() >> 2)) as _;
        writer.write_all(unsafe { &transmute::<_, &[u8; 32]>(self)[..] })?;
        writer.write_all(data)?;
        return Ok(());
    }
}
#[repr(C, packed)] pub struct RequestMapWindow { pub opcode: u8, pub _unused: u8, pub request_length: u16, pub window: u32 }
impl Default for RequestMapWindow {
    fn default() -> Self {
        RequestMapWindow { opcode: 8, _unused: 0, request_length: 2, window: 0 }
    }
}
impl RequestMapWindow {
    pub fn send<W: Write>(&mut self, writer: &mut W) -> IOResult<()> {
        writer.write_all(unsafe { &transmute::<_, &[u8; 8]>(self)[..] })
    }
}

pub type Atom = u32;
pub const ATOM: Atom = 4;
pub const WM_NAME: Atom = 39;
pub const STRING: Atom = 31;

#[allow(dead_code)]
#[repr(u8)] pub enum ChangePropertyMode { Replace = 0, Prepend, Append }
#[repr(C, packed)] pub struct RequestChangeProperty {
    opcode: u8, pub mode: ChangePropertyMode, request_length: u16,
    pub window: u32, pub property: Atom, pub type_: Atom, pub format: u8, _unused: [u8; 3],
    pub data_elements: u32
}
impl Default for RequestChangeProperty {
    fn default() -> Self {
        RequestChangeProperty {
            opcode: 18, mode: ChangePropertyMode::Replace, request_length: 0,
            window: 0, property: 0, type_: 0, format: 8, _unused: [0; 3], data_elements: 0
        }
    }
}
impl RequestChangeProperty {
    fn send_with<W: Write, V: PropertyValue>(&mut self, data: &[V], writer: &mut W) -> IOResult<()> {
        V::send_values(data, self, writer)
    }
}
// PropertyValue for ChangeProperty Request
pub trait PropertyValue: Copy {
    fn send_value1<W: Write>(self, fixed_fields: &mut RequestChangeProperty, writer: &mut W) -> IOResult<()> {
        Self::send_values(&[self], fixed_fields, writer)
    }
    fn send_values<W: Write>(values: &[Self], fixed_fields: &mut RequestChangeProperty, writer: &mut W) -> IOResult<()>;
}
impl PropertyValue for u8 {
    fn send_values<W: Write>(values: &[u8], fixed_fields: &mut RequestChangeProperty, writer: &mut W) -> IOResult<()> {
        let data_pad = pad(values.len(), 4);
        fixed_fields.request_length = 6 + ((values.len() + data_pad) >> 2) as u16;
        fixed_fields.format = 8;
        fixed_fields.data_elements = values.len() as _;
        writer.write_all(unsafe { &transmute::<_, &[u8; 24]>(fixed_fields)[..] })?;
        writer.write_all(values)?; writer.write_all(&vec![0; data_pad])?;
        return Ok(());
    }
}
impl PropertyValue for u16 {
    fn send_values<W: Write>(values: &[u16], fixed_fields: &mut RequestChangeProperty, writer: &mut W) -> IOResult<()> {
        let data_pad = pad(values.len() << 1, 4);
        fixed_fields.request_length = 6 + (((values.len() << 1) + data_pad) >> 2) as u16;
        fixed_fields.format = 16;
        fixed_fields.data_elements = values.len() as _;
        writer.write_all(unsafe { &transmute::<_, &[u8; 24]>(fixed_fields)[..] })?;
        writer.write_all(unsafe { std::slice::from_raw_parts(values.as_ptr() as *const _, values.len() << 1) })?;
        writer.write_all(&vec![0; data_pad])?;
        return Ok(());
    }
}
impl PropertyValue for u32 {
    fn send_values<W: Write>(values: &[u32], fixed_fields: &mut RequestChangeProperty, writer: &mut W) -> IOResult<()> {
        let data_pad = pad(values.len() << 2, 4);
        fixed_fields.request_length = 6 + (((values.len() << 2) + data_pad) >> 2) as u16;
        fixed_fields.format = 32;
        fixed_fields.data_elements = values.len() as _;
        writer.write_all(unsafe { &transmute::<_, &[u8; 24]>(fixed_fields)[..] })?;
        writer.write_all(unsafe { std::slice::from_raw_parts(values.as_ptr() as *const _, values.len() << 2) })?;
        writer.write_all(&vec![0; data_pad])?;
        return Ok(());
    }
}
#[repr(C, packed)] struct RequestInternAtom {
    opcode: u8, pub only_if_exists: u8, request_length: u16, pub name_length: u16, _unused: u16
}
impl Default for RequestInternAtom {
    fn default() -> Self {
        RequestInternAtom { opcode: 16, only_if_exists: 0, request_length: 2, name_length: 0, _unused: 0 }
    }
}
impl RequestInternAtom {
    fn send_with<W: Write>(&mut self, data: &[u8], writer: &mut W) -> IOResult<()> {
        let pads = pad(data.len(), 4);
        self.request_length = 2 + ((data.len() + pads) >> 2) as u16;
        self.name_length = data.len() as _;
        writer.write_all(unsafe { &std::mem::transmute::<_, &[u8; 8]>(self)[..] })?;
        writer.write_all(data)?; writer.write_all(&vec![0; pads])?;
        return Ok(());
    }
}
#[repr(C, packed)] struct ReplyInternAtom {
    _reply_code: u8, _unused: u8, pub sequence_number: u16, pub reply_length: u32, pub atom: Atom, _after: [u8; 20]
}
impl ReplyInternAtom {
    unsafe fn from_bytes_unchecked(bytes: &[u8; 32]) -> &Self { transmute(bytes) }
}

#[repr(C, packed)] pub struct ClientMessageEvent {
    _code: u8, pub format: u8, pub sequence_number: u16, pub window: u32, pub type_: Atom, pub data: [u8; 20]
}
impl ClientMessageEvent {
    pub const CODE: u8 = 33;
    pub unsafe fn as_ref(v: &[u8; 32]) -> &Self { transmute(v) }

    pub unsafe fn u32_data(&self) -> &[u32] { std::slice::from_raw_parts(self.data.as_ptr() as *const _, 20 >> 2) }
}

pub struct XAuthorityEntries { data: Vec<u8> }
impl XAuthorityEntries {
    pub fn open() -> Self {
        let d = std::fs::read(std::env::var_os("XAUTHORITY").expect("No XAuthority"))
            .expect("Could not read XAuthority Keys");
        XAuthorityEntries { data: d }
    }

    pub fn iter(&self) -> XAuthorityEntriesIterator { XAuthorityEntriesIterator { data: &self.data, offset: 0 } }
}
pub struct XAuthorityEntry<'d> {
    pub srv_index: &'d [u8], pub authprotocol_name: &'d [u8], pub authprotocol_data: &'d [u8]
}
pub struct XAuthorityEntriesIterator<'d> { data: &'d [u8], offset: usize }
impl<'d> Iterator for XAuthorityEntriesIterator<'d> {
    type Item = XAuthorityEntry<'d>;

    fn next(&mut self) -> Option<XAuthorityEntry<'d>> {
        if self.data.len() <= self.offset { return None; }

        self.offset += 2;   // つかわないので
        let len_sock_name = unsafe {
            u16::from_be(*transmute::<_, *const _>(self.data.as_ptr().offset(self.offset as _)))
        };
        self.offset += 2 + len_sock_name as usize;
        let len_srv_index = unsafe {
            u16::from_be(*transmute::<_, *const _>(self.data.as_ptr().offset(self.offset as _))) 
        };
        let srv_index = &self.data[self.offset + 2 .. self.offset + 2 + len_srv_index as usize];
        self.offset += 2 + len_srv_index as usize;
        let len_authprotocol_name = unsafe {
            u16::from_be(*transmute::<_, *const _>(self.data.as_ptr().offset(self.offset as _)))
        };
        self.offset += 2;
        let authprotocol_bytes = &self.data[self.offset .. self.offset + len_authprotocol_name as usize];
        self.offset += len_authprotocol_name as usize;
        let len_authprotocol_data = unsafe {
            u16::from_be(*transmute::<_, *const _>(self.data.as_ptr().offset(self.offset as _)))
        };
        self.offset += 2;
        let authprotocol_data_bytes = &self.data[self.offset .. self.offset + len_authprotocol_data as usize];
        self.offset += len_authprotocol_data as usize;

        return Some(XAuthorityEntry {
            srv_index, authprotocol_name: authprotocol_bytes, authprotocol_data: authprotocol_data_bytes
        });
    }
}

fn parse_display_envar_for_local() -> (u32, u32) {
    let vdata = std::env::var("DISPLAY").expect("No DISPLAY var");
    let host_separated = vdata.split(":").skip(1).next().expect("Invalid Format(HostName)");
    let mut dp_sc_separated = host_separated.split(".");
    let dnum = str::parse(dp_sc_separated.next().expect("Invalid Format(No display server number)"))
        .expect("Invalid Format(Display Server Number)");
    let snum = str::parse(dp_sc_separated.next().expect("Invalid Format(No screen number)"))
        .expect("Invalid Format(Screen Number)");

    (dnum, snum)
}

fn connect_local() -> IOResult<UnixStream> {
    let (d, _) = parse_display_envar_for_local();

    let authority_store = XAuthorityEntries::open();
    let auth_pair = authority_store.iter()
        .find(|x| str::parse(unsafe { std::str::from_utf8_unchecked(x.srv_index) }) == Ok(d))
        .map(|x| (x.authprotocol_name, x.authprotocol_data))
        .expect("No suitable Authorization pair");
    
    let mut tp = UnixStream::connect(format!("/tmp/.X11-unix/X{}", d))?;
    tp.write_all(&unsafe { transmute::<u16, [u8; 2]>(0x426c) })?;
    tp.write_all(&unsafe { transmute::<_, [u8; 10]>(ConnectionSetupRequest {
        major_version: 11, minor_version: 0,
        authorization_protocol_name_len: auth_pair.0.len() as _, authorization_protocol_data_len: auth_pair.1.len() as _,
        unused: 0
    }) })?;
    tp.write_all(auth_pair.0)?; tp.write_all(&vec![0; pad(auth_pair.0.len(), 4)])?;
    tp.write_all(auth_pair.1)?; tp.write_all(&vec![0; pad(auth_pair.1.len(), 4)])?;

    return Ok(tp);
}
fn acquire_server_info(sock: &mut UnixStream) -> IOResult<ConnectionResponses> {
    let mut r0 = [0u8]; sock.read(&mut r0)?;
    if r0[0] == 0 {
        let (e, d) = ConnectionFailedFixedFields::read_from(sock)?;
        Ok(ConnectionResponses::Failed(e, d))
    }
    else {
        let (succ_hdr, addbytes) = ConnectionSuccessFixedFields::read_from(sock).unwrap();
        Ok(ConnectionResponses::Success(succ_hdr, addbytes))
    }
}

pub struct Connection {
	socket: UnixStream, server_info: ConnectionSuccessFixedFields, addbytes: Vec<u8>, next_resource_id: u32
}
impl Connection {
	pub fn connect_local() -> IOResult<Self> {
		let mut sock = connect_local()?;
		let (server_info, addbytes) = acquire_server_info(&mut sock)?.ensure_success();
		Ok(Connection { socket: sock, addbytes, next_resource_id: server_info.resource_id_base, server_info })
	}

	pub fn screen_iter(&self) -> ScreenIterator {
		let slice_offset = self.server_info.additional_byte_offsets().2;
		ScreenIterator {
			slice: &self.addbytes[slice_offset..], offset: 0, left: self.server_info.root_screen_count as _
		}
	}
	pub fn next_resource_id(&mut self) -> u32 {
		let v = self.next_resource_id;
		self.next_resource_id += 1;
		return v;
	}
	pub fn start_nonblocking(&self) { self.socket.set_nonblocking(true).unwrap(); }

	pub fn intern_atoms(&mut self, atoms: &[&[u8]]) -> IOResult<Vec<Atom>> {
		let buflen = size_of::<RequestInternAtom>() * atoms.len() + atoms.iter().map(|n| n.len()).fold(0, |a, b| a + b);
		let mut packbuffer = Cursor::new(Vec::with_capacity(buflen));
		for a in atoms { RequestInternAtom::default().send_with(a, &mut packbuffer).unwrap(); }
		self.socket.write_all(&mut packbuffer.into_inner())?;

		let mut replies = vec![0; atoms.len()];
		let mut reply_packet = [0; 32];
		for r in &mut replies {
			self.socket.read_exact(&mut reply_packet)?;
			if reply_packet[0] == 1 { unsafe { *r = ReplyInternAtom::from_bytes_unchecked(&reply_packet).atom; } }
			else { panic!("Not an Atom Return: {:?}", reply_packet); }
		}

		return Ok(replies);
	}
	pub fn get_event(&mut self) -> IOResult<Option<[u8; 32]>> {
		let mut b = [0; 32];
		match self.socket.read(&mut b) {
			Ok(_) => Ok(Some(b)),
			Err(k) => if k.kind() == ErrorKind::WouldBlock { Ok(None) } else { Err(k) }
		}
	}
}
impl Write for Connection {
	fn write(&mut self, buf: &[u8]) -> IOResult<usize> { self.socket.write(buf) }
	fn flush(&mut self) -> IOResult<()> { self.socket.flush() }
}
impl Read for Connection {
	fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> { self.socket.read(buf) }
}

pub struct BufferedPacket(Cursor<Vec<u8>>);
impl BufferedPacket {
    pub fn new() -> Self { BufferedPacket(Cursor::new(Vec::new())) }

    pub fn change_property<V: PropertyValue>(&mut self, res: u32, property: Atom, type_: Atom, value: &[V]) {
        RequestChangeProperty { window: res, property, type_, .. Default::default() }
            .send_with(value, &mut self.0).unwrap();
    }
    pub fn append_property<V: PropertyValue>(&mut self, res: u32, property: Atom, type_: Atom, value: &[V]) {
        RequestChangeProperty { window: res, property, type_, mode: ChangePropertyMode::Append, .. Default::default() }
            .send_with(value, &mut self.0).unwrap();
    }
    pub fn map(&mut self, res: u32) {
        RequestMapWindow { window: res, .. Default::default() }.send(&mut self.0).unwrap();
    }

    pub fn send<W: Write>(self, writer: &mut W) -> IOResult<()> { writer.write_all(&self.0.into_inner()) }
}
impl Write for BufferedPacket {
    fn write(&mut self, buf: &[u8]) -> IOResult<usize> { self.0.write(buf) }
    fn flush(&mut self) -> IOResult<()> { self.0.flush() }
}
