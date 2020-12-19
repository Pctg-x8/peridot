//! OpenGL Helper

use once_cell::unsync::Lazy;

pub struct Texture(libc::c_uint);
impl Texture {
	pub fn new() -> Self {
		let mut id = 0;
		unsafe { glGenTextures(1, &mut id); }
		Texture(id)
	}
	pub fn id(&self) -> libc::c_uint { self.0 }
}
impl Drop for Texture {
	fn drop(&mut self) {
		unsafe { glDeleteTextures(1, &self.0); }
	}
}

pub fn get_error() -> libc::c_uint { unsafe { glGetError() } }

pub struct Framebuffer(libc::c_uint);
impl Framebuffer {
	pub fn new() -> Self {
		let mut id = 0;
		unsafe { glGenFramebuffers(1, &mut id); }
		Framebuffer(id)
	}
	pub fn id(&self) -> libc::c_uint { self.0 }
}
impl Drop for Framebuffer {
	fn drop(&mut self) {
		unsafe { glDeleteFramebuffers(1, &self.0); }
	}
}

const CREATE_MEMORY_OBJECTS_EXT_PROC: Lazy<PFNGLCREATEMEMORYOBJECTSEXTPROC> = Lazy::new(|| unsafe {
	crate::egl::get_fnptr(b"glCreateMemoryObjectsEXT\0").expect("no glCreateMemoryObjectsEXT found")
});
const DELETE_MEMORY_OBJECTS_EXT_PROC: Lazy<PFNGLDELETEMEMORYOBJECTSEXTPROC> = Lazy::new(|| unsafe {
	crate::egl::get_fnptr(b"glDeleteMemoryObjectsEXT\0").expect("no glDeleteMemoryObjectsEXT found")
});
const IMPORT_MEMORY_FD_EXT_PROC: Lazy<PFNGLIMPORTMEMORYFDEXTPROC> = Lazy::new(|| unsafe {
	crate::egl::get_fnptr(b"glImportMemoryFdEXT\0").expect("no glImportMemoryFdEXT found")
});

#[repr(C)]
pub enum HandleType {
	OpaqueFD = GL_HANDLE_TYPE_OPAQUE_FD_EXT as _
}

pub struct MemoryObject(libc::c_uint);
impl MemoryObject {
	pub fn new() -> Self {
		let mut id = 0;
		(*CREATE_MEMORY_OBJECTS_EXT_PROC)(1, &mut id);
		MemoryObject(id)
	}
	pub fn id(&self) -> libc::c_uint { self.0 }

	pub fn import_fd(&self, size: u64, handle_type: HandleType, fd: libc::c_int) {
		(*IMPORT_MEMORY_FD_EXT_PROC)(self.0, size, handle_type as _, fd);
	}
}
impl Drop for MemoryObject {
	fn drop(&mut self) {
		(*DELETE_MEMORY_OBJECTS_EXT_PROC)(1, &self.0)
	}
}

#[repr(C)]
pub enum ShaderType {
	Vertex = GL_VERTEX_SHADER as _,
	Fragment = GL_FRAGMENT_SHADER as _
}

pub struct Shader(libc::c_uint);
impl Shader {
	pub fn new(ty: ShaderType) -> Option<Self> {
		let id = unsafe { glCreateShader(ty as _) };
		if id == 0 { None } else { Some(Self(id)) }
	}
	pub fn source(&self, source: &str) {
		let len = source.len() as _;
		let cs = std::ffi::CString::new(source).expect("intercepted null");
		let cs_ptr = cs.as_ptr();
		unsafe { glShaderSource(self.0, 1, &cs_ptr, &len); }
	}
	pub fn compile(&self) { unsafe { glCompileShader(self.0); } }

	pub fn compile_status(&self) -> libc::c_int {
		let mut st = 0;
		unsafe { glGetShaderiv(self.0, GL_COMPILE_STATUS, &mut st) };
		st
	}
	pub fn info_log(&self) -> std::ffi::CString {
		let mut loglength = 0;
		unsafe { glGetShaderiv(self.0, GL_INFO_LOG_LENGTH, &mut loglength) };
		let mut buf = vec![0u8; loglength as _];
		unsafe { glGetShaderInfoLog(self.0, loglength, std::ptr::null_mut(), buf.as_mut_ptr() as *mut _) };
		unsafe { std::ffi::CString::from_vec_unchecked(buf) }
	}
}
impl Drop for Shader {
	fn drop(&mut self) {
		unsafe { glDeleteShader(self.0); }
	}
}

pub struct Program(libc::c_uint);
impl Program {
	pub fn new() -> Option<Self> {
		let id = unsafe { glCreateProgram() };
		if id == 0 { None } else { Some(Self(id)) }
	}
	pub fn id(&self) -> libc::c_uint { self.0 }

	pub fn attach(&self, shader: &Shader) {
		unsafe { glAttachShader(self.0, shader.0); }
	}
	pub fn link(&self) {
		unsafe { glLinkProgram(self.0); }
	}
	pub fn get_uniform_location(&self, name: &std::ffi::CStr) -> Option<usize> {
		let l = unsafe { glGetUniformLocation(self.0, name.as_ptr()) };
		if l < 0 { None } else { Some(l as _) }
	}
	pub fn get_attrib_location(&self, name: &std::ffi::CStr) -> Option<usize> {
		let l = unsafe { glGetAttribLocation(self.0, name.as_ptr()) };
		if l < 0 { None } else { Some(l as _) }
	}

	pub fn link_status(&self) -> libc::c_int {
		let mut st = 0;
		unsafe { glGetProgramiv(self.0, GL_LINK_STATUS, &mut st) };
		st
	}
	pub fn info_log(&self) -> std::ffi::CString {
		let mut loglength = 0;
		unsafe { glGetProgramiv(self.0, GL_INFO_LOG_LENGTH, &mut loglength) };
		let mut buf = vec![0u8; loglength as _];
		unsafe { glGetProgramInfoLog(self.0, loglength, std::ptr::null_mut(), buf.as_mut_ptr() as *mut _) };
		unsafe { std::ffi::CString::from_vec_unchecked(buf) }
	}
}
impl Drop for Program {
	fn drop(&mut self) {
		unsafe { glDeleteProgram(self.0); }
	}
}

pub struct VertexArray(libc::c_uint);
impl VertexArray {
	pub fn new() -> Self {
		let mut id = 0;
		unsafe { glGenVertexArrays(1, &mut id) };
		VertexArray(id)
	}
	pub fn id(&self) -> libc::c_uint { self.0 }
}
impl Drop for VertexArray {
	fn drop(&mut self) {
		unsafe { glDeleteVertexArrays(1, &self.0); }
	}
}

pub struct Buffer(libc::c_uint);
impl Buffer {
	pub fn new() -> Self {
		let mut id = 0;
		unsafe { glGenBuffers(1, &mut id) };
		Buffer(id)
	}
	pub fn id(&self) -> libc::c_uint { self.0 }
}
impl Drop for Buffer {
	fn drop(&mut self) {
		unsafe { glDeleteBuffers(1, &self.0) };
	}
}

unsafe impl crate::egl::FunctionPtr for PFN_glEGLImageTargetTexture2DOES {
	unsafe fn from_raw(raw: extern fn()) -> Self { std::mem::transmute(raw) }
}
// id creator
unsafe impl crate::egl::FunctionPtr for extern "C" fn(libc::c_int, *mut libc::c_uint) {
	unsafe fn from_raw(raw: extern fn()) -> Self { std::mem::transmute(raw) }
}
// id destroyer
unsafe impl crate::egl::FunctionPtr for extern "C" fn(libc::c_int, *const libc::c_uint) {
	unsafe fn from_raw(raw: extern fn()) -> Self { std::mem::transmute(raw) }
}
unsafe impl crate::egl::FunctionPtr for PFNGLIMPORTMEMORYFDEXTPROC {
	unsafe fn from_raw(raw: extern fn()) -> Self { std::mem::transmute(raw) }
}
unsafe impl crate::egl::FunctionPtr for PFNGLTEXSTORAGEMEM2DEXTPROC {
	unsafe fn from_raw(raw: extern fn()) -> Self { std::mem::transmute(raw) }
}

pub mod raw {
	#![allow(non_camel_case_types)]

	pub const GL_TEXTURE_2D: libc::c_uint = 0x0de1;
	pub const GL_TEXTURE_EXTERNAL_OES: libc::c_uint = 0x8d65;
	pub const GL_TEXTURE_MIN_FILTER: libc::c_uint = 0x2801;
	pub const GL_TEXTURE_MAG_FILTER: libc::c_uint = 0x2800;
	pub const GL_TEXTURE_WRAP_S: libc::c_uint = 0x2802;
	pub const GL_TEXTURE_WRAP_T: libc::c_uint = 0x2803;
	pub const GL_LINEAR: libc::c_uint = 0x2601;
	pub const GL_CLAMP_TO_EDGE: libc::c_uint = 0x812f;
	pub const GL_COLOR_BUFFER_BIT: libc::c_uint = 0x00004000;
	pub const GL_RGBA: libc::c_uint = 0x1908;
    pub const GL_RGBA8: libc::c_uint = 0x8058;
	pub const GL_UNSIGNED_INT_8_8_8_8: libc::c_uint = 0x8035;
	pub const GL_FLOAT: libc::c_uint = 0x1406;
	pub const GL_TRIANGLE_STRIP: libc::c_uint = 0x0005;
	pub const GL_VERSION: libc::c_uint = 0x1f02;
	pub const GL_EXTENSIONS: libc::c_uint = 0x1f03;

	pub const GL_FRAMEBUFFER: libc::c_uint = 0x8d40;
	pub const GL_COLOR_ATTACHMENT0: libc::c_uint = 0x8ce0;
	pub const GL_FRAMEBUFFER_COMPLETE: libc::c_uint = 0x8cd5;

	pub const GL_HANDLE_TYPE_OPAQUE_FD_EXT: libc::c_uint = 0x9586;

	pub const GL_VERTEX_SHADER: libc::c_uint = 0x8b31;
	pub const GL_FRAGMENT_SHADER: libc::c_uint = 0x8b30;
	pub const GL_COMPILE_STATUS: libc::c_uint = 0x8b81;
	pub const GL_LINK_STATUS: libc::c_uint = 0x8b82;
	pub const GL_INFO_LOG_LENGTH: libc::c_uint = 0x8b84;
	pub const GL_TEXTURE0: libc::c_uint = 0x84c0;
	pub const GL_DEBUG_OUTPUT: libc::c_uint = 0x92e0;

	pub const GL_ARRAY_BUFFER: libc::c_uint = 0x8892;
	pub const GL_STATIC_DRAW: libc::c_uint = 0x88e4;

	#[link(name = "GL")]
	extern "C" {
		pub fn glGetError() -> libc::c_uint;
		pub fn glGetString(name: libc::c_uint) -> *const libc::c_uchar;
		pub fn glGetStringi(name: libc::c_uint, index: libc::c_uint) -> *const libc::c_uchar;
		pub fn glEnable(cap: libc::c_uint);
		pub fn glGenTextures(n: libc::c_int, textures: *mut libc::c_uint);
		pub fn glDeleteTextures(n: libc::c_int, textures: *const libc::c_uint);
		pub fn glBindTexture(target: libc::c_uint, texture: libc::c_uint);
		pub fn glTexParameteri(target: libc::c_uint, pname: libc::c_uint, param: libc::c_int);
		pub fn glTexImage2D(target: libc::c_uint, level: libc::c_int, internal_format: libc::c_int, width: libc::c_int, height: libc::c_int, border: libc::c_int, format: libc::c_uint, ty: libc::c_uint, pixels: *const libc::c_void);

		pub fn glGenFramebuffers(n: libc::c_int, framebuffers: *mut libc::c_uint);
		pub fn glDeleteFramebuffers(n: libc::c_int, framebuffers: *const libc::c_uint);
		pub fn glBindFramebuffer(target: libc::c_uint, framebuffer: libc::c_uint);
		pub fn glFramebufferTexture2D(target: libc::c_uint, attachment: libc::c_uint, textarget: libc::c_uint, texture: libc::c_uint, level: libc::c_int);
		pub fn glCheckFramebufferStatus(target: libc::c_uint) -> libc::c_uint;

		pub fn glViewport(x: libc::c_int, y: libc::c_int, width: libc::c_int, height: libc::c_int);
        pub fn glClearColor(red: libc::c_float, green: libc::c_float, blue: libc::c_float, alpha: libc::c_float);
		pub fn glClear(mask: libc::c_uint);
		pub fn glDrawArrays(mode: libc::c_uint, first: libc::c_int, count: libc::c_int);
        pub fn glFlush();

		pub fn glCreateProgram() -> libc::c_uint;
		pub fn glCreateShader(ty: libc::c_uint) -> libc::c_uint;
		pub fn glDeleteProgram(program: libc::c_uint);
		pub fn glDeleteShader(shader: libc::c_uint);
		pub fn glEnableVertexAttribArray(index: libc::c_uint);
		pub fn glVertexAttribPointer(index: libc::c_uint, size: libc::c_int, ty: libc::c_uint, normalized: libc::c_uchar, stride: libc::c_int, pointer: *const libc::c_void);
		pub fn glGetAttribLocation(program: libc::c_uint, name: *const libc::c_char) -> libc::c_int;
		pub fn glGetUniformLocation(program: libc::c_uint, name: *const libc::c_char) -> libc::c_int;
		pub fn glAttachShader(program: libc::c_uint, shader: libc::c_uint);
		pub fn glCompileShader(shader: libc::c_uint);
		pub fn glLinkProgram(program: libc::c_uint);
		pub fn glShaderSource(shader: libc::c_uint, count: libc::c_int, string: *const *const libc::c_char, length: *const libc::c_int);
		pub fn glUseProgram(program: libc::c_uint);
		pub fn glUniform1i(location: libc::c_int, v0: libc::c_int);
		pub fn glGetProgramiv(program: libc::c_uint, pname: libc::c_uint, params: *mut libc::c_int);
		pub fn glGetProgramInfoLog(program: libc::c_uint, bufsize: libc::c_int, length: *mut libc::c_int, info_log: *mut libc::c_char);
		pub fn glGetShaderiv(shader: libc::c_uint, pname: libc::c_uint, params: *mut libc::c_int);
		pub fn glGetShaderInfoLog(shader: libc::c_uint, bufsize: libc::c_int, length: *mut libc::c_int, info_log: *mut libc::c_char);
		pub fn glActiveTexture(texture: libc::c_uint);

		pub fn glGenVertexArrays(n: libc::c_int, arrays: *mut libc::c_uint);
		pub fn glDeleteVertexArrays(n: libc::c_int, arrays: *const libc::c_uint);
		pub fn glBindVertexArray(array: libc::c_uint);
		pub fn glEnableVertexArrayAttrib(vaobj: libc::c_uint, index: libc::c_uint);
		pub fn glGenBuffers(n: libc::c_int, buffers: *mut libc::c_uint);
		pub fn glDeleteBuffers(n: libc::c_int, buffers: *const libc::c_uint);
		pub fn glBindBuffer(target: libc::c_uint, buffer: libc::c_uint);
		pub fn glBufferData(target: libc::c_uint, size: libc::size_t, data: *const libc::c_void, usage: libc::c_uint);
	}

	pub type PFNGLCREATEMEMORYOBJECTSEXTPROC = extern "C" fn(n: libc::c_int, memory_objects: *mut libc::c_uint);
	pub type PFNGLDELETEMEMORYOBJECTSEXTPROC = extern "C" fn(n: libc::c_int, memory_objects: *const libc::c_uint);
	pub type PFNGLTEXSTORAGEMEM2DEXTPROC = extern "C" fn(
		target: libc::c_uint, levels: libc::c_int, internal_format: libc::c_uint, width: libc::c_int, height: libc::c_int, memory: libc::c_uint, offset: u64
	);
	pub type PFNGLIMPORTMEMORYFDEXTPROC = extern "C" fn(memory: libc::c_uint, size: u64, handle_type: libc::c_uint, fd: libc::c_int);
	pub type PFN_glEGLImageTargetTexture2DOES = extern "C" fn(target: libc::c_uint, image: *mut libc::c_void);
}
use self::raw::*;
