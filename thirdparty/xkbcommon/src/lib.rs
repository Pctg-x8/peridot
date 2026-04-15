use bitflags::bitflags;
use core::ptr::NonNull;

pub mod ffi;
pub use self::ffi::xkb_keycode_t as Keycode;
pub use self::ffi::xkb_layout_index_t as LayoutIndex;
pub use self::ffi::xkb_mod_mask_t as ModMask;

bitflags! {
    #[derive(Clone, Copy)]
    pub struct ContextFlags : ffi::xkb_context_flags {
        const NO_FLAGS = ffi::XKB_CONTEXT_NO_FLAGS;
        const NO_DEFAULT_INCLUDES = ffi::XKB_CONTEXT_NO_DEFAULT_INCLUDES;
        const NO_ENVIRONMENT_NAMES = ffi::XKB_CONTEXT_NO_ENVIRONMENT_NAMES;
        const NO_SECURE_GETENV = ffi::XKB_CONTEXT_NO_SECURE_GETENV;
    }
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct KeymapCompileFlags : ffi::xkb_keymap_compile_flags {
        const NO_FLAGS = ffi::XKB_KEYMAP_COMPILE_NO_FLAGS;
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct StateComponent : ffi::xkb_state_component {
        const MODS_DEPRESSED = ffi::XKB_STATE_MODS_DEPRESSED;
        const MODS_LATCHED = ffi::XKB_STATE_MODS_LATCHED;
        const MODS_LOCKED = ffi::XKB_STATE_MODS_LOCKED;
        const MODS_EFFECTIVE = ffi::XKB_STATE_MODS_EFFECTIVE;
        const LAYOUT_DEPRESSED = ffi::XKB_STATE_LAYOUT_DEPRESSED;
        const LAYOUT_LATCHED = ffi::XKB_STATE_LAYOUT_LATCHED;
        const LAYOUT_LOCKED = ffi::XKB_STATE_LAYOUT_LOCKED;
        const LAYOUT_EFFECTIVE = ffi::XKB_STATE_LAYOUT_EFFECTIVE;
        const LEDS = ffi::XKB_STATE_LEDS;
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum KeymapFormat {
    TextV1 = ffi::XKB_KEYMAP_FORMAT_TEXT_V1,
    TextV2 = ffi::XKB_KEYMAP_FORMAT_TEXT_V2,
}

#[repr(transparent)]
pub struct Context(NonNull<ffi::xkb_context>);
impl Drop for Context {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            ffi::xkb_context_unref(self.0.as_ptr());
        }
    }
}
impl Clone for Context {
    #[inline(always)]
    fn clone(&self) -> Self {
        unsafe {
            ffi::xkb_context_ref(self.0.as_ptr());
        }
        Self(self.0)
    }
}
impl Context {
    #[inline(always)]
    pub fn new(flags: ContextFlags) -> Option<Self> {
        NonNull::new(unsafe { ffi::xkb_context_new(flags.bits()) }).map(Self)
    }
}

#[repr(transparent)]
pub struct Keymap(NonNull<ffi::xkb_keymap>);
impl Drop for Keymap {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            ffi::xkb_keymap_unref(self.0.as_ptr());
        }
    }
}
impl Clone for Keymap {
    #[inline(always)]
    fn clone(&self) -> Self {
        unsafe {
            ffi::xkb_keymap_ref(self.0.as_ptr());
        }
        Self(self.0)
    }
}
impl Keymap {
    #[inline(always)]
    pub fn from_buffer(
        context: &Context,
        buffer: &[u8],
        format: KeymapFormat,
        flags: KeymapCompileFlags,
    ) -> Option<Self> {
        NonNull::new(unsafe {
            ffi::xkb_keymap_new_from_buffer(
                context.0.as_ptr(),
                buffer.as_ptr().cast(),
                buffer.len(),
                format as _,
                flags.bits(),
            )
        })
        .map(Self)
    }

    #[inline(always)]
    pub fn mod_index(&self, name: &core::ffi::CStr) -> Option<u32> {
        match unsafe { ffi::xkb_keymap_mod_get_index(self.0.as_ptr(), name.as_ptr()) } {
            r if r == ffi::XKB_MOD_INVALID => None,
            r => Some(r),
        }
    }
}

#[repr(transparent)]
pub struct State(NonNull<ffi::xkb_state>);
impl Drop for State {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            ffi::xkb_state_unref(self.0.as_ptr());
        }
    }
}
impl Clone for State {
    #[inline(always)]
    fn clone(&self) -> Self {
        unsafe {
            ffi::xkb_state_ref(self.0.as_ptr());
        }
        Self(self.0)
    }
}
impl State {
    #[inline(always)]
    pub fn new(keymap: &Keymap) -> Option<Self> {
        NonNull::new(unsafe { ffi::xkb_state_new(keymap.0.as_ptr()) }).map(Self)
    }

    #[inline(always)]
    pub fn update_mask(
        &mut self,
        depressed_mode: ModMask,
        latched_mods: ModMask,
        locked_mods: ModMask,
        depressed_layout: LayoutIndex,
        latched_layout: LayoutIndex,
        locked_layout: LayoutIndex,
    ) -> StateComponent {
        StateComponent::from_bits_retain(unsafe {
            ffi::xkb_state_update_mask(
                self.0.as_ptr(),
                depressed_mode,
                latched_mods,
                locked_mods,
                depressed_layout,
                latched_layout,
                locked_layout,
            )
        })
    }

    #[inline(always)]
    pub fn key_get_utf8(&self, key: Keycode, buf: &mut [core::mem::MaybeUninit<u8>]) -> usize {
        unsafe {
            ffi::xkb_state_key_get_utf8(self.0.as_ptr(), key, buf.as_mut_ptr().cast(), buf.len())
                as _
        }
    }

    #[inline(always)]
    pub fn mod_index_is_active(&self, mod_index: u32, state: StateComponent) -> bool {
        unsafe { ffi::xkb_state_mod_index_is_active(self.0.as_ptr(), mod_index, state.bits()) == 1 }
    }
}
