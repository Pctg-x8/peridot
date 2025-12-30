//! Swift Linking

pub const KEYMOD_SHIFT: u8 = 1;
pub const KEYMOD_OPTION: u8 = 2;
pub const KEYMOD_CONTROL: u8 = 3;
pub const KEYMOD_COMMAND: u8 = 4;
pub const KEYMOD_CAPSLOCK: u8 = 5;

#[repr(C)]
pub struct GameDriverCallbacks {
    pub terminate: extern "C" fn(*mut core::ffi::c_void, SwiftContext),
    pub update: extern "C" fn(*mut core::ffi::c_void),
    pub resize: extern "C" fn(*mut core::ffi::c_void, w: u32, h: u32),
    pub handle_character_keydown: extern "C" fn(*mut core::ffi::c_void, character: u8),
    pub handle_character_keyup: extern "C" fn(*mut core::ffi::c_void, character: u8),
    pub handle_keymod_down: extern "C" fn(*mut core::ffi::c_void, code: u8),
    pub handle_keymod_up: extern "C" fn(*mut core::ffi::c_void, code: u8),
    pub handle_mouse_button_down: extern "C" fn(*mut core::ffi::c_void, index: u8),
    pub handle_mouse_button_up: extern "C" fn(*mut core::ffi::c_void, index: u8),
    pub report_mouse_move_abs: extern "C" fn(*mut core::ffi::c_void, x: f32, y: f32),
    pub poll_usercode_task: extern "C" fn(*mut core::ffi::c_void),
}

pub type SwiftContext = *mut core::ffi::c_void;

pub type AudioFormatCallback =
    extern "C" fn(context: *mut core::ffi::c_void, channels: u32, sample_rate: core::ffi::c_double);
pub type AudioRenderCallback = extern "C" fn(
    context: *mut core::ffi::c_void,
    frame_count: u32,
    audio_buffer: *mut core::ffi::c_void, /* AudioBufferList */
) -> u8;

unsafe extern "C" {
    pub fn nslog_utf8(bytes: *const u8, length: usize);

    fn ni_acquire_layer_size(
        layer_ptr: *const core::ffi::c_void,
        width: *mut u32,
        height: *mut u32,
    );

    pub fn nsapp_reply_should_terminate();
    pub fn nsbundle_path_for_resource(
        path: *const u8,
        path_length: usize,
        ext: *const u8,
        ext_lenght: usize,
        out_path: *mut u8,
        out_path_length: *mut usize,
    ) -> bool;
    pub fn nsscreen_backing_scale_factor() -> f32;
    fn ni_obtain_mouse_pointer_position(
        rt_view: *const core::ffi::c_void,
        x: *mut f32,
        y: *mut f32,
    ) -> bool;

    pub fn give_game_driver_callbacks(
        swift_context: SwiftContext,
        callbacks: *const GameDriverCallbacks,
        aux_ptr: *mut core::ffi::c_void,
    );

    pub fn schedule_usercode_task_polling(swift_context: SwiftContext);

    pub fn launch_audio(
        swift_context: SwiftContext,
        callback_context: *mut core::ffi::c_void,
        format_callback: AudioFormatCallback,
        render_callback: AudioRenderCallback,
    );
    pub fn teardown_audio(swift_context: SwiftContext);
}

#[inline]
pub unsafe fn acquire_layer_size(layer: *const core::ffi::c_void) -> (u32, u32) {
    let mut w = core::mem::MaybeUninit::uninit();
    let mut h = core::mem::MaybeUninit::uninit();
    unsafe {
        ni_acquire_layer_size(layer, w.as_mut_ptr(), h.as_mut_ptr());
    }

    unsafe { (w.assume_init(), h.assume_init()) }
}

#[inline]
pub unsafe fn obtain_mouse_pointer_position(view: *const core::ffi::c_void) -> Option<(f32, f32)> {
    let mut x = core::mem::MaybeUninit::uninit();
    let mut y = core::mem::MaybeUninit::uninit();
    let acquired =
        unsafe { ni_obtain_mouse_pointer_position(view, x.as_mut_ptr(), y.as_mut_ptr()) };

    acquired.then(|| unsafe { (x.assume_init(), y.assume_init()) })
}

#[no_mangle]
pub extern "C" fn launch_game(swift_context: SwiftContext, rt_layer: *mut core::ffi::c_void) {
    crate::launch_f(swift_context, rt_layer, |mut engine| async move {
        engine.internal_native_link_mut().al.post_init().await;
        crate::userlib::game_main(&mut engine).await;
    });
}

#[no_mangle]
pub extern "C" fn captionbar_text(length: *mut usize) -> *const core::ffi::c_char {
    unsafe {
        *length = crate::userlib::APP_TITLE.len();
    }

    crate::userlib::APP_TITLE.as_ptr().cast()
}
