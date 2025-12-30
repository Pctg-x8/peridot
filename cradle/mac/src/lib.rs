use bedrock as br;
use br::{InstanceChild, PhysicalDevice, SurfaceCreateInfo, VkHandle};
use core::future::Future;
use peridot::mthelper::SharedRef;
use std::ffi::CStr;
use std::pin::Pin;

use crate::asset::PlatformAssetLoader;
use crate::audio::AudioEngineContext;
use crate::native_interface::{
    acquire_layer_size, give_game_driver_callbacks, nsapp_reply_should_terminate,
    nsscreen_backing_scale_factor, obtain_mouse_pointer_position, schedule_usercode_task_polling,
    teardown_audio, GameDriverCallbacks, SwiftContext, KEYMOD_CAPSLOCK, KEYMOD_COMMAND,
    KEYMOD_CONTROL, KEYMOD_OPTION, KEYMOD_SHIFT,
};

mod asset;
mod audio;
mod log;
mod native_interface;
mod userlib;

struct Surface {
    gfx_device: peridot::VulkanGfx,
    handle: br::vk::VkSurfaceKHR,
}
impl Drop for Surface {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_surface(
                self.gfx_device.instance().native_ptr(),
                self.handle,
                None,
            );
        }
    }
}
impl br::VkHandle for Surface {
    type Handle = br::vk::VkSurfaceKHR;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

pub struct Presenter {
    layer_ptr: *mut core::ffi::c_void,
    sc: peridot::IntegratedSwapchain<Surface>,
}
unsafe impl Sync for Presenter {}
unsafe impl Send for Presenter {}
impl Presenter {
    fn new(layer_ptr: *mut core::ffi::c_void, g: &peridot::Graphics) -> Self {
        let obj = Surface {
            handle: unsafe {
                br::MetalSurfaceCreateInfo::new(layer_ptr.cast_const())
                    .execute(g.device().instance(), None)
                    .expect("Failed to create surface")
            },
            gfx_device: g.device().clone(),
        };
        let support = g
            .device()
            .surface_support(&obj)
            .expect("Failed to query Surface Support");
        if !support {
            panic!("Vulkan Rendering is not supported by this adapter.");
        }

        let (width, height) = unsafe { acquire_layer_size(layer_ptr) };

        Presenter {
            layer_ptr,
            sc: peridot::IntegratedSwapchain::new(g, obj, peridot::math::Vector2(width, height)),
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat {
        self.sc.format()
    }

    fn back_buffer_count(&self) -> usize {
        self.sc.back_buffer_count()
    }

    fn back_buffer_size(&self) -> peridot::math::Vector2<u32> {
        self.sc.back_buffer_size()
    }

    fn back_buffer<'a>(&'a self, index: usize) -> Option<br::VkHandleRef<'a, br::vk::VkImage>> {
        self.sc.back_buffer(index)
    }

    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_back_buffer_layout()
    }

    fn emit_initialize_back_buffer_commands<'r>(
        &self,
        recorder: br::CmdRecord<'r>,
    ) -> br::CmdRecord<'r> {
        self.sc.emit_initialize_back_buffer_commands(recorder)
    }

    fn next_back_buffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_back_buffer_index()
    }

    fn render_and_present<'s, 'r>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::VkHandleMut<Handle = br::vk::VkFence>,
        back_buffer_index: u32,
        render_submission: peridot::SubmissionBatchBuilder<'r>,
        update_submission: Option<peridot::SubmissionBatchBuilder<'r>>,
    ) -> br::Result<()>
    where
        's: 'r,
    {
        self.sc.render_and_present(
            g,
            last_render_fence,
            back_buffer_index,
            render_submission,
            update_submission,
        )
    }

    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<u32>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs re-initializing back-buffer resource
        true
    }

    fn current_geometry_extent(&self) -> peridot::math::Vector2<u32> {
        let (w, h) = unsafe { acquire_layer_size(self.layer_ptr) };

        peridot::math::Vector2(w, h)
    }
}

pub struct NativeLink {
    rt_view: *mut core::ffi::c_void,
    al: PlatformAssetLoader,
}
unsafe impl Sync for NativeLink {}
unsafe impl Send for NativeLink {}
impl NativeLink {
    pub fn new(rt_view: *mut core::ffi::c_void) -> Self {
        NativeLink {
            al: PlatformAssetLoader::new(),
            rt_view,
        }
    }
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type Presenter = Presenter;

    fn instance_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_surface", c"VK_MVK_macos_surface"]
    }
    fn device_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_swapchain"]
    }

    fn asset_loader(&self) -> &PlatformAssetLoader {
        &self.al
    }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter {
        Presenter::new(self.rt_view, g)
    }

    fn rendering_precision(&self) -> f32 {
        unsafe { nsscreen_backing_scale_factor() }
    }
}

type Engine<'q> = peridot::Engine<'q, NativeLink>;

const USERCODE_WAKER_VTABLE: &'static core::task::RawWakerVTable = &core::task::RawWakerVTable::new(
    |ptr| core::task::RawWaker::new(ptr, USERCODE_WAKER_VTABLE),
    |ptr| /*println!("game_main wake")*/{unsafe { schedule_usercode_task_polling(ptr.cast_mut().cast::<core::ffi::c_void>()); }},
    |ptr| /*println!("game_main wake by ref")*/{ unsafe { schedule_usercode_task_polling(ptr.cast_mut().cast::<core::ffi::c_void>()); } },
    |_| {},
);

pub struct GameDriver {
    ex_input: peridot::InputProcess,
    frame_timing_sender: async_std::channel::Sender<()>,
    event_sender: async_std::channel::Sender<peridot::EngineEvent>,
    event_queue: peridot::EventQueue,
    _pinned: core::marker::PhantomPinned,
}

pub struct AppInternalState {
    pub event_queue: peridot::EventQueue,
}

fn launch_f<'f, F>(
    swift_context: SwiftContext,
    v: *mut core::ffi::c_void,
    launch_usercode: impl FnOnce(Engine<'f>) -> F,
) where
    F: Future<Output = ()> + 'f,
{
    log::init_logging();

    let (event_sender, event_receiver) = async_std::channel::unbounded::<peridot::EngineEvent>();
    let (_, frame_timing_receiver) = async_std::channel::bounded::<()>(1);

    let state = Box::new(AppInternalState {
        event_queue: peridot::EventQueue::new(),
    });
    let state_ptr = Box::into_raw(state);
    let state_lifetime_extended: &'f AppInternalState = unsafe { &*state_ptr };

    let mut engine = Engine::new(
        userlib::APP_IDENTIFIER,
        userlib::APP_VERSION,
        NativeLink::new(v),
        unsafe { core::mem::MaybeUninit::zeroed().assume_init() },
        (event_sender.clone(), event_receiver),
        frame_timing_receiver,
        &state_lifetime_extended.event_queue,
    );
    let nih = Box::new(NativeInputHandler::new(v));
    engine.input().set_nativelink(nih);
    let mut audio_engine = Box::pin(AudioEngineContext::new(engine.audio_mixer().clone()));
    audio_engine.as_mut().connect(swift_context);
    engine.post_init();
    let input = engine.input().clone();

    let usercode_waker =
        unsafe { core::task::Waker::new(swift_context.cast::<()>(), &USERCODE_WAKER_VTABLE) };
    let usercode = Box::pin(launch_usercode(engine));

    struct GameDriverContext<F> {
        usercode: Pin<Box<F>>,
        usercode_waker: core::task::Waker,
        state: Box<AppInternalState>,
        input: peridot::InputProcess,
        #[allow(dead_code)]
        audio_engine: Pin<Box<AudioEngineContext>>,
    }
    extern "C" fn game_driver_terminate<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        swift_context: SwiftContext,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.state.event_queue.enqueue(peridot::Event::Shutdown);
        loop {
            if ctx
                .usercode
                .as_mut()
                .poll(&mut core::task::Context::from_waker(&ctx.usercode_waker))
                .is_ready()
            {
                break;
            }
        }

        unsafe {
            teardown_audio(swift_context);
        }
        drop(unsafe { Box::from_raw(ctx) });

        unsafe {
            nsapp_reply_should_terminate();
        }
    }
    extern "C" fn game_driver_update<F: core::future::Future>(ctx: *mut core::ffi::c_void) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.state.event_queue.enqueue(peridot::Event::NextFrame);
    }
    extern "C" fn game_driver_resize<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        w: u32,
        h: u32,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.state
            .event_queue
            .enqueue(peridot::Event::Resize(peridot::math::Vector2(w, h)));
    }
    extern "C" fn game_driver_handle_character_keydown<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        character: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.input.dispatch_button_event(
            peridot::NativeButtonInput::Character((character as char).to_ascii_uppercase()),
            true,
        );
    }
    extern "C" fn game_driver_handle_character_keyup<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        character: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.input.dispatch_button_event(
            peridot::NativeButtonInput::Character((character as char).to_ascii_uppercase()),
            false,
        );
    }
    extern "C" fn game_driver_handle_keymod_down<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        code: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        let code_to_bty = match code {
            KEYMOD_SHIFT => peridot::NativeButtonInput::LeftShift,
            KEYMOD_OPTION => peridot::NativeButtonInput::LeftAlt,
            KEYMOD_CONTROL => peridot::NativeButtonInput::LeftControl,
            KEYMOD_COMMAND => peridot::NativeButtonInput::LeftMeta,
            KEYMOD_CAPSLOCK => peridot::NativeButtonInput::CapsLock,
            _ => return,
        };
        ctx.input.dispatch_button_event(code_to_bty, true);
    }
    extern "C" fn game_driver_handle_keymod_up<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        code: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        let code_to_bty = match code {
            KEYMOD_SHIFT => peridot::NativeButtonInput::LeftShift,
            KEYMOD_OPTION => peridot::NativeButtonInput::LeftAlt,
            KEYMOD_CONTROL => peridot::NativeButtonInput::LeftControl,
            KEYMOD_COMMAND => peridot::NativeButtonInput::LeftMeta,
            KEYMOD_CAPSLOCK => peridot::NativeButtonInput::CapsLock,
            _ => return,
        };
        ctx.input.dispatch_button_event(code_to_bty, false);
    }
    extern "C" fn game_driver_handle_mouse_button_down<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        index: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.input
            .dispatch_button_event(peridot::NativeButtonInput::Mouse(index as _), true);
    }
    extern "C" fn game_driver_handle_mouse_button_up<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        index: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.input
            .dispatch_button_event(peridot::NativeButtonInput::Mouse(index as _), false);
    }
    extern "C" fn game_driver_report_mouse_move_abs<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        x: f32,
        y: f32,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        let scale = unsafe { nsscreen_backing_scale_factor() };
        ctx.input
            .dispatch_analog_event(peridot::NativeAnalogInput::MouseX, x * scale, true);
        ctx.input
            .dispatch_analog_event(peridot::NativeAnalogInput::MouseY, y * scale, true);
    }
    extern "C" fn game_driver_poll_usercode_task<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
    ) {
        let ctx = unsafe { &mut *ctx.cast::<GameDriverContext<F>>() };

        let r = ctx
            .usercode
            .as_mut()
            .poll(&mut core::task::Context::from_waker(&ctx.usercode_waker));
        if r.is_ready() {
            tracing::warn!("Usercode task terminated?");
        }
    }
    let cbs: &'static GameDriverCallbacks = &GameDriverCallbacks {
        terminate: game_driver_terminate::<F>,
        update: game_driver_update::<F>,
        resize: game_driver_resize::<F>,
        handle_character_keydown: game_driver_handle_character_keydown::<F>,
        handle_character_keyup: game_driver_handle_character_keyup::<F>,
        handle_keymod_down: game_driver_handle_keymod_down::<F>,
        handle_keymod_up: game_driver_handle_keymod_up::<F>,
        handle_mouse_button_down: game_driver_handle_mouse_button_down::<F>,
        handle_mouse_button_up: game_driver_handle_mouse_button_up::<F>,
        report_mouse_move_abs: game_driver_report_mouse_move_abs::<F>,
        poll_usercode_task: game_driver_poll_usercode_task::<F>,
    };
    let context_ptr = Box::into_raw(Box::new(GameDriverContext {
        usercode,
        usercode_waker,
        state: unsafe { Box::from_raw(state_ptr) },
        input,
        audio_engine,
    }));
    unsafe { give_game_driver_callbacks(swift_context, cbs, context_ptr as _) }

    // execute initial process
    game_driver_poll_usercode_task::<F>(context_ptr.cast::<core::ffi::c_void>());
}

struct NativeInputHandler {
    rt_view: *mut core::ffi::c_void,
}
unsafe impl Sync for NativeInputHandler {}
unsafe impl Send for NativeInputHandler {}
impl NativeInputHandler {
    fn new(rt_view: *mut core::ffi::c_void) -> Self {
        NativeInputHandler { rt_view }
    }
}
impl peridot::NativeInput for NativeInputHandler {
    fn get_pointer_position(&self, index: u32) -> Option<(f32, f32)> {
        if index == 0 {
            Some(unsafe { obtain_mouse_pointer_position(self.rt_view).unwrap_or((0.0, 0.0)) })
        } else {
            None
        }
    }
}
