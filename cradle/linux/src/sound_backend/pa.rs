//! PulseAudio Sound Backend

use pulseaudio_rs as pa;
use std::sync::{Arc, RwLock};
use std::pin::Pin;

struct AudioDataWriter(Arc<RwLock<peridot::audio::Mixer>>);
impl pa::stream::WriteRequestHandler for AudioDataWriter
{
	fn callback(&mut self, stream: &mut pa::StreamRef, nbytes: usize)
	{
		let mut rem = nbytes;
		while rem > 0
		{
			let (bufptr, len) = stream.begin_write(rem).expect("BeginWrite failed!");
			let buf = unsafe { std::slice::from_raw_parts_mut(bufptr as *mut f32, len >> 2) };
			for b in buf.iter_mut() { *b = 0.0; }
			self.0.write().expect("Mixer Write Failed!").process(buf);
			stream.write_slice(&buf, None).expect("Writing Slice failed!");
			rem -= len;
		}
	}
}

pub struct NativeAudioEngine
{
	mlp: pa::mainloop::Threaded,
	context: pa::Context,
	stream: pa::Stream,
	_wh: Pin<Box<AudioDataWriter>>
}
impl NativeAudioEngine
{
	pub async fn new(mixer: &Arc<RwLock<peridot::audio::Mixer>>) -> Self
	{
		info!("Starting AudioEngine via PulseAudio......");

		let mut mlp = pa::mainloop::Threaded::new().expect("Failed to initialize PulseAudio Mainloop");
		mlp.start().expect("Failed to start PulseAudio Mainloop");
		let mut context = pa::Context::new(&mlp, "Peridot::NativeAudioEngine")
			.expect("Failed to initialize PulseAudio context");
		context.connect(None, pa::context::NOFLAGS, None).expect("Failed to connect PulseAudio server");
		context.await_state_until(pa::context::State::Ready).await;

		let spec = pa::SampleSpec
		{
			format: pa::SampleFormat::FLOAT32LE as _,
			rate: 44100,
			channels: 2
		};
		let mut stream = context.new_stream("Peridot::NativeAudioEngine::Output", &spec, None)
			.expect("Failed to initialize playback stream");
		let mut writer = Box::pin(AudioDataWriter(mixer.clone()));
		stream.set_write_request_callback(writer.as_mut());
		let flags = pa::stream::AUTO_TIMING_UPDATE |
			pa::stream::FIX_FORMAT |
			pa::stream::FIX_RATE |
			pa::stream::FIX_CHANNELS;
		stream.connect_playback(None, None, flags, None, None).expect("Failed to connect playback stream");
		stream.await_state_until(pa::stream::State::Ready).await;
		info!("PulseAudio Sink Device = {}", stream.device_name());

		trace!("Done!");
		NativeAudioEngine
		{
			mlp,
			context,
			stream,
			_wh: writer
		}
	}
}
impl Drop for NativeAudioEngine
{
	fn drop(&mut self)
	{
		self.stream.disconnect();
		self.mlp.stop();
	}
}
