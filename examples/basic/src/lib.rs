
use std::marker::PhantomData;

pub struct Game<NL>(PhantomData<*const NL>);
impl<NL: peridot::NativeLinker> Game<NL>
{
    pub const NAME: &'static str = "Peridot Examples - Basic";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<NL: peridot::NativeLinker> peridot::FeatureRequests for Game<NL> {}
impl<NL: peridot::NativeLinker> peridot::EngineEvents<NL> for Game<NL>
{
    fn init(_e: &mut peridot::Engine<NL>) -> Self { Game(PhantomData) }
}
