
use std::marker::PhantomData;

pub struct Empty<PL: peridot::NativeLinker>(PhantomData<*const PL>);
impl<PL: peridot::NativeLinker> Empty<PL>
{
    pub const NAME: &'static str = "Peridot Examples - Basic";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<PL: peridot::NativeLinker> peridot::FeatureRequests for Empty<PL> {}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Empty<PL>
{
    fn init(_e: &mut peridot::Engine<PL>) -> Self { Empty(PhantomData) }
}
