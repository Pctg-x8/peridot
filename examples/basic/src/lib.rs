
use std::marker::PhantomData;

pub struct Empty<PL: peridot::PlatformLinker>(PhantomData<*const PL>);
impl<PL: peridot::PlatformLinker> Empty<PL> {
    pub const NAME: &'static str = "Peridot Examples - Basic";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<PL: peridot::PlatformLinker> peridot::EngineEvents<PL> for Empty<PL> {
    fn init(_e: &peridot::Engine<Self, PL>) -> Self { Empty(PhantomData) }
}
