use std::marker::PhantomData;

pub struct Game<NL>(PhantomData<*const NL>);
impl<NL: peridot::NativeLinker> peridot::FeatureRequests for Game<NL> {}
impl<NL: peridot::NativeLinker> peridot::EngineEvents<NL> for Game<NL> {
    fn init(_e: &mut peridot::Engine<NL>) -> Self {
        Game(PhantomData)
    }
}
