pub async fn game_main<'x>(e: &mut peridot::Engine<'x, impl peridot::NativeLinker>) {
    loop {
        match e.next_event().await {
            peridot::Event::Shutdown => break,
            _ => (),
        }
    }
}
