use rendering_configuration::{
    syntax::{ParserState, ToplevelElement},
    tokenizer,
};

fn main() {
    let content = std::fs::read_to_string(&std::env::args_os().nth(1).unwrap()).unwrap();

    let ctx = tokenizer::Context::new(&content);
    let mut state = ParserState::new(ctx);
    while !state.is_finished() {
        let Some(top) = ToplevelElement::parse(&mut state) else {
            break;
        };

        println!("top: {top:#?}");
    }
}
