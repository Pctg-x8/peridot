use rendering_configuration::{
    codegen::RenderingConfiguration,
    syntax::{ParserState, ToplevelElement},
    tokenizer,
};

fn main() {
    let content = std::fs::read_to_string(&std::env::args_os().nth(1).unwrap()).unwrap();

    let ctx = tokenizer::Context::new(&content);
    let mut state = ParserState::new(ctx);
    let mut toplevel_elements = Vec::new();
    while !state.is_finished() {
        let Some(top) = ToplevelElement::parse(&mut state) else {
            break;
        };

        toplevel_elements.push(top);
    }

    let rc = RenderingConfiguration::new(toplevel_elements);
    // println!("asset: {rc:#?}");

    let prelude = rc.gen_vk_prelude();
    let code = rc.gen_code_for_pass("Visibility.Lighting");

    println!("{prelude}\n{code}");
}
