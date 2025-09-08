use rendering_configuration::tokenizer;

fn main() {
    let content = std::fs::read_to_string(&std::env::args_os().nth(1).unwrap()).unwrap();

    let mut ctx = tokenizer::Context::new(&content);
    loop {
        match tokenizer::next_token(&mut ctx) {
            Some(t) => {
                println!("{t:?}");
            }
            None => {
                if !ctx.is_finished() {
                    println!("no token: left {}", ctx.src());
                }
                break;
            }
        }
    }
}
