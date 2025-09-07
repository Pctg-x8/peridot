use rendering_configuration::next_token;

fn main() {
    let content = std::fs::read_to_string(&std::env::args_os().nth(1).unwrap()).unwrap();

    let mut current: &str = &content;
    loop {
        match next_token(&current) {
            (Some(t), rest) => {
                println!("{t:?}");
                current = rest;
            }
            (None, rest) if rest.is_empty() => {
                break;
            }
            (None, rest) => {
                println!("no token: left {rest}");
                break;
            }
        }
    }
}
