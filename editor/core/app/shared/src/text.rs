pub fn prev_char_byte(input: &str, current_byte: usize) -> usize {
    let mut new_cursor_pos = current_byte.saturating_sub(1);
    while new_cursor_pos > 0 && !input.is_char_boundary(new_cursor_pos) {
        new_cursor_pos -= 1;
    }

    new_cursor_pos
}

pub fn next_char_byte(input: &str, current_byte: usize) -> usize {
    if let Some(c) = input.get(current_byte..) {
        // optimal way
        return current_byte + c.chars().next().map_or(0, char::len_utf8);
    }

    let mut new_cursor_pos = current_byte.saturating_add(1).min(input.len());
    while new_cursor_pos < input.len() && !input.is_char_boundary(new_cursor_pos) {
        new_cursor_pos += 1;
    }

    new_cursor_pos
}
