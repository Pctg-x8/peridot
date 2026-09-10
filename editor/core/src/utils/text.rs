#[cfg(all(not(windows), not(target_os = "macos")))]
pub fn generic_word_segments(input: &str) -> Vec<&str> {
    use peridot_tp_budoux as budoux;
    use unicode_segmentation::UnicodeSegmentation;

    // UnicodeSegmentation+BudouX generic fallback

    let mut words = Vec::new();
    let mut chars = input.chars();
    let mut is_budou_cluster = false;
    let mut same_cluster_range = 0..0;
    let mut cb = 0;
    while let Some(c) = chars.next() {
        let is_budou_cluster_c = crate::utils::is_budou_cluster_char(c);
        if is_budou_cluster != is_budou_cluster_c {
            // breaking method boundary
            if !same_cluster_range.is_empty() {
                if !is_budou_cluster {
                    words.extend(input[same_cluster_range.clone()].split_word_bounds())
                } else {
                    words.extend(budoux::parse(
                        &budoux::embedded::ja_knbc::MODEL,
                        &input[same_cluster_range.clone()],
                    ))
                }
            }

            is_budou_cluster = is_budou_cluster_c;
            same_cluster_range = cb..cb;
        }

        same_cluster_range.end += c.len_utf8();
        cb += c.len_utf8();
    }
    if !same_cluster_range.is_empty() {
        if !is_budou_cluster {
            words.extend(input[same_cluster_range.clone()].split_word_bounds())
        } else {
            words.extend(budoux::parse(
                &budoux::embedded::ja_knbc::MODEL,
                &input[same_cluster_range.clone()],
            ))
        }
    }

    words
}
