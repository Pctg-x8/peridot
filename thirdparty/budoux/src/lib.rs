//! simple port from budoux(source-repo)/budoux/parser.py

use std::collections::HashMap;

pub trait Model {
    fn base_score(&self) -> f32;
    fn score(&self, feature: Feature, sentence: &str) -> i32;
}
impl<T> Model for &'_ T
where
    T: Model,
{
    #[inline(always)]
    fn base_score(&self) -> f32 {
        T::base_score(self)
    }

    #[inline(always)]
    fn score(&self, feature: Feature, sentence: &str) -> i32 {
        T::score(self, feature, sentence)
    }
}

pub struct JsonModel {
    score_map: HashMap<String, HashMap<String, i32>>,
    base_score: f32,
}
impl JsonModel {
    pub fn new(score_map: HashMap<String, HashMap<String, i32>>) -> Self {
        let base_score = -score_map.values().flat_map(|x| x.values()).sum::<i32>() as f32 * 0.5;

        Self {
            score_map,
            base_score,
        }
    }
}
impl Model for JsonModel {
    #[inline(always)]
    fn base_score(&self) -> f32 {
        self.base_score
    }

    #[inline(always)]
    fn score(&self, feature: Feature, sentence: &str) -> i32 {
        let feature_str = match feature {
            Feature::UW1 => "UW1",
            Feature::UW2 => "UW2",
            Feature::UW3 => "UW3",
            Feature::UW4 => "UW4",
            Feature::UW5 => "UW5",
            Feature::UW6 => "UW6",
            Feature::BW1 => "BW1",
            Feature::BW2 => "BW2",
            Feature::BW3 => "BW3",
            Feature::TW1 => "TW1",
            Feature::TW2 => "TW2",
            Feature::TW3 => "TW3",
            Feature::TW4 => "TW4",
        };

        self.score_map
            .get(feature_str)
            .and_then(|feature_map| feature_map.get(sentence))
            .copied()
            .unwrap_or(0)
    }
}

#[cfg(feature = "embedded")]
pub mod embedded;
#[cfg(feature = "embedded")]
pub use self::embedded::EmbeddedModel;

#[derive(Debug, Clone, Copy)]
pub enum Feature {
    UW1,
    UW2,
    UW3,
    UW4,
    UW5,
    UW6,
    BW1,
    BW2,
    BW3,
    TW1,
    TW2,
    TW3,
    TW4,
}

/// original version
pub fn parse<'s>(model: &(impl Model + ?Sized), sentence: &'s str) -> Vec<&'s str> {
    if sentence.is_empty() {
        return Vec::new();
    }

    let base_score = model.base_score();
    let sentence_char_indices = sentence
        .char_indices()
        .map(|(a, _)| a)
        .chain(core::iter::once(sentence.len()))
        .collect::<Vec<_>>();
    let mut chunks = vec![0..sentence_char_indices[1]];
    for i in 1..sentence_char_indices.len() - 1 {
        let mut score = base_score;
        if i > 2 {
            score += model.score(
                Feature::UW1,
                &sentence[sentence_char_indices[i - 3]..sentence_char_indices[i - 2]],
            ) as f32;
        }
        if i > 1 {
            score += model.score(
                Feature::UW2,
                &sentence[sentence_char_indices[i - 2]..sentence_char_indices[i - 1]],
            ) as f32;
        }
        score += model.score(
            Feature::UW3,
            &sentence[sentence_char_indices[i - 1]..sentence_char_indices[i]],
        ) as f32;
        score += model.score(
            Feature::UW4,
            &sentence[sentence_char_indices[i]..sentence_char_indices[i + 1]],
        ) as f32;
        if i + 1 < sentence_char_indices.len() {
            score += model.score(Feature::UW5, &sentence[sentence_char_indices[i + 1]..]) as f32;
        }
        if i + 2 < sentence_char_indices.len() {
            score += model.score(Feature::UW6, &sentence[sentence_char_indices[i + 2]..]) as f32;
        }

        if i > 1 {
            score += model.score(
                Feature::BW1,
                &sentence[sentence_char_indices[i - 2]..sentence_char_indices[i - 1]],
            ) as f32;
        }
        score += model.score(
            Feature::BW2,
            &sentence[sentence_char_indices[i - 1]..sentence_char_indices[i]],
        ) as f32;
        if i + 1 < sentence_char_indices.len() {
            score += model.score(Feature::BW3, &sentence[sentence_char_indices[i]..]) as f32;
        }

        if i > 2 {
            score += model.score(
                Feature::TW1,
                &sentence[sentence_char_indices[i - 3]..sentence_char_indices[i - 2]],
            ) as f32;
        }
        if i > 1 {
            score += model.score(
                Feature::TW2,
                &sentence[sentence_char_indices[i - 2]..sentence_char_indices[i + 1]],
            ) as f32;
        }
        if i + 1 < sentence_char_indices.len() {
            score += model.score(Feature::TW3, &sentence[sentence_char_indices[i - 1]..]) as f32;
        }
        if i + 2 < sentence_char_indices.len() {
            score += model.score(Feature::TW4, &sentence[sentence_char_indices[i]..]) as f32;
        }

        if score > 0.0 {
            chunks.push(sentence_char_indices[i]..sentence_char_indices[i + 1]);
        } else {
            chunks.last_mut().expect("empty?").end = sentence_char_indices[i + 1];
        }
    }

    chunks.into_iter().map(|x| &sentence[x]).collect()
}

pub fn parse_indices<'s>(model: &(impl Model + ?Sized), sentence: &'s str) -> Vec<usize> {
    if sentence.is_empty() {
        return Vec::new();
    }

    let base_score = model.base_score();
    let sentence_char_indices = sentence
        .char_indices()
        .map(|(a, _)| a)
        .chain(core::iter::once(sentence.len()))
        .collect::<Vec<_>>();
    let mut chunk_heads = vec![0];
    let mut current_chunk_tail = sentence_char_indices[1];
    for i in 1..sentence_char_indices.len() - 1 {
        let mut score = base_score;
        if i > 2 {
            score += model.score(
                Feature::UW1,
                &sentence[sentence_char_indices[i - 3]..sentence_char_indices[i - 2]],
            ) as f32;
        }
        if i > 1 {
            score += model.score(
                Feature::UW2,
                &sentence[sentence_char_indices[i - 2]..sentence_char_indices[i - 1]],
            ) as f32;
        }
        score += model.score(
            Feature::UW3,
            &sentence[sentence_char_indices[i - 1]..sentence_char_indices[i]],
        ) as f32;
        score += model.score(
            Feature::UW4,
            &sentence[sentence_char_indices[i]..sentence_char_indices[i + 1]],
        ) as f32;
        if i + 1 < sentence_char_indices.len() {
            score += model.score(Feature::UW5, &sentence[sentence_char_indices[i + 1]..]) as f32;
        }
        if i + 2 < sentence_char_indices.len() {
            score += model.score(Feature::UW6, &sentence[sentence_char_indices[i + 2]..]) as f32;
        }

        if i > 1 {
            score += model.score(
                Feature::BW1,
                &sentence[sentence_char_indices[i - 2]..sentence_char_indices[i - 1]],
            ) as f32;
        }
        score += model.score(
            Feature::BW2,
            &sentence[sentence_char_indices[i - 1]..sentence_char_indices[i]],
        ) as f32;
        if i + 1 < sentence_char_indices.len() {
            score += model.score(Feature::BW3, &sentence[sentence_char_indices[i]..]) as f32;
        }

        if i > 2 {
            score += model.score(
                Feature::TW1,
                &sentence[sentence_char_indices[i - 3]..sentence_char_indices[i - 2]],
            ) as f32;
        }
        if i > 1 {
            score += model.score(
                Feature::TW2,
                &sentence[sentence_char_indices[i - 2]..sentence_char_indices[i + 1]],
            ) as f32;
        }
        if i + 1 < sentence_char_indices.len() {
            score += model.score(Feature::TW3, &sentence[sentence_char_indices[i - 1]..]) as f32;
        }
        if i + 2 < sentence_char_indices.len() {
            score += model.score(Feature::TW4, &sentence[sentence_char_indices[i]..]) as f32;
        }

        if score > 0.0 {
            // split here
            chunk_heads.push(sentence_char_indices[i]);
            current_chunk_tail = sentence_char_indices[i + 1];
        } else {
            // chain
            current_chunk_tail = sentence_char_indices[i + 1];
        }
    }

    chunk_heads.push(current_chunk_tail);
    chunk_heads
}

/// rust-style parse iterator
pub struct BreakIterator<'m, 's, M: 'm + Model + ?Sized> {
    model: &'m M,
    base_score: f32,
    content: &'s str,
    char_indices: Vec<usize>,
    pointer: usize,
}
impl<'m, 's, M: 'm + Model + ?Sized> BreakIterator<'m, 's, M> {
    pub fn new(model: &'m M, content: &'s str) -> Self {
        Self {
            // TODO: できればメモリ確保しないようにしたい
            char_indices: content
                .char_indices()
                .map(|(a, _)| a)
                .chain(core::iter::once(content.len()))
                .collect(),
            base_score: model.base_score(),
            model,
            content,
            pointer: 0,
        }
    }
}
impl<'m, M: 'm + Model + ?Sized> BreakIterator<'m, '_, M> {
    #[inline(always)]
    fn try_uw(&self, pointer_offset: isize) -> Option<&str> {
        let base_pointer = self.pointer.checked_add_signed(pointer_offset)?;
        let start_index = *self.char_indices.get(base_pointer)?;
        let end_index = self
            .char_indices
            .get(base_pointer + 1)
            .copied()
            .unwrap_or(self.content.len());

        Some(&self.content[start_index..end_index])
    }

    #[inline(always)]
    fn try_bw(&self, pointer_offset: isize) -> Option<&str> {
        let start_index = *self
            .char_indices
            .get(self.pointer.checked_add_signed(pointer_offset)?)?;
        let end_index = self
            .char_indices
            .get(self.pointer + (pointer_offset + 2) as usize)
            .copied()
            .unwrap_or(self.content.len());

        Some(&self.content[start_index..end_index])
    }

    #[inline(always)]
    fn try_tw(&self, pointer_offset: isize) -> Option<&str> {
        let start_index = *self
            .char_indices
            .get(self.pointer.checked_add_signed(pointer_offset)?)?;
        let end_index = self
            .char_indices
            .get(self.pointer + (pointer_offset + 3) as usize)
            .copied()
            .unwrap_or(self.content.len());

        Some(&self.content[start_index..end_index])
    }
}
impl<'m, M: 'm + Model + ?Sized> Iterator for BreakIterator<'m, '_, M> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pointer == 0 {
            // first call
            self.pointer = 1;
            return Some(0);
        }

        if self.content.is_empty() {
            // no content
            return None;
        }

        if self.pointer == self.char_indices.len() {
            // beyond
            return None;
        }

        while self.pointer < self.char_indices.len() - 1 {
            let score = [
                self.try_uw(-3).map(|x| self.model.score(Feature::UW1, x)),
                self.try_uw(-2).map(|x| self.model.score(Feature::UW2, x)),
                self.try_uw(-1).map(|x| self.model.score(Feature::UW3, x)),
                self.try_uw(0).map(|x| self.model.score(Feature::UW4, x)),
                self.try_uw(1).map(|x| self.model.score(Feature::UW5, x)),
                self.try_uw(2).map(|x| self.model.score(Feature::UW6, x)),
                self.try_bw(-2).map(|x| self.model.score(Feature::BW1, x)),
                self.try_bw(-1).map(|x| self.model.score(Feature::BW2, x)),
                self.try_bw(0).map(|x| self.model.score(Feature::BW3, x)),
                self.try_tw(-3).map(|x| self.model.score(Feature::TW1, x)),
                self.try_tw(-2).map(|x| self.model.score(Feature::TW2, x)),
                self.try_tw(-1).map(|x| self.model.score(Feature::TW3, x)),
                self.try_tw(0).map(|x| self.model.score(Feature::TW4, x)),
            ]
            .into_iter()
            .flatten()
            .fold(self.base_score, |a, b| a + b as f32);

            if score > 0.0 {
                // split here
                let word_boundary = self.char_indices[self.pointer];
                self.pointer += 1;
                return Some(word_boundary);
            }

            self.pointer += 1;
        }

        // term
        let word_boundary = self.char_indices[self.pointer];
        self.pointer = self.char_indices.len();
        Some(word_boundary)
    }
}
