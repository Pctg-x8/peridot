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
