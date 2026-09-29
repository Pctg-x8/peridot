pub struct EmbeddedModel {
    pub base_score: f32,
    pub uw1_score: phf::Map<&'static str, i32>,
    pub uw2_score: phf::Map<&'static str, i32>,
    pub uw3_score: phf::Map<&'static str, i32>,
    pub uw4_score: phf::Map<&'static str, i32>,
    pub uw5_score: phf::Map<&'static str, i32>,
    pub uw6_score: phf::Map<&'static str, i32>,
    pub bw1_score: phf::Map<&'static str, i32>,
    pub bw2_score: phf::Map<&'static str, i32>,
    pub bw3_score: phf::Map<&'static str, i32>,
    pub tw1_score: phf::Map<&'static str, i32>,
    pub tw2_score: phf::Map<&'static str, i32>,
    pub tw3_score: phf::Map<&'static str, i32>,
    pub tw4_score: phf::Map<&'static str, i32>,
}
impl crate::Model for EmbeddedModel {
    #[inline(always)]
    fn base_score(&self) -> f32 {
        self.base_score
    }

    #[inline(always)]
    fn score(&self, feature: crate::Feature, sentence: &str) -> i32 {
        match feature {
            crate::Feature::UW1 => self.uw1_score.get(sentence),
            crate::Feature::UW2 => self.uw2_score.get(sentence),
            crate::Feature::UW3 => self.uw3_score.get(sentence),
            crate::Feature::UW4 => self.uw4_score.get(sentence),
            crate::Feature::UW5 => self.uw5_score.get(sentence),
            crate::Feature::UW6 => self.uw6_score.get(sentence),
            crate::Feature::BW1 => self.bw1_score.get(sentence),
            crate::Feature::BW2 => self.bw2_score.get(sentence),
            crate::Feature::BW3 => self.bw3_score.get(sentence),
            crate::Feature::TW1 => self.tw1_score.get(sentence),
            crate::Feature::TW2 => self.tw2_score.get(sentence),
            crate::Feature::TW3 => self.tw3_score.get(sentence),
            crate::Feature::TW4 => self.tw4_score.get(sentence),
        }
        .copied()
        .unwrap_or(0)
    }
}

#[cfg(feature = "embedded-ja_knbc")]
pub mod ja_knbc;
