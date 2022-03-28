use std::collections::HashMap;

#[derive(serde::Deserialize)]
pub struct Project {
    pub app_package_id: String,
    pub entry_type_name: Option<String>,
    pub asset_dir: Option<std::path::PathBuf>,
    #[serde(default)]
    pub features: Vec<String>,
    #[serde(default)]
    pub engine_features: Vec<String>,
    #[serde(default)]
    pub platform: HashMap<String, PlatformOverrides>,
}

#[derive(serde::Deserialize)]
pub struct PlatformOverrides {
    pub app_package_id: Option<String>,
    pub asset_dir: Option<std::path::PathBuf>,
    pub features: Option<Vec<String>>,
    pub engine_features: Option<Vec<String>>,
}

pub struct PlatformConfiguration<'s> {
    pub app_package_id: &'s str,
    pub entry_type_name: Option<&'s str>,
    pub asset_dir: Option<&'s std::path::Path>,
    pub features: &'s [String],
    pub engine_features: &'s [String],
}

impl Project {
    pub fn resolve_config(&self, platform: &str) -> PlatformConfiguration {
        let overrides = self.platform.get(platform);

        PlatformConfiguration {
            app_package_id: overrides
                .and_then(|o| o.app_package_id.as_deref())
                .unwrap_or(&self.app_package_id as &str),
            entry_type_name: self.entry_type_name.as_deref(),
            asset_dir: overrides
                .and_then(|o| o.asset_dir.as_deref())
                .or(self.asset_dir.as_deref()),
            features: overrides
                .and_then(|o| o.features.as_ref())
                .unwrap_or(&self.features),
            engine_features: overrides
                .and_then(|o| o.engine_features.as_ref())
                .unwrap_or(&self.engine_features),
        }
    }
}
