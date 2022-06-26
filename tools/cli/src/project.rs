use std::collections::HashMap;

#[derive(serde::Deserialize)]
pub struct Project {
    pub app_package_id: String,
    pub title: Option<String>,
    pub entry_type_name: Option<String>,
    pub asset_dir: Option<std::path::PathBuf>,
    #[serde(default)]
    pub features: Vec<String>,
    #[serde(default)]
    pub engine_features: Vec<String>,
    #[serde(default)]
    pub default_extent: WindowExtents,
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

#[derive(serde::Deserialize, Clone)]
#[serde(tag = "type")]
pub enum WindowExtents {
    Fixed { width: u16, height: u16 },
    Resizable { width: u16, height: u16 },
    Fullscreen,
}
impl Default for WindowExtents {
    fn default() -> Self {
        Self::Fixed {
            width: 640,
            height: 480,
        }
    }
}
impl WindowExtents {
    pub fn map_peridot_code(&self) -> String {
        match self {
            Self::Fixed { width, height } => {
                format!("peridot::WindowExtents::Fixed({width}, {height})")
            }
            Self::Resizable { width, height } => {
                format!("peridot::WindowExtents::Resizable({width}, {height})")
            }
            Self::Fullscreen => String::from("peridot::WindowExtents::Fullscreen"),
        }
    }
}

pub struct PlatformConfiguration<'s> {
    pub app_package_id: &'s str,
    pub title: Option<&'s str>,
    pub entry_type_name: Option<&'s str>,
    pub asset_dir: Option<&'s std::path::Path>,
    pub features: &'s [String],
    pub engine_features: &'s [String],
    pub default_extent: WindowExtents,
}

impl Project {
    pub fn resolve_config(&self, platform: &str) -> PlatformConfiguration {
        let overrides = self.platform.get(platform);

        PlatformConfiguration {
            app_package_id: overrides
                .and_then(|o| o.app_package_id.as_deref())
                .unwrap_or(&self.app_package_id as &str),
            title: self.title.as_deref(),
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
            default_extent: self.default_extent.clone(),
        }
    }
}
