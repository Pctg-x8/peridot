//! Cargo Manifest

use std::collections::HashMap;
use std::path::Path;

fn is_false(b: &bool) -> bool { !b }

#[derive(serde::Deserialize, serde::Serialize, Debug)]
pub struct CargoPackageSection<'s> {
    pub name: Option<&'s str>,
    #[serde(flatten)]
    pub others: HashMap<&'s str, toml::Value>
}
#[derive(serde::Deserialize, serde::Serialize, Debug)]
pub struct CargoDependencyOptions<'s> {
    #[serde(skip_serializing_if = "is_false", default)]
    pub optional: bool,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub features: Vec<&'s str>,
    #[serde(flatten, borrow = "'s")]
    pub others: HashMap<&'s str, toml::Value>
}
impl<'s> Default for CargoDependencyOptions<'s> {
    fn default() -> Self {
        CargoDependencyOptions {
            optional: false,
            features: vec![],
            others: HashMap::new()
        }
    }
}
#[derive(serde::Deserialize, serde::Serialize, Debug)]
#[serde(untagged)]
pub enum CargoDependency<'s> {
    VersionOnly(&'s str),
    VersionDetailed {
        version: &'s str,
        #[serde(flatten, borrow = "'s")]
        opts: CargoDependencyOptions<'s>
    },
    Path {
        path: &'s Path,
        #[serde(flatten, borrow = "'s")]
        opts: CargoDependencyOptions<'s>
    },
    Git {
        git: &'s str,
        branch: Option<&'s str>,
        #[serde(flatten, borrow = "'s")]
        opts: CargoDependencyOptions<'s>
    }
}
#[derive(serde::Deserialize, serde::Serialize, Debug)]
pub struct CargoManifest<'s> {
    #[serde(borrow = "'s")]
    pub package: Option<CargoPackageSection<'s>>,
    #[serde(borrow = "'s", serialize_with = "toml::ser::tables_last")]
    pub dependencies: HashMap<&'s str, CargoDependency<'s>>,
    #[serde(flatten)]
    pub others: HashMap<&'s str, toml::Value>
}

pub fn gen_manifest(
    manifest_path: &Path,
    manifest_template_path: &Path,
    userlib_path_abs: &Path,
    userlib_name: &str,
    userlib_features: Vec<&str>
) {
    use std::io::Write;

    let manifest_content = std::fs::read_to_string(&manifest_template_path)
        .expect("Failed to load cradle manifest");
    let mut manifest: CargoManifest = toml::from_str(&manifest_content)
        .expect("Failed to parse cradle manifest");

    // set userlib as cradle's dependency
    manifest.dependencies.insert(userlib_name, CargoDependency::Path {
        path: userlib_path_abs, opts: CargoDependencyOptions {
            features: userlib_features,
            .. Default::default()
        }
    });

    std::fs::OpenOptions::new().create(true).truncate(true).write(true).open(&manifest_path)
        .expect("Failed to open Userlib Cargo.toml for writing")
        .write(toml::to_string_pretty(&manifest).expect("Failed to serialize Cargo.toml").as_bytes())
        .expect("Failed to write Modified Cargo.toml");
}
