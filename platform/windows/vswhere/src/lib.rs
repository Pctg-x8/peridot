use std::{ffi::OsStr, path::Path};

pub struct VSWhere(std::process::Command);
impl VSWhere {
    #[inline(always)]
    pub fn new(path: impl AsRef<OsStr>) -> Self {
        Self(std::process::Command::new(path))
    }

    #[inline(always)]
    pub fn latest(&mut self) -> &mut Self {
        self.0.arg("-latest");
        self
    }

    #[inline(always)]
    pub fn property(&mut self, name: &str) -> &mut Self {
        self.0.args(["-property", name]);
        self
    }

    #[inline(always)]
    pub fn get_output(&mut self) -> Result<VSWhereOutput, ExecutionError> {
        let r = self.0.output()?;
        if !r.status.success() {
            return Err(ExecutionError::UnsuccessfulExit(r.status));
        }

        Ok(VSWhereOutput(r))
    }
}
impl Default for VSWhere {
    #[inline(always)]
    fn default() -> Self {
        Self::new(
            std::path::PathBuf::from(
                std::env::var_os("ProgramFiles(x86)").expect("no program files x86"),
            )
            .join("Microsoft Visual Studio/Installer/vswhere.exe"),
        )
    }
}

#[repr(transparent)]
pub struct VSWhereOutput(std::process::Output);
impl VSWhereOutput {
    #[inline(always)]
    pub fn extract_single_path(&self) -> Result<&Path, core::str::Utf8Error> {
        Ok(Path::new(std::str::from_utf8(
            self.0.stdout.trim_ascii_end(),
        )?))
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ExecutionError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("vswhere exited with status {0}")]
    UnsuccessfulExit(std::process::ExitStatus),
}
