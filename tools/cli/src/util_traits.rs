use std::path::Path;

pub trait DirectoryPathExt: AsRef<Path> {
    fn ensure_directory(&self) -> Result<(), std::io::Error> {
        let p = self.as_ref();

        if !p.try_exists()? {
            std::fs::create_dir_all(p)?;
        }

        Ok(())
    }

    #[inline(always)]
    fn read_dir_recursive(&self) -> Result<ReadDirRecursive, std::io::Error> {
        Ok(ReadDirRecursive {
            iter_stack: vec![self.as_ref().read_dir()?],
        })
    }
}
impl<T: AsRef<Path>> DirectoryPathExt for T {}

pub struct ReadDirRecursive {
    iter_stack: Vec<std::fs::ReadDir>,
}
impl Iterator for ReadDirRecursive {
    type Item = Result<std::fs::DirEntry, std::io::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let Some(mut iter) = self.iter_stack.pop() else {
                break None;
            };

            let entry = match iter.next() {
                None => continue,
                Some(Err(e)) => break Some(Err(e)),
                Some(Ok(e)) => e,
            };
            self.iter_stack.push(iter);

            match entry.file_type() {
                Err(e) => break Some(Err(e)),
                // recurse into directory
                Ok(t) if t.is_dir() => match entry.path().read_dir() {
                    Err(e) => break Some(Err(e)),
                    Ok(x) => {
                        self.iter_stack.push(x);
                        continue;
                    }
                },
                Ok(_) => break Some(Ok(entry)),
            }
        }
    }
}
