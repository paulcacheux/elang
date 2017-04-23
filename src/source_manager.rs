use std::fs::File;
use std::io::Read;
use std::io;
use std::path::{Path, PathBuf};

struct SourceFile {
    path: PathBuf,
    content: String,
}

pub struct SourceManager {
    sources: Vec<SourceFile>
}

impl SourceManager {
    pub fn new() -> Self {
        SourceManager {
            sources: Vec::new()
        }
    }

    pub fn register_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<usize> {
        let index = self.sources.len();
        self.sources.push(SourceFile {
            path: path.as_ref().to_path_buf(),
            content: slurp_file(path)?
        });

        Ok(index)
    }

    pub fn get_input(&self, index: usize) -> &str {
        &self.sources[index].content
    }

    pub fn get_file_path(&self, index: usize) -> &Path {
        &self.sources[index].path
    }
}

fn slurp_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}
