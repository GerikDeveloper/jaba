use tokio::fs::File;
use tokio::io::AsyncReadExt;

pub(crate) const EOF: char = '\0';

#[derive(Clone)]
pub(crate) struct GlobPos {
    pub(crate) src_path: String,
    pub(crate) line_pos: usize,
    pub(crate) ch_pos: usize,
}

pub(crate) struct SrcDriver {
    pos: usize,
    glob_pos: GlobPos,
    src: Vec<char>,
}

impl SrcDriver {
    pub(crate) async fn new(src_path: String) -> Option<Self> {
        if let Ok(mut src_file) = File::open(src_path.clone()).await {
            let mut src_str: String = String::new();
            if let Ok(_) = src_file.read_to_string(&mut src_str).await {
                let src: Vec<char> = src_str.chars().collect();
                if src.len() != 0 {
                    return Some(
                        Self {
                            pos: 0,
                            glob_pos: GlobPos {
                                src_path,
                                line_pos: 0,
                                ch_pos: 0,
                            },
                            src,
                        }
                    );
                }
            }
        }
        return None;
    }

    pub(crate) fn next_ch(&mut self) {
        if self.pos < self.src.len() {
            self.pos += 1;
            self.glob_pos.ch_pos += 1;
            if self.get_ch() == '\n' {
                self.glob_pos.line_pos += 1;
                self.glob_pos.ch_pos = 0;
            }
        }

    }

    pub(crate) fn get_ch(&self) -> char {
        return if let Some(res) = self.src.get(self.pos) { *res } else { EOF };
    }

    pub(crate) fn get_next_ch(&mut self) -> char {
        self.next_ch();
        return self.get_ch();
    }

    pub(crate) fn get_pos(&self) -> usize {
        return self.pos;
    }

    pub(crate) fn skip_spaces(&mut self) {
        while self.get_ch() == ' ' ||
            self.get_ch() == '\t' ||
            self.get_ch() == '\n' ||
            self.get_ch() == '\r' {
            self.next_ch();
        }
    }

    pub(crate) fn get_glob_pos(&self) -> GlobPos {
        return self.glob_pos.clone();
    }
}