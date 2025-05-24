use phf::phf_map;
use crate::jaba::errors_handler::ErrorsHandler;
use crate::jaba::src_driver;
use crate::jaba::src_driver::{GlobPos, SrcDriver};

const MAX_NUM: i32 = 0xFFFF;
#[derive(Clone, PartialEq)]
pub(crate) enum Lex { //B - bitwise, L - logical
    None,
    Name(String),
    Num(i32),
    Malv, //Malvera
    Vera,
    Uzi,
    Konst,
    Lig,
    //Sxa
    Se,
    Alie,
    Buk,
    Plus,
    Minus,
    BAnd,
    BOr,
    Xor,
    Not,
    LS,
    RS,
    ROL,
    ROR,
    LAnd,
    LOr,
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
    Comma,
    Point,
    Colon,
    Semi,
    Ass,
    LPar,
    RPar,
    LBrac,
    RBrac,
    FN,
    Sago,
    Red,
    EOF,
}

static KW_TABLE: phf::Map<&'static str, Lex> = phf_map! {
    "malv" => Lex::Malv,
    "vera" => Lex::Vera,
    "uzi" => Lex::Uzi,
    "konst" => Lex::Konst,
    "lig" => Lex::Lig,
    //Sxa
    "se" => Lex::Se,
    "alie" => Lex::Alie,
    "buk" => Lex::Buk,
    "fn" => Lex::FN,
    "red" => Lex::Red,
};

pub(crate) struct LexAnalyzer {
    src_driver: SrcDriver,
    lex: Lex,
    glob_pos: GlobPos,
}

impl LexAnalyzer {
    pub(crate) fn new(src_driver: SrcDriver) -> Self {
        let glob_pos = src_driver.get_glob_pos();
        let mut res = Self {
            src_driver,
            lex: Lex::None,
            glob_pos,
        };
        res.next_lex();
        return res;
    }

    fn test_kw(ident: String) -> Lex {
        if let Some(lex) = KW_TABLE.get(ident.as_str()) {
            return lex.clone();
        }
        return Lex::Name(ident);
    }

    fn ident(&mut self) {
        let mut res: String = String::new();
        loop {
            match self.src_driver.get_ch() {
                '0'..='9' | 'A'..='Z' | 'a'..='z' | '_' => {
                    res.push(self.src_driver.get_ch());
                }
                _ => { break; }
            }
            self.src_driver.next_ch();
        }
        self.lex = Self::test_kw(res);
    }

    fn num(&mut self) {
        let mut radix: i32 = 10;
        let mut res: i32 = 0;
        if self.src_driver.get_ch() == '0' {
            match self.src_driver.get_next_ch() {
                'x' => { radix = 16; self.src_driver.next_ch(); }
                'b' => { radix = 2; self.src_driver.next_ch(); }
                'k' => { radix = 8; self.src_driver.next_ch(); }
                _ => {}
            }
        }
        loop {
            match self.src_driver.get_ch() {
                '0'..='9' => {
                    let digit = self.src_driver.get_ch() as i32 - '0' as i32;
                    if digit < radix {
                        res *= radix;
                        res += digit;
                    } else { ErrorsHandler::error_0020(radix, self.get_glob_pos()); }
                }
                'A'..='F' => {
                    let digit = self.src_driver.get_ch() as i32 - 'A' as i32 + 10;
                    if digit < radix {
                        res *= radix;
                        res += digit;
                    } else { ErrorsHandler::error_0020(radix, self.get_glob_pos()); }
                }
                'a'..='f' => {
                    let digit = self.src_driver.get_ch() as i32 - 'a' as i32 + 10;
                    if digit < radix {
                        res *= radix;
                        res += digit;
                    } else { ErrorsHandler::error_0020(radix, self.get_glob_pos()); }
                }
                _ => { break; }
            }
            if res > MAX_NUM { ErrorsHandler::error_0021(self.get_glob_pos()); }
            self.src_driver.next_ch();
        }
        self.lex = Lex::Num(res);
    }

    fn skip_comment(&mut self) {
        if self.src_driver.get_ch() == '/' {
            loop {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '\n' ||
                    self.src_driver.get_ch() == src_driver::EOF {
                    break;
                }
            }
        } else if self.src_driver.get_ch() == '*' {
            let mut lvl: usize = 1;
            while lvl != 0 {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '*' {
                    if self.src_driver.get_next_ch() == '/' {
                        lvl -= 1;
                    }
                } else if self.src_driver.get_ch() == '/' {
                    if self.src_driver.get_next_ch() == '*' {
                        lvl += 1;
                    }
                }

                if self.src_driver.get_ch() == src_driver::EOF {
                    ErrorsHandler::error_0011(self.get_glob_pos());
                }
            }
            self.src_driver.next_ch();
        } else {
            //Dummy check
            ErrorsHandler::error_0010(
                Some(String::from("['/', '*']")),
                Some(if self.src_driver.get_ch() != src_driver::EOF {
                    String::from(format!("['{}']", self.src_driver.get_ch()))
                } else { String::from("[FDD]") }),
                self.get_glob_pos(),
            );
        }
    }

    pub(crate) fn next_lex(&mut self) {
        self.src_driver.skip_spaces();
        self.glob_pos = self.src_driver.get_glob_pos();
        match self.src_driver.get_ch() {
            'A'..='Z' | 'a'..='z' => {
                self.ident();
            }
            '0'..='9' => {
                self.num();
            }
            '+' => { self.src_driver.next_ch(); self.lex = Lex::Plus; }
            '-' => {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '>' {
                    self.src_driver.next_ch();
                    self.lex = Lex::Sago;
                } else { self.lex = Lex::Minus; }
            }
            '&' => {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '&' {
                    self.src_driver.next_ch();
                    self.lex = Lex::LAnd;
                } else { self.lex = Lex::BAnd; }
            }
            '|' => {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '|' {
                    self.src_driver.next_ch();
                    self.lex = Lex::LOr;
                } else { self.lex = Lex::BOr; }
            }
            '^' => {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '>' {
                    self.src_driver.next_ch();
                    self.lex = Lex::ROR;
                } else { self.lex = Lex::Xor; }
            }
            '!' => {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '=' {
                    self.src_driver.next_ch();
                    self.lex = Lex::NE;
                } else { self.lex = Lex::Not; }
            }
            '=' => {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '=' {
                    self.src_driver.next_ch();
                    self.lex = Lex::EQ;
                } else { self.lex = Lex::Ass; }
            }
            '<' => {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '=' {
                    self.src_driver.next_ch();
                    self.lex = Lex::LE;
                } else if self.src_driver.get_ch() == '<' {
                    self.src_driver.next_ch();
                    self.lex = Lex::LS;
                } else if self.src_driver.get_ch() == '^' {
                    self.src_driver.next_ch();
                    self.lex = Lex::ROL;
                } else { self.lex = Lex::LT; }
            }
            '>' => {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '=' {
                    self.src_driver.next_ch();
                    self.lex = Lex::GE;
                } else if self.src_driver.get_ch() == '>' {
                    self.src_driver.next_ch();
                    self.lex = Lex::RS;
                } else { self.lex = Lex::GT; }
            }
            ',' => { self.src_driver.next_ch(); self.lex = Lex::Comma; }
            '.' => { self.src_driver.next_ch(); self.lex = Lex::Point; }
            ':' => { self.src_driver.next_ch(); self.lex = Lex::Colon; }
            ';' => { self.src_driver.next_ch(); self.lex = Lex::Semi; }
            '(' => { self.src_driver.next_ch(); self.lex = Lex::LPar; }
            ')' => { self.src_driver.next_ch(); self.lex = Lex::RPar; }
            '{' => { self.src_driver.next_ch(); self.lex = Lex::LBrac; }
            '}' => { self.src_driver.next_ch(); self.lex = Lex::RBrac; }
            '/' => {
                self.src_driver.next_ch();
                if self.src_driver.get_ch() == '/' || self.src_driver.get_ch() == '*' {
                    self.skip_comment();
                    self.next_lex();
                } else {
                    ErrorsHandler::error_0010(
                        Some(String::from("['/', '*']")),
                        Some(if self.src_driver.get_ch() != src_driver::EOF {
                            String::from(format!("['{}']", self.src_driver.get_ch()))
                        } else { String::from("[FDD]") }),
                        self.get_glob_pos(),
                    );
                }
            }
            src_driver::EOF => { self.src_driver.next_ch(); self.lex = Lex::EOF; }
            _ => {
                ErrorsHandler::error_0010(
                    None,
                    Some(String::from(format!("['{}']", self.src_driver.get_ch()))),
                    self.get_glob_pos(),
                );
            }
        }
    }

    pub(crate) fn get_lex(&self) -> Lex {
        return self.lex.clone();
    }

    pub(crate) fn get_next_lex(&mut self) -> Lex {
        self.next_lex();
        return self.get_lex();
    }

    pub(crate) fn get_glob_pos(&self) -> GlobPos {
        return self.glob_pos.clone();
    }
}
