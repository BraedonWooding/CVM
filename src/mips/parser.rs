extern crate log;
use log::warn;

use crate;
use mips::*;

/*
    This is a different sort of parser effectively we are just going to
    do the transformation to IR / bytecode as we go instead of trying to create
    an AST, this is typically called an assembler in this context :)
 */

#[derive(Debug, Clone)]
pub struct ErrorToken {
    text: String,
    line: usize,
    col: usize,
}

pub type Token = Spanned<TokenKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Label(Ident),
    RegInstruction(Spanned<RegInstruction>),
    JmpInstruction(Spanned<JmpInstruction>),
    Macro{name: Ident, args: Vec<Ident>, body: Vec<Statement>},
    MacroCall(Ident),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    JmpInstruction(JmpInstruction),
    RegInstruction(RegInstruction)
}

pub enum JmpInstruction {

}

pub struct RegInstruction {
    
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Str(String),
    // TODO: When involving SPIM this can be larger
    Int(i16),
    // Characters in MIPS are a single byte
    Char(u8),
    Float(f64),
    MacroArg(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Directives {

}

#[derive(Clone)]
pub struct Parser<'a> {
    pub line: usize,
    pub col: usize,
    pub byte_offset: usize,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(chars: &'a str) -> Parser<'a> {
        // Lines / Cols start at 1
        Parser {
            line: 1,
            col: 1,
            byte_offset: 0,
            chars: chars.chars().peekable(),
        }
    }

    fn move_next(&mut self) {
        match self.chars.next() {
            Some('\n') => {
                self.line += 1;
                self.col = 1;
                self.byte_offset += 1;
            }
            Some(c) => {
                self.col += 1;
                self.byte_offset += c.len_utf8();
            }
            None => {}
        }
    }

    fn peek(&mut self) -> Option<char> {
        loop {
            match self.chars.peek() {
                Some(c) => {
                    if !c.is_whitespace() {
                        return Some(*c);
                    } else {
                        self.move_next();
                    }
                }
                None => return None,
            }
        }
    }

    fn next(&mut self) -> Option<char> {
        let c = self.peek();
        self.move_next();
        c
    }

    fn parse_simple_num(&mut self, base: u32) -> String {
        let mut num = String::new();
        loop {
            num.push(match self.chars.peek() {
                Some(c) if !c.is_digit(base) => break,
                None => break,
                Some(_) => self.next().unwrap(),
            });
        }
        num
    }

    fn parse_num(&mut self, start: char) -> Result<TokenKind, ErrorToken> {
        let mut num = start.to_string();
        num += &self.parse_simple_num(10);
        let mut postfix_guess = Postfix::i32;

        if self.chars.peek() == Some(&'.') {
            num.push('.');
            self.move_next();
            num += &self.parse_simple_num(10);
            postfix_guess = Postfix::f64;
        }
        if self.chars.peek() == Some(&'e') || self.chars.peek() == Some(&'E') {
            num.push('e');
            self.move_next();
            match self.chars.peek() {
                Some(c) if *c == '+' || *c == '-' => {
                    num.push(*c);
                    self.move_next();
                }
                _ => {}
            }
            num += &self.parse_simple_num(10);
            postfix_guess = Postfix::f64;
        }

        // postfix parsing
        // i.e. stuff like 2u or 2i64 or 2f...
        let postfix = match self.chars.peek() {
            Some('u') => {
                self.move_next();
                match self.parse_simple_num(10).as_str() {
                    "8" => Postfix::u8,
                    "16" => Postfix::u16,
                    "32" | "" => Postfix::u32,
                    "64" => Postfix::u64,
                    other => {
                        if self.matches("size") {
                            Postfix::usize
                        } else {
                            warn!("Invalid Postfix...");
                            return Err(ErrorToken::new(other.to_string(), self.line, self.col));
                        }
                    }
                }
            }
            Some('f') => {
                self.move_next();
                match self.parse_simple_num(10).as_str() {
                    "32" | "" => Postfix::f32,
                    "64" => Postfix::f64,
                    other => {
                        warn!("Invalid Postfix...");
                        return Err(ErrorToken::new(other.to_string(), self.line, self.col));
                    }
                }
            }
            Some('i') => {
                self.move_next();
                match self.parse_simple_num(10).as_str() {
                    "8" => Postfix::i8,
                    "16" => Postfix::i16,
                    "32" | "" => Postfix::i32,
                    "64" => Postfix::i64,
                    other => {
                        if self.matches("size") {
                            Postfix::isize
                        } else {
                            warn!("Invalid Postfix...");
                            return Err(ErrorToken::new(other.to_string(), self.line, self.col));
                        }
                    }
                }
            }
            // default postfix
            _ => postfix_guess,
        };

        match self.chars.peek() {
            Some(c) if c.is_ascii_alphabetic() || *c == '_' => {
                warn!("Invalid Number ... can't have identifier tokens");
                return Err(ErrorToken::new(num, self.line, self.col));
            }
            _ => {}
        }

        Ok(TokenKind::Number(num, postfix))
    }

    fn valid_identifier_continuer(c: char) -> bool {
        c.is_digit(10) || c.is_ascii_alphabetic() || c == '_'
    }

    // Won't work if you try to match it to something that contains a newline
    fn matches(&mut self, to: &str) -> bool {
        // NOTE: This isn't going to be slow despite it having to clone iterator
        //       effectively on most systems is just going to result in a clone
        //       of a file pointer (i.e. dup2 or whatever)
        let chars = self.chars.clone();
        let mut to_chars = to.chars().peekable();
        while to_chars.peek().is_some() {
            if to_chars.next() != self.chars.next() {
                self.chars = chars;
                return false;
            }
        }

        // we have to make sure the next character isn't a valid identifier
        // else we'll too eagerly match!
        match self.chars.peek() {
            Some(c) if Self::valid_identifier_continuer(*c) => {
                self.chars = chars;
                false
            }
            _ => {
                self.col += to.len();
                true
            }
        }
    }
}

impl Token {
    pub fn is_assignment(&self) -> bool {
        match **self {
            TokenKind::Assign => true,
            TokenKind::AddAssign => true,
            TokenKind::SubAssign => true,
            TokenKind::MulAssign => true,
            TokenKind::ModAssign => true,
            TokenKind::DivAssign => true,
            TokenKind::LShiftAssign => true,
            TokenKind::RShiftAssign => true,
            TokenKind::BitAndAssign => true,
            TokenKind::BitOrAssign => true,
            TokenKind::BitXorAssign => true,
            _ => false,
        }
    }

    pub fn is_unary(&self) -> bool {
        match **self {
            TokenKind::BitNot => true,
            TokenKind::Not => true,
            TokenKind::Asterix => true,
            TokenKind::BitAnd => true,
            TokenKind::Add => true,
            TokenKind::Sub => true,
            _ => false,
        }
    }
}

impl ErrorToken {
    pub fn new(text: String, line: usize, col: usize) -> ErrorToken {
        ErrorToken {
            text: text,
            line: line,
            col: col,
        }
    }

    pub fn report_error(&self) -> ! {
        panic!("Error {}:{}  {}", self.line, self.col, self.text);
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Token, ErrorToken>;

    fn next(&mut self) -> Option<Self::Item> {
        /*
         This function is quite large but is quite readable
         so I'm going to keep it this way.

         We could construct a trie and solve it that way
         but it'll just be less readable (I've experimented)
        */

        let line = self.line;
        let col = self.col;
        // The byte_offset points to the start of the token
        let start_byte_offset = self.byte_offset;

        // TODO: Maybe try to use a macro here??
        let kind = match self.next() {
            Some('~') => TokenKind::BitNot,
            Some('+') => match self.peek() {
                Some('=') => {
                    self.next();
                    TokenKind::AddAssign
                }
                _ => TokenKind::Add,
            },
            Some('-') => match self.peek() {
                Some('=') => {
                    self.next();
                    TokenKind::SubAssign
                }
                Some('>') => {
                    self.next();
                    TokenKind::Arrow
                }
                Some('-') if self.matches("--") => TokenKind::Uninitialised,
                _ => TokenKind::Sub,
            },
            Some('*') => match self.peek() {
                Some('=') => {
                    self.next();
                    TokenKind::MulAssign
                }
                _ => TokenKind::Asterix,
            },
            Some('/') => match self.peek() {
                Some('/') => {
                    // comment skip till newline
                    // @HACK: Order has to be this way else it'll read one extra line
                    let start = self.line;
                    while self.peek().is_some() && start == self.line {
                        self.move_next();
                    }
                    return Iterator::next(self);
                }
                Some('=') => {
                    self.next();
                    TokenKind::DivAssign
                }
                _ => TokenKind::Div,
            },
            Some('%') => match self.peek() {
                Some('=') => {
                    self.next();
                    TokenKind::ModAssign
                }
                _ => TokenKind::Mod,
            },
            Some('^') => match self.peek() {
                Some('=') => {
                    self.next();
                    TokenKind::BitXorAssign
                }
                _ => TokenKind::BitXor,
            },
            Some('|') => match self.peek() {
                Some('|') => {
                    self.next();
                    TokenKind::BoolOr
                }
                Some('=') => {
                    self.next();
                    TokenKind::BitOrAssign
                }
                _ => TokenKind::BitOr,
            },
            Some('&') => match self.peek() {
                Some('&') => {
                    self.next();
                    TokenKind::BoolAnd
                }
                Some('=') => {
                    self.next();
                    TokenKind::BitAndAssign
                }
                _ => TokenKind::BitAnd,
            },
            Some('\\') => TokenKind::Lambda,
            Some(':') => TokenKind::Colon,
            Some(';') => TokenKind::SemiColon,
            Some('(') => TokenKind::LParen,
            Some(')') => TokenKind::RParen,
            Some('[') => TokenKind::LBracket,
            Some(']') => TokenKind::RBracket,
            Some('{') => TokenKind::LBrace,
            Some('}') => TokenKind::RBrace,
            Some('.') => TokenKind::Period,
            Some(',') => TokenKind::Comma,
            Some('<') => match self.peek() {
                Some('=') => {
                    self.next();
                    TokenKind::LessEqual
                }
                Some('<') => {
                    self.next();
                    match self.peek() {
                        Some('=') => {
                            self.next();
                            TokenKind::LShiftAssign
                        }
                        _ => TokenKind::LShift,
                    }
                }
                _ => TokenKind::LAngle,
            },
            Some('>') => match self.peek() {
                Some('=') => {
                    self.next();
                    TokenKind::GreaterEqual
                }
                Some('>') => {
                    self.next();
                    match self.peek() {
                        Some('=') => {
                            self.next();
                            TokenKind::RShiftAssign
                        }
                        _ => TokenKind::RShift,
                    }
                }
                _ => TokenKind::RAngle,
            },
            Some('!') => match self.peek() {
                Some('=') => {
                    self.next();
                    TokenKind::NotEqual
                }
                _ => TokenKind::Not,
            },
            Some('=') => match self.peek() {
                Some('=') => {
                    self.next();
                    TokenKind::Equal
                }
                Some('>') => {
                    self.next();
                    TokenKind::FatArrow
                }
                _ => TokenKind::Assign,
            },
            Some('f') if self.matches("alse") => TokenKind::Bool(false),
            Some('t') if self.matches("rue") => TokenKind::Bool(true),
            Some('n') if self.matches("ull") => TokenKind::Null,
            Some('i') if self.matches("f") => TokenKind::If,
            Some('i') if self.matches("s") => TokenKind::Is,
            Some('w') if self.matches("hile") => TokenKind::While,
            Some('d') if self.matches("efer") => TokenKind::Defer,
            Some('f') if self.matches("or") => TokenKind::For,
            Some('f') if self.matches("n") => TokenKind::Function,
            Some('s') if self.matches("truct") => TokenKind::Struct,
            Some('e') if self.matches("num") => TokenKind::Enum,
            Some('e') if self.matches("lse") => TokenKind::Else,
            Some('l') if self.matches("et") => TokenKind::Let,
            Some('s') if self.matches("izeof") => TokenKind::Sizeof,
            Some('n') if self.matches("ew") => TokenKind::New,
            Some('r') if self.matches("eturn") => TokenKind::Return,
            Some('b') if self.matches("reak") => TokenKind::Break,
            Some('c') if self.matches("ontinue") => TokenKind::Continue,
            Some('c') if self.matches("ast") => TokenKind::Cast,
            Some('"') => {
                // TODO: Extract into function
                let mut string = String::new();
                loop {
                    match self.chars.peek() {
                        Some('"') => {
                            self.move_next();
                            break;
                        }
                        Some('\\') => {
                            self.move_next();
                            string.push(match self.chars.peek() {
                                Some('n') => '\n',
                                Some('\\') => '\\',
                                Some('t') => '\t',
                                Some('r') => '\r',
                                Some('\'') => '\'',
                                Some('"') => '"',
                                // TODO: Unicode
                                _ => {
                                    return Some(Err(ErrorToken::new(string, self.line, self.col)))
                                }
                            });
                            self.move_next();
                        }
                        Some(c) => {
                            string.push(*c);
                            self.move_next();
                        }
                        None => return Some(Err(ErrorToken::new(string, self.line, self.col))),
                    }
                }
                TokenKind::Str(string)
            }
            Some('\'') => {
                panic!("TODO: Handle characters.  I'm too lazy todo them");
            }
            Some(c) if c.is_digit(10) => match self.parse_num(c) {
                Err(e) => return Some(Err(e)),
                Ok(tok) => tok,
            },

            // TODO: C supports unicode identifiers so we should
            //       apparently it also supports \U... :O
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                let mut id = c.to_string();
                loop {
                    match self.chars.peek() {
                        Some(c) if Self::valid_identifier_continuer(*c) => {
                            id.push(*c);
                            self.move_next();
                        }
                        _ => break,
                    }
                }
                TokenKind::Ident(id)
            }
            Some(other) => {
                return Some(Err(ErrorToken::new(other.to_string(), self.line, self.col)))
            }
            None => return None,
        };

        let byte_offset = (start_byte_offset, self.byte_offset);
        Some(Ok(Token::new(
            kind,
            Span {
                line,
                col,
                byte_offset,
            },
        )))
    }
}
