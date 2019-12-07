// It makes the prefixes nicer to use
#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum Postfix {
    None,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f32,
    f64,
    usize,
    isize,
}

#[derive(Debug)]
pub struct ErrorToken {
    text: String,
    line: usize,
    col: usize,
}

#[derive(Debug)]
pub enum Token {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LAngle,
    RAngle,

    Add,
    Sub,
    Asterix,
    Mod,
    Div,

    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,

    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    ModAssign,
    DivAssign,
    LShiftAssign,
    RShiftAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,

    GreaterEqual,
    LessEqual,
    Not,
    Equal,
    NotEqual,
    BoolAnd,
    BoolOr,

    Colon,
    SemiColon,
    Period,
    Comma,
    Uninitialised,

    Let,
    Sizeof,
    New,
    Defer,
    While,
    Else,
    If,
    For,
    Struct,
    Enum, // TODO
    Null,
    Return,
    Break,
    Continue,

    Ident(String),
    Character(char),
    Str(String),
    Number(String, Postfix),
    Bool(bool)
}

pub struct Lexer<'a> {
    line: usize,
    col: usize,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(chars: &'a str) -> Lexer<'a> {
        Lexer {
            line: 0,
            col: 0,
            chars: chars.chars().peekable(),
        }
    }

    fn move_next(&mut self) {
        match self.chars.next() {
            Some('\n') => {
                self.line += 1;
                self.col = 1;
            },
            Some(_) => {
                self.col += 1;
            },
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
                },
                None => return None
            }
        }
    }

    fn next(&mut self) -> Option<char> {
        let c = self.peek();
        self.move_next();
        c
    }

    fn parse_num(&mut self, start: char) -> Result<Token, ErrorToken> {
        let mut num = start.to_string();
        let mut found_dot = false;
        let mut found_exp = false;

        // TODO: Handle postfixes like u8 and usize
        loop {
            let c = match self.chars.peek() {
                Some(c) if !c.is_digit(10) && *c != 'e' && *c != '.' => break,
                None => break,
                Some(c) => *c
            };

            num.push(c);
            self.move_next();

            // Note: we aren't using our 'peek' method
            //       since we want spaces to break up
            //       our number.
            match c {
                '0' | '1' | '2' | '3' | '4' | '5'  | '6' | '7' | '8' | '9' => {},
                'e' if !found_exp => {
                    found_exp = true;
                    match self.chars.peek() {
                        Some('+') | Some('-') => {
                            num.push(*self.chars.peek().unwrap());
                            self.move_next();
                        }
                        _ => {}
                    }
                }
                '.' if !found_dot && !found_exp => {
                    found_dot = true;
                },
                _ => {
                    return Err(ErrorToken::new(num, self.line, self.col));
                }
            }
        }

        Ok(Token::Number(num, Postfix::None))
    }

    fn valid_identifier_continuer(c: char) -> bool {
        return c.is_digit(10) || c.is_ascii_alphabetic() || c == '_';
    }

    // Won't work if you try to match it to something that contains a newline
    fn matches(&mut self, to: &str) -> bool {
        let chars = self.chars.clone();
        let mut to_chars = to.chars().peekable();
        while to_chars.peek().is_some() {
            if to_chars.next() != self.chars.next() {
                self.chars = chars;
                return false;
            }
        }

        // we have to make sure the next character isn't a valid identifier
        match self.chars.peek() {
            Some(c) if Self::valid_identifier_continuer(*c) => {
                self.chars = chars;
                false
            },
            _ => {
                self.col += to.len();
                true
            }
        }
    }
}

impl ErrorToken {
    pub fn new(text: String, line: usize, col: usize) -> ErrorToken {
        ErrorToken {
            text: text,
            line: line,
            col: col
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, ErrorToken>;

    fn next(&mut self) -> Option<Self::Item> {
        /*
          This function is quite large but is quite readable
          so I'm going to keep it this way.

          We could construct a trie and solve it that way
          but it'll just be less readable (I've experimented)
         */

        // This is our 'we haven't read yet' indicator
        if self.line == 0 {
            self.line = 1;
            self.col = 1;
        }

        // TODO: Maybe try to use a macro here??
        match self.next() {
            Some('+') => match self.peek() {
                Some('=') => { self.next(); Some(Ok(Token::AddAssign)) },
                _ => Some(Ok(Token::Add))
            }
            Some('-') => match self.peek() {
                Some('=') => { self.next(); Some(Ok(Token::SubAssign)) },
                Some('-') if self.matches("--") => Some(Ok(Token::Uninitialised)),
                _ => Some(Ok(Token::Sub))
            }
            Some('*') => match self.peek() {
                Some('=') => { self.next(); Some(Ok(Token::MulAssign)) },
                _ => Some(Ok(Token::Asterix))
            }
            Some('/') => match self.peek() {
                Some('/') => {
                    // comment skip till newline
                    // @HACK: Order has to be this way else it'll read one extra line
                    let start = self.line;
                    while self.peek().is_some() && start == self.line { self.move_next(); }
                    Iterator::next(self)
                },
                Some('=') => { self.next(); Some(Ok(Token::DivAssign)) },
                c => {println!("{}", c.unwrap()); Some(Ok(Token::Div))
}            }
            Some('%') => match self.peek() {
                Some('=') => { self.next(); Some(Ok(Token::ModAssign)) },
                _ => Some(Ok(Token::Mod))
            }
            Some('^') => match self.peek() {
                Some('=') => { self.next(); Some(Ok(Token::BitXorAssign)) },
                _ => Some(Ok(Token::BitXor))
            }
            Some('|') => match self.peek() {
                Some('|') => { self.next(); Some(Ok(Token::BoolOr)) },
                Some('=') => { self.next(); Some(Ok(Token::BitOrAssign)) },
                _ => Some(Ok(Token::BitOr))
            }
            Some('&') => match self.peek() {
                Some('&') => { self.next(); Some(Ok(Token::BoolAnd)) },
                Some('=') => { self.next(); Some(Ok(Token::BitAndAssign)) },
                _ => Some(Ok(Token::BitAnd))
            }
            Some(':') => Some(Ok(Token::Colon)),
            Some(';') => Some(Ok(Token::SemiColon)),
            Some('(') => Some(Ok(Token::LParen)),
            Some(')') => Some(Ok(Token::RParen)),
            Some('[') => Some(Ok(Token::LBracket)),
            Some(']') => Some(Ok(Token::RBracket)),
            Some('{') => Some(Ok(Token::LBrace)),
            Some('}') => Some(Ok(Token::RBrace)),
            Some('.') => Some(Ok(Token::Period)),
            Some(',') => Some(Ok(Token::Comma)),
            Some('<') => match self.peek() {
                Some('=') => { self.next(); Some(Ok(Token::LessEqual)) },
                Some('<') => { self.next(); match self.peek() {
                    Some('=') => { self.next(); Some(Ok(Token::LShiftAssign)) },
                    _ => Some(Ok(Token::LShift))
                }},
                _ => Some(Ok(Token::LAngle)),
            },
            Some('>') => match self.peek() {
                Some('=') => { self.next(); Some(Ok(Token::GreaterEqual)) },
                Some('>') => { self.next(); match self.peek() {
                    Some('=') => { self.next(); Some(Ok(Token::RShiftAssign)) },
                    _ => Some(Ok(Token::RShift))
                }},
                _ => Some(Ok(Token::RAngle)),
            },
            Some('!') => match self.peek() {
                Some('=') => { self.next(); Some(Ok(Token::NotEqual)) },
                _ => Some(Ok(Token::Not)),
            },
            Some('=') => match self.peek() {
                Some('=') => { self.next(); Some(Ok(Token::Equal)) },
                _ => Some(Ok(Token::Assign)),
            },
            Some('f') if self.matches("alse") => Some(Ok(Token::Bool(false))),
            Some('t') if self.matches("rue") => Some(Ok(Token::Bool(true))),
            Some('n') if self.matches("ull") => Some(Ok(Token::Null)),
            Some('i') if self.matches("f") => Some(Ok(Token::If)),
            Some('w') if self.matches("hile") => Some(Ok(Token::While)),
            Some('d') if self.matches("defer") => Some(Ok(Token::Defer)),
            Some('f') if self.matches("or") => Some(Ok(Token::For)),
            Some('s') if self.matches("truct") => Some(Ok(Token::Struct)),
            Some('e') if self.matches("num") => Some(Ok(Token::Enum)),
            Some('e') if self.matches("lse") => Some(Ok(Token::Else)),
            Some('l') if self.matches("et") => Some(Ok(Token::Let)),
            Some('s') if self.matches("izeof") => Some(Ok(Token::Sizeof)),
            Some('n') if self.matches("ew") => Some(Ok(Token::New)),
            Some('r') if self.matches("eturn") => Some(Ok(Token::Return)),
            Some('b') if self.matches("reak") => Some(Ok(Token::Break)),
            Some('c') if self.matches("ontinue") => Some(Ok(Token::Continue)),
            Some('"') => {
                // TODO: Extract into function
                let mut string = String::new();
                loop {
                    match self.chars.peek() {
                        Some('"') => { self.move_next(); break; },
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
                                _ => return Some(Err(ErrorToken::new(string, self.line, self.col)))
                            });
                            self.move_next();
                        }
                        Some(c) => {
                            string.push(*c);
                            self.move_next();
                        }
                        None => return Some(Err(ErrorToken::new(string, self.line, self.col)))
                    }
                }
                Some(Ok(Token::Str(string)))
            }
            Some('\'') => {
                panic!("TODO: Handle characters.  I'm too lazy todo them");
            },
            Some(c) if c.is_digit(10) => Some(self.parse_num(c)),

            // TODO: C supports unicode identifiers so we should
            //       apparently it also supports \U... :O
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                let mut id = c.to_string();
                loop {
                    match self.chars.peek() {
                        Some(c) if Self::valid_identifier_continuer(*c) => {
                            id.push(*c);
                            self.move_next();
                        },
                        _ => break,
                    }
                }
                Some(Ok(Token::Ident(id)))
            }
            Some(other) => Some(Err(ErrorToken::new(other.to_string(), self.line, self.col))),
            None => None
        }
    }
}

