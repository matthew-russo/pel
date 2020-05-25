use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::convert::TryInto;
use std::error::Error;
use std::fmt::Display;
use std::iter::FromIterator;
use std::str::FromStr;

use crate::lexer::tokens::*;
use crate::lexer::tokens::TokenData::*;

#[derive(Debug, Clone)]
pub(crate) enum LexError {
    Message(String),
    Source(Box<LexError>)
}

impl LexError {
    fn of(err: LexError) -> Self {
        LexError::Source(Box::new(err))
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexError::Message(m) => write!(f, "lex error: {}", m),
            LexError::Source(err) => err.fmt(f)
        }
    }
}

impl Error for LexError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            LexError::Message(_) => None,
            LexError::Source(err) => Some(err),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Lexer {
    pub(crate) file: String,
    input: String,
    pub(crate) start: u32,
    pub(crate) current: u32,
    pub(crate) line: u32,
    pub(crate) col: u32,
    lexing: Vec<char>,
    tokens: Vec<Token>
}

impl Lexer {
    pub(crate) fn new() -> Self {
        Self {
            file: "unimplemented".into(),
            input: String::new(),
            start: 0,
            current: 0,
            line: 0,
            col: 0,
            lexing: Vec::new(),
            tokens: Vec::new(),
        }
    }

    fn reset_state(&mut self, input: String) {
        self.input = input;
        self.start = 0;
        self.current = 0;
        self.lexing = Vec::new();
        self.tokens = Vec::new();
    }

    pub(crate) fn lex(&mut self, input: String) -> Result<Vec<Token>, LexError> {
        self.reset_state(input);

        loop {
            match self._lex() {
                Ok(Token { data: TokenData::EOF, .. }) => {
                    self.emit_token(Token::from_data(&self, TokenData::EOF));
                    return Ok(self.tokens.drain(..).collect());
                },
                Ok(token) => self.emit_token(token),
                Err(e) => return Err(LexError::of(e)),
            }
        }
    }

    pub(crate) fn emit_token(&mut self, token: Token) {
        self.tokens.push(token);
        self.start = self.current;
        self.lexing = Vec::new();
    }

    pub(crate) fn drop_current(&mut self) {
        self.start = self.current;
        self.lexing = Vec::new();
    }

    fn try_lex<LexFn>(&mut self, lex_fn: LexFn) -> Result<Token, LexError>
            where LexFn: Fn(&mut Self) -> Result<Token, LexError> {
        let snapshot = self.snapshot();

        return match lex_fn(self) {
            Ok(token) => Ok(token),
            Err(e) => {
                self.reset(snapshot);
                Err(e)
            }
        }
    }

    fn snapshot(&mut self) -> (u32, u32, Vec<char>) {
        return (self.start, self.current, self.lexing.clone());
    }

    fn reset(&mut self, snapshot: (u32, u32, Vec<char>)) {
        self.start = snapshot.0;
        self.current = snapshot.1;
        self.lexing = snapshot.2;
    }

    fn _lex(&mut self) -> Result<Token, LexError> {
        self.chomp_while_any_of("\t\n \r");
        self.drop_current();
        
        if (self.current + 1) >= self.input.len().try_into().unwrap() {
            return Ok(Token::from_data(&self, TokenData::EOF));
        }

        match self.try_lex(Self::lex_separator) {
            Ok(token) => return Ok(token),
            Err(_) => ()
        }

        match self.try_lex(Self::lex_operator) {
            Ok(token) => return Ok(token),
            Err(_) => ()
        }

        match self.try_lex(Self::lex_keyword) {
            Ok(token) => return Ok(token),
            Err(_) => ()
        }

        match self.try_lex(Self::lex_boolean_literal) {
            Ok(token) => return Ok(token),
            Err(_) => ()
        }

        match self.try_lex(Self::lex_char_literal) {
            Ok(token) => return Ok(token),
            Err(_) => ()
        }

        match self.try_lex(Self::lex_string_literal) {
            Ok(token) => return Ok(token),
            Err(_) => ()
        }

        match self.try_lex(Self::lex_int_literal) {
            Ok(token) => return Ok(token),
            Err(_) => ()
        }

        match self.try_lex(Self::lex_identifier) {
            Ok(token) => return Ok(token),
            Err(_) => ()
        }

        let current_char = self.current_char();
        let message = format!("failed to lex at line: {:?}, col: {:?}, position: {:?}, char: {:?}",
                              self.line,
                              self.col,
                              self.current,
                              current_char);
        Err(LexError::Message(message))
    }

    fn lex_separator(&mut self) -> Result<Token, LexError> {
        return match self.chomp() {
            '(' => Ok(Token::from_data(&self, OpenParen)),
            ')' => Ok(Token::from_data(&self, CloseParen)),
            '{' => Ok(Token::from_data(&self, OpenCurlyBracket)),
            '}' => Ok(Token::from_data(&self, CloseCurlyBracket)),
            '[' => Ok(Token::from_data(&self, OpenSquareBracket)),
            ']' => Ok(Token::from_data(&self, CloseSquareBracket)),
            '@' => Ok(Token::from_data(&self, AtSign)),
            '|' => {
                return match self.chomp() {
                    '|' => Err(LexError::Message("could not lex separator pipe as it is part of LogicalOr token to picked up by operator lexing".to_string())),
                    _ => Ok(Token::from_data(&self, Pipe)),
                }
                    
            },
            '-' => {
                return match self.chomp() {
                    '>' => Ok(Token::from_data(&self, Arrow)),
                    _ => Err(LexError::Message("could not lex separator".to_string()))
                }
            },
            '=' => {
                return match self.chomp() {
                    '>' => Ok(Token::from_data(&self, FatArrow)),
                    _ => Err(LexError::Message("could not lex separator".to_string()))
                }
            },
            ':' => {
                return match self.current_char() {
                    ':' => {
                        self.chomp();
                        Ok(Token::from_data(&self, DoubleColon))
                    },
                    '=' => {
                        Err(LexError::Message("could not lex separator".to_string()))
                    },
                    _ => Ok(Token::from_data(&self, Colon))
                }
            },
            ';' => Ok(Token::from_data(&self, Semicolon)),
            ',' => Ok(Token::from_data(&self, Comma)),
            _ => Err(LexError::Message("could not lex separator".to_string())),
        }
    }

    fn lex_operator(&mut self) -> Result<Token, LexError> {
        match self.chomp() {
            '&' => {
                return match self.current_char() {
                    '&' => {
                        self.chomp();
                        Ok(Token::from_data(&self, LogicalAnd))
                    },
                    _ => Ok(Token::from_data(&self, ReferenceOf))
                }
            }
            '*' => Ok(Token::from_data(&self, Multiply)),
            '/' => Ok(Token::from_data(&self, Divide)),
            '+' => Ok(Token::from_data(&self, Plus)),
            '-' => Ok(Token::from_data(&self, Minus)),
            '!' => {
                return match self.current_char() {
                    '=' => {
                        self.chomp();
                        Ok(Token::from_data(&self, NotEqualTo))
                    },
                    _ => Ok(Token::from_data(&self, LogicalNot))
                }
            }
            '|' => {
                return match self.chomp() {
                    '|' => Ok(Token::from_data(&self, LogicalOr)),
                    _ => Err(LexError::Message("could not lex operator".to_string()))
                }
            }
            '<' => {
                return match self.current_char() {
                    '=' => {
                        self.chomp();
                        Ok(Token::from_data(&self, LessThanOrEqual))
                    },
                    _ => Ok(Token::from_data(&self, LessThan))
                }
            },
            '>' => {
                return match self.current_char() {
                    '=' => {
                        self.chomp();
                        Ok(Token::from_data(&self, GreaterThanOrEqual))
                    },
                    _ => Ok(Token::from_data(&self, GreaterThan))
                }
            },
            ':' => {
                return match self.chomp() {
                    '=' => Ok(Token::from_data(&self, Assignment)),
                    _ => Err(LexError::Message("could not lex operator".to_string()))
                }
            },
            '=' => {
                return match self.chomp() {
                    '=' => Ok(Token::from_data(&self, EqualTo)),
                    _ => Err(LexError::Message("could not lex operator".to_string()))
                }
            },
            '.' => Ok(Token::from_data(&self, Dot)),
            _ => Err(LexError::Message("could not lex operator".to_string())),
        }
    }

    fn lex_keyword(&mut self) -> Result<Token, LexError> {
        let start = self.start;
        let next_word = self.chomp_while_any_of(VALID_IDENTIFIER_STARTER);

        match &*next_word {
            TYPE      => Ok(Token::from_data(&self, Type)),
            DEPENDENT => Ok(Token::from_data(&self, Dependent)),
            ON        => Ok(Token::from_data(&self, On)),
            MODULE    => Ok(Token::from_data(&self, Module)),
            CONTRACT  => Ok(Token::from_data(&self, Contract)),
            ENUM      => Ok(Token::from_data(&self, Enum)),
            OBJECT    => Ok(Token::from_data(&self, Object)),
            FIELDS    => Ok(Token::from_data(&self, Fields)),
            VARIANTS  => Ok(Token::from_data(&self, Variants)),
            METHODS   => Ok(Token::from_data(&self, Methods)),
            FUNCTIONS => Ok(Token::from_data(&self, Functions)),
            FUNC      => Ok(Token::from_data(&self, Func)),
            IMPLEMENT => Ok(Token::from_data(&self, Implement)),
            MATCH     => Ok(Token::from_data(&self, Match)),
            FOR       => Ok(Token::from_data(&self, For)),
            IF        => Ok(Token::from_data(&self, If)),
            ELSE      => Ok(Token::from_data(&self, Else)),
            BREAK     => Ok(Token::from_data(&self, Break)),
            CONTINUE  => Ok(Token::from_data(&self, Continue)),
            SELF_TYPE => Ok(Token::from_data(&self, SelfType)),
            SELF_VAR  => Ok(Token::from_data(&self, SelfVariable)),
            PUBLIC    => Ok(Token::from_data(&self, Public)),
            RETURN    => Ok(Token::from_data(&self, Return)),
            LET       => Ok(Token::from_data(&self, Let)),
            USE       => Ok(Token::from_data(&self, Use)),
            _ => {
                self.start = start;
                Err(LexError::Message("could not lex keyword".to_string()))
            }
        }
    }

    fn lex_boolean_literal(&mut self) -> Result<Token, LexError> {
        let next_word = self.chomp_while_any_of(VALID_IDENTIFIER_STARTER);

        match &*next_word {
            TRUE => Ok(Token::from_data(&self, BooleanLit(true))),
            FALSE => Ok(Token::from_data(&self, BooleanLit(false))),
            _ => {
                Err(LexError::Message("could not lex boolean lit".to_string()))
            }
        }
    }

    fn lex_char_literal(&mut self) -> Result<Token, LexError> {
        let current_char = self.current_char();
        return match current_char {
            '\'' => {
                self.current = self.current + 1;
                let char_lit = self.chomp_until(SINGLE_QUOTE);

                // TODO -> eval char

                Ok(Token::from_data(&self, CharLit('f')))
            },
            _ => Err(LexError::Message("could not lex char literal".to_string()))
        }
    }

    fn lex_string_literal(&mut self) -> Result<Token, LexError> {
        return match self.current_char() {
            '"' => {
                self.current = self.current + 1;
                let string_lit = self.chomp_until(DOUBLE_QUOTE);
                Ok(Token::from_data(&self, StringLit(string_lit)))
            },
            _ => Err(LexError::Message("could not lex string literal".to_string()))
        }
    }

    fn lex_float_literal(&mut self) -> Result<Token, LexError> {
        let integral_part = self.chomp_while_any_of(DIGITS);

        return match self.current_char() {
            '.' => {
                self.chomp();
                let fractional_part = self.chomp_while_any_of(DIGITS);
                let f32_string = format!("{}.{}", integral_part, fractional_part);
                let f = f32::from_str(&f32_string).unwrap();
                Ok(Token::from_data(&self, FloatLit(f)))
            },
            _ => Err(LexError::Message("could not lex float literal".to_string())),
        }
    }

    fn lex_int_literal(&mut self) -> Result<Token, LexError> {
        return match self.chomp_while_any_of(DIGITS).as_ref() {
            "" => Err(LexError::Message("could not lex int literal".to_string())),
            integral_part => {
                let i = i32::from_str(&integral_part).unwrap();
                Ok(Token::from_data(&self, IntegerLit(i)))
            }
        }
    }

    fn lex_identifier(&mut self) -> Result<Token, LexError> {
        if self.chomp_next(VALID_IDENTIFIER_STARTER).is_empty() {
            return Err(LexError::Message("could not lex identifier".to_string()));
        }

        self.chomp_while_any_of(VALID_IDENTIFIER_FINISHER);
        if self.lexing.is_empty() {
            return Err(LexError::Message("could not lex identifier".to_string()));
        }


        Ok(Token::from_data(&self, Identifier(self.lexing.iter().collect())))
    }

    fn chomp_until(&mut self, chomp_str: &str) -> String {
        let chomp_set: HashSet<char, RandomState> = HashSet::from_iter(chomp_str.to_string().chars());

        let mut chomped = Vec::new();
        
        loop {
            let current = self.chomp();
            if chomp_set.contains(&current) {
                break;
            }

            chomped.push(current);
        }

        chomped.iter().collect()
    }

    fn chomp_while_any_of(&mut self, chomp_str: &str) -> String {
        let chomp_set: HashSet<char, RandomState> = HashSet::from_iter(chomp_str.to_string().chars());
        
        let mut chomped = Vec::new();

        loop {
            if (self.current + 1) >= self.input.len().try_into().unwrap() {
                break;
            }    

            let current = self.current_char();
            if !chomp_set.contains(&current) {
                break;
            }

            chomped.push(self.chomp());
        }

        chomped.iter().collect()
    }

    fn chomp_next(&mut self, chomp_str: &str) -> String {
        let chomp_set: HashSet<char, RandomState> = HashSet::from_iter(chomp_str.to_string().chars());
        
        let mut chomped = Vec::new();
       
        let current = self.current_char();
        if chomp_set.contains(&current) {
            chomped.push(self.chomp());
        }

        chomped.iter().collect()
    }

    fn chomp(&mut self) -> char {
        let current_char = self.current_char();
        self.lexing.push(current_char);
        self.current += 1;

        if current_char == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }

        return current_char;
    }

    fn current_char(&mut self) -> char {
        let index = self.current as usize;
        self.input.chars().nth(index).unwrap()
    }
}
