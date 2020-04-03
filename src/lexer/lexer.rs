use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::convert::TryInto;
use std::error::Error;
use std::fmt::Display;
use std::iter::FromIterator;
use std::str::FromStr;

use crate::lexer::tokens::*;
use crate::lexer::tokens::Token::*;

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
    input: String,
    start: i32,
    current: i32,
    lexing: Vec<char>,
    tokens: Vec<Token>
}

impl Lexer {
    pub(crate) fn new() -> Self {
        Self {
            input: String::new(),
            start: 0,
            current: 0,
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
                Ok(Token::EOF) => {
                    self.emit_token(Token::EOF);
                    return Ok(self.tokens.drain(..).collect());
                }
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

    fn snapshot(&mut self) -> (i32, i32, Vec<char>) {
        return (self.start, self.current, self.lexing.clone());
    }

    fn reset(&mut self, snapshot: (i32, i32, Vec<char>)) {
        self.start = snapshot.0;
        self.current = snapshot.1;
        self.lexing = snapshot.2;
    }

    fn _lex(&mut self) -> Result<Token, LexError> {
        self.chomp_while_any_of("\t\n \r");
        self.drop_current();
        
        if (self.current + 1) >= self.input.len().try_into().unwrap() {
            return Ok(Token::EOF);
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
        let message = format!("failed to lex at position: {:?}, char: {:?}", self.current, current_char);
        Err(LexError::Message(message))
    }

    fn lex_separator(&mut self) -> Result<Token, LexError> {
        return match self.chomp() {
            '(' => Ok(OpenParen),
            ')' => Ok(CloseParen),
            '{' => Ok(OpenCurlyBracket),
            '}' => Ok(CloseCurlyBracket),
            '[' => Ok(OpenSquareBracket),
            ']' => Ok(CloseSquareBracket),
            '|' => {
                return match self.chomp() {
                    '|' => Err(LexError::Message("could not lex separator pipe as it is part of LogicalOr token to picked up by operator lexing".to_string())),
                    _ => Ok(Pipe),
                }
                    
            },
            '-' => {
                return match self.chomp() {
                    '>' => Ok(Arrow),
                    _ => Err(LexError::Message("could not lex separator".to_string()))
                }
            },
            '=' => {
                return match self.chomp() {
                    '>' => Ok(FatArrow),
                    _ => Err(LexError::Message("could not lex separator".to_string()))
                }
            },
            ':' => {
                return match self.current_char() {
                    ':' => {
                        self.chomp();
                        Ok(DoubleColon)
                    },
                    '=' => {
                        Err(LexError::Message("could not lex separator".to_string()))
                    },
                    _ => Ok(Colon)
                }
            },
            ';' => Ok(Semicolon),
            ',' => Ok(Comma),
            _ => Err(LexError::Message("could not lex separator".to_string())),
        }
    }

    fn lex_operator(&mut self) -> Result<Token, LexError> {
        match self.chomp() {
            '&' => {
                return match self.current_char() {
                    '&' => {
                        self.chomp();
                        Ok(LogicalAnd)
                    },
                    _ => Ok(ReferenceOf)
                }
            }
            '*' => Ok(Multiply),
            '/' => Ok(Divide),
            '+' => Ok(Plus),
            '-' => Ok(Minus),
            '!' => {
                return match self.current_char() {
                    '=' => {
                        self.chomp();
                        Ok(NotEqualTo)
                    },
                    _ => Ok(LogicalNot)
                }
            }
            '|' => {
                return match self.chomp() {
                    '|' => Ok(LogicalOr),
                    _ => Err(LexError::Message("could not lex operator".to_string()))
                }
            }
            '<' => {
                return match self.current_char() {
                    '<' => {
                        self.chomp();
                        Ok(OpenDoubleAngleBracket)
                    },
                    '=' => {
                        self.chomp();
                        Ok(LessThanOrEqual)
                    },
                    _ => Ok(LessThan)
                }
            },
            '>' => {
                return match self.current_char() {
                    '>' => {
                        self.chomp();
                        Ok(CloseDoubleAngleBracket)
                    },
                    '=' => {
                        self.chomp();
                        Ok(GreaterThanOrEqual)
                    },
                    _ => Ok(GreaterThan)
                }
            },
            ':' => {
                return match self.chomp() {
                    '=' => Ok(Assignment),
                    _ => Err(LexError::Message("could not lex operator".to_string()))
                }
            },
            '=' => {
                return match self.chomp() {
                    '=' => Ok(EqualTo),
                    _ => Err(LexError::Message("could not lex operator".to_string()))
                }
            },
            '.' => Ok(Dot),
            _ => Err(LexError::Message("could not lex operator".to_string())),
        }
    }

    fn lex_keyword(&mut self) -> Result<Token, LexError> {
        let start = self.start;
        let next_word = self.chomp_while_any_of(VALID_IDENTIFIER_STARTER);

        match &*next_word {
            TYPE => Ok(Type),
            DEPENDENT => Ok(Dependent),
            ON => Ok(On),
            MODULE => Ok(Module),
            CONTRACT => Ok(Contract),
            ENUM => Ok(Enum),
            OBJECT => Ok(Object),
            FIELDS => Ok(Fields),
            VARIANTS => Ok(Variants),
            METHODS => Ok(Methods),
            FUNCTIONS => Ok(Functions),
            FUNC => Ok(Func),
            IMPLEMENT => Ok(Implement),
            MATCH => Ok(Match),
            FOR => Ok(For),
            IF => Ok(If),
            ELSE => Ok(Else),
            BREAK => Ok(Break),
            CONTINUE => Ok(Continue),
            SELF_TYPE => Ok(SelfType),
            SELF_VAR => Ok(SelfVariable),
            PUBLIC => Ok(Public),
            RETURN => Ok(Return),
            LET => Ok(Let),
            USE => Ok(Use),
            _ => {
                self.start = start;
                Err(LexError::Message("could not lex keyword".to_string()))
            }
        }
    }

    fn lex_boolean_literal(&mut self) -> Result<Token, LexError> {
        let next_word = self.chomp_while_any_of(VALID_IDENTIFIER_STARTER);

        match &*next_word {
            TRUE => Ok(BooleanLit(true)),
            FALSE => Ok(BooleanLit(false)),
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

                Ok(CharLit('f'))
            },
            _ => Err(LexError::Message("could not lex char literal".to_string()))
        }
    }

    fn lex_string_literal(&mut self) -> Result<Token, LexError> {
        return match self.current_char() {
            '"' => {
                self.current = self.current + 1;
                let string_lit = self.chomp_until(DOUBLE_QUOTE);
                Ok(StringLit(string_lit))
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
                Ok(FloatLit(f))
            },
            _ => Err(LexError::Message("could not lex float literal".to_string())),
        }
    }

    fn lex_int_literal(&mut self) -> Result<Token, LexError> {
        return match self.chomp_while_any_of(DIGITS).as_ref() {
            "" => Err(LexError::Message("could not lex int literal".to_string())),
            integral_part => {
                let i = i32::from_str(&integral_part).unwrap();
                Ok(IntegerLit(i))
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
        
        Ok(Identifier(self.lexing.iter().collect()))
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
        self.current = self.current + 1;
        return current_char;
    }

    fn current_char(&mut self) -> char {
        let index = self.current as usize;
        self.input.chars().nth(index).unwrap()
    }
}
