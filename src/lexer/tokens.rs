use std::fmt::Display;

#[derive(Debug, Clone)]
pub(crate) enum Token {
    // operators
    ReferenceOf,
    Divide,
    Multiply,
    Plus,
    Minus,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Assignment,
    EqualTo,
    NotEqualTo,
    Dot,
   
    // separators
    OpenParen,
    CloseParen,
    OpenCurlyBracket,
    CloseCurlyBracket,
    OpenSquareBracket,
    CloseSquareBracket,
    OpenDoubleAngleBracket,
    CloseDoubleAngleBracket,
    Pipe,
    Arrow,
    FatArrow,
    Colon,
    DoubleColon,
    Semicolon,
    Comma,

    // keywords
    Type,
    Dependent,
    On,
    Module,
    Contract,
    Enum,
    Object,
    Fields,
    Variants,
    Methods,
    Functions,
    Func,
    Implement,
    Match,
    For,
    If,
    Else,
    Break,
    Continue,
    SelfType,
    SelfVariable,
    Public,
    Return,
    Let,
    Use,

    // literals
    BooleanLit(bool),
    StringLit(String),
    CharLit(char),
    IntegerLit(i32),
    FloatLit(f32),

    // other
    Identifier(String),
    EOF,
    LexError(String),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Token::*;

        let message = match self {
            ReferenceOf             => "ReferenceOf".to_string(),
            Divide                  => "Divide".to_string(),
            Multiply                => "Multiply".to_string(),
            Plus                    => "Plus".to_string(),
            Minus                   => "Minus".to_string(),
            LogicalNot              => "LogicalNot".to_string(),
            LogicalAnd              => "LogicalAnd".to_string(),
            LogicalOr               => "LogicalOr".to_string(),
            GreaterThan             => "GreaterThan".to_string(),
            GreaterThanOrEqual      => "GreaterThanOrEqual".to_string(),
            LessThan                => "LessThan".to_string(),
            LessThanOrEqual         => "LessThanOrEqual".to_string(),
            Assignment              => "Assignment".to_string(),
            EqualTo                 => "EqualTo".to_string(),
            NotEqualTo              => "NotEqualTo".to_string(),
            Dot                     => "Dot".to_string(),
            
            // separators
            OpenParen               => "OpenParen".to_string(),
            CloseParen              => "CloseParen".to_string(),
            OpenCurlyBracket        => "OpenCurlyBracket".to_string(),
            CloseCurlyBracket       => "CloseCurlyBracket".to_string(),
            OpenSquareBracket       => "OpenSquareBracket".to_string(),
            CloseSquareBracket      => "CloseSquareBracket".to_string(),
            OpenDoubleAngleBracket  => "OpenDoubleAngleBracket".to_string(),
            CloseDoubleAngleBracket => "CloseDoubleAngleBracket".to_string(),
            Pipe                    => "Pipe".to_string(),
            Arrow                   => "Arrow".to_string(),
            FatArrow                => "FatArrow".to_string(),
            Colon                   => "Colon".to_string(),
            DoubleColon             => "DoubleColon".to_string(),
            Semicolon               => "Semicolon".to_string(),
            Comma                   => "Comma".to_string(),

            // keywords
            Type               => "Type".to_string(),
            Dependent          => "Dependent".to_string(),
            On                 => "On".to_string(),
            Module             => "Module".to_string(),
            Contract           => "Contract".to_string(),
            Enum               => "Enum".to_string(),
            Object             => "Object".to_string(),
            Fields             => "Fields".to_string(),
            Variants           => "Variants".to_string(),
            Methods            => "Methods".to_string(),
            Functions          => "Functions".to_string(),
            Func               => "Func".to_string(),
            Implement          => "Implement".to_string(),
            Match              => "Match".to_string(),
            For                => "For".to_string(),
            If                 => "If".to_string(),
            Else               => "Else".to_string(),
            Break              => "Break".to_string(),
            Continue           => "Continue".to_string(),
            SelfType           => "SelfType".to_string(),
            SelfVariable       => "SelfVariable".to_string(),
            Public             => "Public".to_string(),
            Return             => "Return".to_string(),
            Let                => "let".to_string(),
            Use                => "use".to_string(),

            // literals
            BooleanLit(b)      => format!("BooleanLit({})", b),
            StringLit(s)       => format!("StringLit({})", s),
            CharLit(c)         => format!("CharLit({})", c),
            IntegerLit(i)      => format!("IntegerLit({})", i),
            FloatLit(f)        => format!("FloatLit({})", f),

            // other
            Identifier(id)     => format!("Identifier({})", id),
            EOF                => "<<EOF>>".to_string(),
            LexError(err)      => format!("LexError({})", err),
        };

        write!(f, "{}", message)
    }
}

pub(crate) const VALID_IDENTIFIER_STARTER  : &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
pub(crate) const VALID_IDENTIFIER_FINISHER : &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789";

pub(crate) const DIGITS       : &str = "0123456789";

pub(crate) const SINGLE_QUOTE : &str = "'";
pub(crate) const DOUBLE_QUOTE : &str = "\"";

pub(crate) const TYPE         : &str = "type";
pub(crate) const DEPENDENT    : &str = "dependent";
pub(crate) const ON           : &str = "on";
pub(crate) const MODULE       : &str = "module";
pub(crate) const CONTRACT     : &str = "contract";
pub(crate) const ENUM         : &str = "enum";
pub(crate) const OBJECT       : &str = "object";
pub(crate) const FIELDS       : &str = "fields";
pub(crate) const VARIANTS     : &str = "variants";
pub(crate) const METHODS      : &str = "methods";
pub(crate) const FUNCTIONS    : &str = "functions";
pub(crate) const FUNC         : &str = "func";
pub(crate) const IMPLEMENT    : &str = "implement";
pub(crate) const MATCH        : &str = "match";
pub(crate) const FOR          : &str = "for";
pub(crate) const IF           : &str = "if";
pub(crate) const ELSE         : &str = "else";
pub(crate) const BREAK        : &str = "break";
pub(crate) const CONTINUE     : &str = "continue";
pub(crate) const SELF_TYPE    : &str = "Self";
pub(crate) const SELF_VAR     : &str = "self";
pub(crate) const PUBLIC       : &str = "public";
pub(crate) const RETURN       : &str = "return";
pub(crate) const LET          : &str = "let";
pub(crate) const TRUE         : &str = "true";
pub(crate) const FALSE        : &str = "false";
pub(crate) const USE          : &str = "use";

