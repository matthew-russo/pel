use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;

use crate::lexer::tokens::*;
use crate::syntax::parse_tree::*;
use crate::utils::utils;

#[derive(Debug, Clone)]
pub(crate) enum ParseError {
    Message(String),
    Source(Box<ParseError>)
}

impl ParseError {
    fn of(err: ParseError) -> Self {
        ParseError::Source(Box::new(err))
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseError::Message(m) => write!(f, "parse error: {}", m),
            ParseError::Source(err) => err.fmt(f)
        }
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseError::Message(_) => None,
            ParseError::Source(err) => Some(err),
        }
    }
}

const IDENTIFIER: Token = Token::Identifier(String::new());
const STRING_LIT: Token = Token::StringLit(String::new());
const CHAR_LIT: Token = Token::CharLit('y');
const BOOL_LIT: Token = Token::BooleanLit(true);
const INT_LIT: Token = Token::IntegerLit(42);
const FLOAT_LIT: Token = Token::FloatLit(42.0);

#[derive(Debug)]
pub(crate) struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens,
            current: 0,
        }
    }

    pub(crate) fn parse(&mut self) -> Result<Program, ParseError> {
        self.program()
    }

    fn expect(&mut self, expected_token: Token, currently_parsing: &str) -> Result<Token, ParseError> {
        let next_token = self.current_token();

        if utils::discriminants_equal(&expected_token, &next_token) {
            self.current = self.current + 1;
            return Ok(next_token);
        }

        Err(ParseError::Message(format!("expected {} while parsing {} but got {} at {}", expected_token, currently_parsing, next_token, self.current)))
    }

    fn current_token(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn extract_identifier(t: &Token) -> Option<String> {
        // TODO -> maybe get rid of clone
        return match t {
            Token::Identifier(id) => Some(id.clone()),
            _ => None
        }
    }

    fn program(&mut self) -> Result<Program, ParseError> {
        let mut declarations = Vec::new();
        loop {
            if let Token::EOF = self.current_token() {
                break
            }

            match self.declaration() {
                Ok(decl) => declarations.push(decl),
                Err(e) => return Err(e),
            }
        }

        Ok(Program {
            declarations,
        })
    }

    fn declaration(&mut self) -> Result<Declaration, ParseError> {
        return match self.current_token() {
            Token::Type => {
                let message = format!("unexpected reserved type token");
                Err(ParseError::Message(message))
            },
            Token::Enum => {
                let enum_decl = self.enum_declaration()?;
                Ok(Declaration::EnumDeclarationNode(enum_decl))
            },
            Token::Contract => {
                let contract_decl = self.contract_declaration()?;
                Ok(Declaration::ContractDeclarationNode(contract_decl))
            },
            Token::Implement => {
                let impl_decl = self.implementation_declaration()?;
                Ok(Declaration::ImplementationDeclarationNode(impl_decl))
            },
            Token::Object => {
                let obj_decl = self.object_declaration()?;
                Ok(Declaration::ObjectDeclarationNode(obj_decl))
            },
            Token::Func => {
                let func_decl = self.function_declaration()?;
                Ok(Declaration::FunctionDeclarationNode(func_decl))
            },
            t => {
                let message = format!("Unable to parse any type of declaration at token: {}, pos: {}", t, self.current);
                Err(ParseError::Message(message))
            }
        };
    }

    fn generic_params(&mut self) -> Result<Vec<String>, ParseError> {
        self.expect(Token::OpenDoubleAngleBracket, "generic_params")?;
       
        let mut params = Vec::new();

        match self.current_token() {
            Token::Identifier(id) => {
                self.expect(IDENTIFIER, "generic_params")?;
                params.push(id);

                loop {
                    match self.current_token() {
                        Token::CloseDoubleAngleBracket => {
                            self.expect(Token::CloseDoubleAngleBracket, "generic_params")?;
                            break;
                        },
                        Token::Comma => {
                            self.expect(Token::Comma, "generic_params")?;
                            let id_token = self.expect(IDENTIFIER, "generic_params")?;
                            let id = Self::extract_identifier(&id_token).unwrap();
                            params.push(id);
                        },
                        t => {
                            let message = format!("unexpected token: {} while parsing generic_params", t);
                            panic!(message);
                        },
                    }
                }
            },
            _ => {
                self.expect(Token::CloseDoubleAngleBracket, "generic_params")?;
            }
        }

        Ok(params)
    }

    fn generic_args(&mut self) -> Result<Vec<Expression>, ParseError> {
        self.expect(Token::OpenDoubleAngleBracket, "generic_args")?;
       
        let mut args = Vec::new();

        match self.current_token() {
            Token::CloseDoubleAngleBracket => {
                    self.expect(Token::CloseDoubleAngleBracket, "generic_args")?;
            },
            _ => {
                let expr = self.expression()?;
                args.push(expr);
                loop {
                    match self.current_token() {
                        Token::CloseDoubleAngleBracket => {
                            self.expect(Token::CloseDoubleAngleBracket, "generic_args")?;
                            break;
                        },
                        Token::Comma => {
                            self.expect(Token::Comma, "generic_args")?;
                            let expr = self.expression()?;
                            args.push(expr);
                        },
                        t => {
                            let message = format!("unexpected token: {} while parsing generic_args", t);
                            panic!(message);
                        },
                    }
                }
            }
        }

        Ok(args)
    }

    fn enum_declaration(&mut self) -> Result<EnumDeclaration, ParseError> {
        self.expect(Token::Enum, "enum_declaration")?;
        let type_name_token = self.expect(IDENTIFIER, "enum_declaration")?;
        let type_name = Self::extract_identifier(&type_name_token).unwrap();

        let type_params = match self.current_token() {
            Token::OpenDoubleAngleBracket => {
                let generic_params = self.generic_params()?;
                generic_params
            },
            _ => Vec::new(),
        };
        
        self.expect(Token::OpenCurlyBracket, "enum_declaration")?;
        let variants = self.variants()?;

        let methods = match self.current_token() {
            Token::Methods => self.methods()?,
            _ => Vec::new(),
        };

        self.expect(Token::CloseCurlyBracket, "enum_declaration")?;

        Ok(EnumDeclaration {
            type_name,
            type_params,
            variants,
            methods,
        })
    }

    fn object_declaration(&mut self) -> Result<ObjectDeclaration, ParseError> {
        self.expect(Token::Object, "object_declaration")?;
        let type_name_token = self.expect(IDENTIFIER, "object_declaration")?;
        let type_name = Self::extract_identifier(&type_name_token).unwrap();

        let type_params = match self.current_token() {
            Token::OpenDoubleAngleBracket => {
                let generic_params = self.generic_params()?;
                generic_params
            },
            _ => Vec::new(),
        };

        self.expect(Token::OpenCurlyBracket, "object_declaration")?;
        let fields = self.fields()?;
        
        let methods = match self.tokens[self.current] {
            Token::Methods => self.methods()?,
            _ => Vec::new(),
        };

        self.expect(Token::CloseCurlyBracket, "object_declaration")?;

        Ok(ObjectDeclaration {
            type_name,
            type_params,
            fields,
            methods,
        })
    }

    fn contract_declaration(&mut self) -> Result<ContractDeclaration, ParseError> {
        self.expect(Token::Contract, "contract_declaration")?;
        let type_name_token = self.expect(IDENTIFIER, "contract_declaration")?;
        let type_name = Self::extract_identifier(&type_name_token).unwrap();

        let type_params = match self.current_token() {
            Token::OpenDoubleAngleBracket => {
                let generic_params = self.generic_params()?;
                generic_params
            },
            _ => Vec::new(),
        };

        self.expect(Token::OpenCurlyBracket, "contract_declaration")?;

        let mut functions = Vec::new();

        loop {
            match self.function_signature() {
                Ok(func) => functions.push(func),
                Err(_) => break
            }
            self.expect(Token::Semicolon, "contract_declaration")?;
        }
        
        self.expect(Token::CloseCurlyBracket, "contract_declaration")?;

        Ok(ContractDeclaration {
            type_name,
            type_params, 
            functions,
        })
    }

    fn implementation_declaration(&mut self) -> Result<ImplementationDeclaration, ParseError> {
        self.expect(Token::Implement, "implementation_declaration")?;
        let contract = self.expression()?;

        self.expect(Token::For, "implementation_declaration")?;
        let implementing_type = self.expression()?;

        self.expect(Token::OpenCurlyBracket, "implementation_declaration")?;

        let mut functions = Vec::new();

        loop {
            match self.function_declaration() {
                Ok(func_decl) => functions.push(func_decl),
                Err(_) => break
            }
        }

        self.expect(Token::CloseCurlyBracket, "implementation_declaration")?;

        Ok(ImplementationDeclaration {
            implementing_type,
            contract,
            functions,
        })
    }

    fn function_declaration(&mut self) -> Result<FunctionDeclaration, ParseError> {
        let visibility = match self.visibility() {
            Ok(v) => Some(v),
            Err(_) => None
        };
        let signature = self.function_signature()?;
        self.expect(Token::OpenCurlyBracket, "function_declaration")?;
        let body = self.block_body()?;
        self.expect(Token::CloseCurlyBracket, "function_declaration")?;
       
        Ok(FunctionDeclaration {
            visibility,
            signature,
            body,
        })
    }

    fn visibility(&mut self) -> Result<Visibility, ParseError> {
        return match self.tokens[self.current] {
            Token::Public => {
                self.expect(Token::Public, "visibility")?;
                Ok(Visibility::Public)
            },
            _ => {
                let message = format!("unable to parse visibility at {}", self.tokens[self.current]);
                Err(ParseError::Message(message))
            }
        }
    }

    fn function_signature(&mut self) -> Result<FunctionSignature, ParseError> {
        self.expect(Token::Func, "function_signature")?;
        let name_token = self.expect(IDENTIFIER, "function_signature")?;
        let name = Self::extract_identifier(&name_token).unwrap();

        let type_parameters = match self.current_token() {
            Token::OpenDoubleAngleBracket => {
                let generic_params = self.generic_params()?;
                generic_params
            },
            _ => Vec::new(),
        };

        self.expect(Token::OpenParen, "function_signature")?;

        let mut parameters = Vec::new();

        match self.current_token() {
            Token::CloseParen => (),
            _ => {
                let param = self.function_parameter()?;
                parameters.push(param);
                loop {
                    match self.current_token() {
                        Token::Comma => {
                            self.expect(Token::Comma, "function_signature")?;
                            let param = self.function_parameter()?;
                            parameters.push(param);
                        },
                        _ => break
                    }
                }
            },
        }

        self.expect(Token::CloseParen, "function_signature")?;

        let returns = match self.current_token() {
            Token::Arrow => {
                self.expect(Token::Arrow, "function_signature").unwrap();
                // TODO -> figure out how to parse this

                match self.current_token() {
                    Token::SelfType => {
                        self.expect(Token::SelfType, "function_signature")?;
                        let chainable_expr = Box::new(ChainableExpression {
                            start: ExpressionStart::VariableNode(Variable::SelfType),
                            chained: Vec::new(),
                        });
                        Some(Expression::ChainableExpressionNode(chainable_expr))
                    },
                    Token::Identifier(id) => {
                        self.expect(IDENTIFIER, "function_signature")?;
                        let chainable_expr = Box::new(ChainableExpression {
                            start: ExpressionStart::VariableNode(Variable::Name(id)),
                            chained: Vec::new(),
                        });
                        Some(Expression::ChainableExpressionNode(chainable_expr))
                    },
                    _ => {
                        None
                    }
                }
            },
            _ => None
        };

        Ok(FunctionSignature {
            name,
            type_parameters,
            parameters,
            returns,
        })
    }

    fn function_parameter(&mut self) -> Result<FunctionParameter, ParseError> {
        return match self.current_token() {
            Token::SelfVariable => {
                self.expect(Token::SelfVariable, "function_parameter")?;
                Ok(FunctionParameter::SelfParam)
            },
            _ => {
                let typed_var_decl = self.typed_variable_declaration()?;
                Ok(FunctionParameter::TypedVariableDeclarationParam(typed_var_decl))
            }
        }
    }

    fn typed_variable_declaration(&mut self) -> Result<TypedVariableDeclaration, ParseError> {
        let name_token = self.expect(IDENTIFIER, "typed_variable_declaration")?;
        let name = Self::extract_identifier(&name_token).unwrap();
        self.expect(Token::Colon, "typed_variable_declaration")?;
        let type_reference = self.expression()?;

        Ok(TypedVariableDeclaration {
            name,
            type_reference,
        })
    }

    fn variants(&mut self) -> Result<Vec<VariantDeclaration>, ParseError> {
        self.expect(Token::Variants, "variants")?;
        self.expect(Token::OpenCurlyBracket, "variants")?;
        
        let mut variants = Vec::new();

        loop {
            match self.variant_declaration() {
                Ok(variant) => variants.push(variant),
                Err(_) => break,
            }
        }
        
        self.expect(Token::CloseCurlyBracket, "variants")?;

        Ok(variants)
    }

    fn variant_declaration(&mut self) -> Result<VariantDeclaration, ParseError> {
        let name_token = self.expect(IDENTIFIER, "variant_declaration")?;
        let name = Self::extract_identifier(&name_token).unwrap();
        
        let mut contains = None;

        match self.expect(Token::OpenParen, "variant_declaration") {
            Ok(_) => {
                let type_reference = self.expression()?;
                self.expect(Token::CloseParen, "variant_declaration")?;
                contains = Some(type_reference)
            },
            Err(_) => (),
        }

        self.expect(Token::Comma, "variant_declaration")?;

        Ok(VariantDeclaration {
            name,
            contains,
        })
    }

    fn fields(&mut self) -> Result<Vec<TypedVariableDeclaration>, ParseError> {
        self.expect(Token::Fields, "fields")?;
        self.expect(Token::OpenCurlyBracket, "fields")?;
        
        let mut fields = Vec::new();

        loop {
            match self.typed_variable_declaration() {
                Ok(tvd) => fields.push(tvd),
                Err(_) => break
            }

            self.expect(Token::Comma, "fields")?;
        }

        self.expect(Token::CloseCurlyBracket, "fields")?;

        Ok(fields)
    }

    fn methods(&mut self) -> Result<Vec<FunctionDeclaration>, ParseError> {
        self.expect(Token::Methods, "methods")?;
        self.expect(Token::OpenCurlyBracket, "methods")?;
        
        let mut methods = Vec::new();

        loop {
            match self.function_declaration() {
                Ok(method) => methods.push(method),
                Err(e) => break
            }
        }
    
        self.expect(Token::CloseCurlyBracket, "methods")?;

        Ok(methods)
    }

    fn functions(&mut self) -> Result<Vec<FunctionDeclaration>, ParseError> {
        self.expect(Token::Functions, "functions")?;
        self.expect(Token::OpenCurlyBracket, "functions")?;
        
        let mut functions = Vec::new();

        loop {
            match self.function_declaration() {
                Ok(function) => functions.push(function),
                Err(e) => break
            }
        }
    
        self.expect(Token::CloseCurlyBracket, "functions")?;

        Ok(functions)
    }

    fn block_body(&mut self) -> Result<BlockBody, ParseError> {
        let mut statements = Vec::new();

        loop {
            if let Token::CloseCurlyBracket = self.current_token() {
                break;
            }

            let stmt = self.statement()?;
            statements.push(stmt);
        }

        Ok(BlockBody {
            statements,
        })
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        return match self.current_token() {
            Token::Let => {
                let assignment = self.variable_assignment()?;
                self.expect(Token::Semicolon, "statement")?;
                Ok(Statement::VariableAssignmentNode(assignment))
            },
            Token::Return => {
                let return_stmt = self.return_stmt()?;
                self.expect(Token::Semicolon, "statement")?;
                Ok(Statement::ReturnNode(return_stmt))        
            },
            _ => {
                let expr = self.expression()?;
                self.expect(Token::Semicolon, "statement")?;
                Ok(Statement::ExpressionNode(expr))
            }
        }
    }

    fn return_stmt(&mut self) -> Result<Return, ParseError> {
        self.expect(Token::Return, "statement")?;
        let value = self.expression()?;

        Ok(Return {
            value,
        })
    }
 
    fn expression(&mut self) -> Result<Expression, ParseError> {
        return match self.current_token() {
            Token::LogicalNot => {
                self.expect(Token::LogicalNot, "expression")?;
                let expr = self.chainable_expression()?;
                let op = UnaryOperation {
                    op: UnaryOperator::Not,
                    expr: expr,
                };
                Ok(Expression::UnaryOperationNode(Box::new(op)))
            },
            Token::Pipe => {
                let lambda = self.lambda()?;
                Ok(Expression::LambdaNode(Box::new(lambda)))
            },
            _ => {
                let chainable_expr = self.chainable_expression()?;
                Ok(Expression::ChainableExpressionNode(Box::new(chainable_expr)))
            }
        }
    }

    fn lambda(&mut self) -> Result<Lambda, ParseError> {
        self.expect(Token::Pipe, "lambda")?;
       
        let mut params = Vec::new();
        match self.current_token() {
            Token::Identifier(id) => {
                let param = self.typed_variable_declaration()?;
                params.push(param);

                loop {
                    match self.current_token() {
                        Token::Comma => {
                            self.expect(Token::Comma, "lambda")?;
                            let param = self.typed_variable_declaration()?;
                            params.push(param);
                        },
                        _ => break
                    }
                }
            },
            _ => ()
        }

        self.expect(Token::Pipe, "lambda")?;
        self.expect(Token::OpenCurlyBracket, "lambda")?;
        let body = self.block_body()?;
        self.expect(Token::CloseCurlyBracket, "lambda")?;

        Ok(Lambda {
            params,
            body,
        })
    }

    fn chainable_expression(&mut self) -> Result<ChainableExpression, ParseError> {
        let start = self.expression_start()?;

        let mut chained = Vec::new();
        loop {
            match self.current_token() {
                Token::Dot => {
                    let current = self.current;
                    if let Ok(field_access) = self.field_access() {
                        chained.push(ExpressionChain::FieldAccessNode(field_access));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                Token::DoubleColon => {
                    let current = self.current;
                    if let Ok(mod_access) = self.module_access() {
                        chained.push(ExpressionChain::ModuleAccessNode(mod_access));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                Token::OpenCurlyBracket => {
                    let current = self.current;
                    if let Ok(obj_init) = self.object_initialization() {
                        chained.push(ExpressionChain::ObjectInitializationNode(obj_init));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                Token::OpenParen => {
                    let current = self.current;
                    if let Ok(func_app) = self.function_application() {
                        chained.push(ExpressionChain::FunctionApplicationNode(func_app));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                Token::OpenDoubleAngleBracket => {
                    let current = self.current;
                    if let Ok(type_application) = self.type_application() {
                        chained.push(ExpressionChain::TypeApplicationNode(type_application));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                Token::Plus => {
                    self.expect(Token::Plus, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Plus,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::Minus => {
                    self.expect(Token::Minus, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Minus,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::Multiply => {
                    self.expect(Token::Multiply, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Multiply,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::Divide => {
                    self.expect(Token::Divide, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Divide,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::LessThan => {
                    self.expect(Token::LessThan, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::LessThan,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::LessThanOrEqual => {
                    self.expect(Token::LessThanOrEqual, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::LessThanOrEqual,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::GreaterThan => {
                    self.expect(Token::GreaterThan, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::GreaterThan,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::GreaterThanOrEqual => {
                    self.expect(Token::GreaterThanOrEqual, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::GreaterThanOrEqual,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::EqualTo => {
                    self.expect(Token::EqualTo, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Equal,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::NotEqualTo => {
                    self.expect(Token::NotEqualTo, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::NotEqual,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::LogicalAnd => {
                    self.expect(Token::LogicalAnd, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::And,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                Token::LogicalOr => {
                    self.expect(Token::LogicalOr, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Or,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                _ => break,
            }
        }

        Ok(ChainableExpression {
            start,
            chained,
        })
    }

    fn expression_start(&mut self) -> Result<ExpressionStart, ParseError> {
        return match self.current_token() {
            Token::If => {
                let cond_expr = self.conditional_expr()?;
                Ok(ExpressionStart::ConditionalNode(cond_expr))
            },
            Token::Match => {
                let match_expr = self.match_expr()?;
                Ok(ExpressionStart::MatchNode(match_expr))
            },
            Token::For => {
                let loop_expr = self.loop_expr()?;
                Ok(ExpressionStart::LoopNode(loop_expr))
            },
            Token::SelfVariable => {
                self.expect(Token::SelfVariable, "expression_start")?;
                Ok(ExpressionStart::VariableNode(Variable::SelfVariable))
            },
            Token::SelfType => {
                self.expect(Token::SelfType, "expression_start")?;
                Ok(ExpressionStart::VariableNode(Variable::SelfType))
            },
            Token::Identifier(id) => {
                self.expect(IDENTIFIER, "expression_start")?;
                Ok(ExpressionStart::VariableNode(Variable::Name(id)))
            },
            Token::StringLit(s) => {
                self.expect(STRING_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::StringValue(s)))
            },
            Token::CharLit(c) => {
                self.expect(CHAR_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::CharValue(c)))
            },
            Token::BooleanLit(b) => {
                self.expect(BOOL_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::BooleanValue(b)))
            },
            Token::IntegerLit(i) => {
                self.expect(INT_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::IntegerValue(i)))
            },
            Token::FloatLit(f) => {
                self.expect(FLOAT_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::FloatValue(f)))
            },
            t => {
                let message = format!("unable to parse expression_start at {}, pos: {}", t, self.current);
                Err(ParseError::Message(message))
            }
        }
    }

    fn field_access(&mut self) -> Result<FieldAccess, ParseError> {
        self.expect(Token::Dot, "field_access")?;
        let id_token = self.expect(IDENTIFIER, "field_access")?;
        let id = Self::extract_identifier(&id_token).unwrap();
        Ok(FieldAccess {
            field_name: id,
        })
    }

    fn module_access(&mut self) -> Result<ModuleAccess, ParseError> {
        self.expect(Token::DoubleColon, "module_access")?;
        let id_token = self.expect(IDENTIFIER, "module_access")?;
        let id = Self::extract_identifier(&id_token).unwrap();
        Ok(ModuleAccess {
            name: id,
        })
    }

    fn object_initialization(&mut self) -> Result<ObjectInitialization, ParseError> {
        self.expect(Token::OpenCurlyBracket, "object_initialization")?;

        let mut fields = HashMap::new();
        loop {
            match self.expect(IDENTIFIER, "object_initialization") {
                Ok(id_token) => {
                    let id = Self::extract_identifier(&id_token).unwrap();
                    self.expect(Token::Colon, "object_initialization")?;
                    let expr = self.expression()?;
                    self.expect(Token::Comma, "object_initialization")?;
                    fields.insert(id, expr);
                },
                Err(_) => break,
            }
        }

        self.expect(Token::CloseCurlyBracket, "object_initialization")?;
        Ok(ObjectInitialization {
            fields,
        })
    }

    fn function_application(&mut self) -> Result<FunctionApplication, ParseError> {
        self.expect(Token::OpenParen, "call")?;
        let mut args = Vec::new();

        match self.tokens[self.current] {
            Token::CloseParen => (),
            _ => {
                let expr = self.expression()?;
                args.push(expr);
                
                loop {
                    match self.tokens[self.current] {
                        Token::Comma => {
                            self.expect(Token::Comma, "call")?;
                            let expr = self.expression()?;
                            args.push(expr);
                        },
                        _ => break
                    }
                }
            },
        }

        self.expect(Token::CloseParen, "call")?;

        Ok(FunctionApplication {
            args,
        })
    }

    fn type_application(&mut self) -> Result<TypeApplication, ParseError> {
        self.expect(Token::OpenDoubleAngleBracket, "type_application")?;
        let mut args = Vec::new();

        match self.tokens[self.current] {
            Token::CloseDoubleAngleBracket => (),
            _ => {
                let expr = self.expression()?;
                args.push(expr);
                
                loop {
                    match self.tokens[self.current] {
                        Token::Comma => {
                            self.expect(Token::Comma, "type_application")?;
                            let expr = self.expression()?;
                            args.push(expr);
                        },
                        _ => break
                    }
                }
            },
        }

        self.expect(Token::CloseDoubleAngleBracket, "type_application")?;

        Ok(TypeApplication {
            args,
        })
    }

    fn conditional_expr(&mut self) -> Result<Conditional, ParseError> {
        let mut if_exprs = vec![self.if_expr()?];
        let mut else_expr = None;

        loop {
            match self.expect(Token::Else, "conditional_expr") {
                Ok(_) => (),
                Err(_) => break,
            }

            match self.tokens[self.current] {
                Token::If => if_exprs.push(self.if_expr()?),
                Token::OpenCurlyBracket => {
                    self.expect(Token::OpenCurlyBracket, "conditional_expr")?;
                    let block_body = self.block_body()?;
                    self.expect(Token::CloseCurlyBracket, "conditional_expr")?;
                    else_expr = Some(block_body);
                    break;
                },
                _ => return Err(ParseError::Message("unable to parse else-if or else epxressions".to_string())),
            }
        }
        
        Ok(Conditional {
            if_exprs,
            else_expr,
        })
    }

    fn if_expr(&mut self) -> Result<If, ParseError> {
        self.expect(Token::If, "if_expr")?;
        let condition = self.expression()?;
        self.expect(Token::OpenCurlyBracket, "if_expr")?;
        let body = self.block_body()?;
        self.expect(Token::CloseCurlyBracket, "if_expr")?;

        Ok(If {
            condition,
            body,
        })
    }

    fn match_expr(&mut self) -> Result<Match, ParseError> {
        self.expect(Token::Match, "match_expr")?;
        let expr = self.expression()?;
        self.expect(Token::OpenCurlyBracket, "match_expr")?;
        let patterns = self.patterns()?;
        self.expect(Token::CloseCurlyBracket, "match_expr")?;

        Ok(Match {
            expr,
            patterns,
        })
    }

    fn patterns(&mut self) -> Result<Vec<Pattern>, ParseError> {
        let mut patterns = Vec::new();

        loop {
            match self.pattern() {
                Ok(pattern) => patterns.push(pattern),
                Err(_) => break,
            }
        }

        Ok(patterns) 
    }

    fn pattern(&mut self) -> Result<Pattern, ParseError> {
        let expr = self.expression()?;
        self.expect(Token::FatArrow, "pattern")?;
        self.expect(Token::OpenCurlyBracket, "pattern")?;
        let body = self.block_body()?;
        self.expect(Token::CloseCurlyBracket, "pattern")?;
        self.expect(Token::Comma, "pattern")?;

        Ok(Pattern {
            to_match: expr,
            body,
        })
    }

    fn loop_expr(&mut self) -> Result<Loop, ParseError> {
        self.expect(Token::For, "loop_expr")?;
        let init = self.variable_assignment()?;
        self.expect(Token::Semicolon, "loop_expr")?;
        let condition = self.expression()?;
        self.expect(Token::Semicolon, "loop_expr")?;
        let step = self.statement()?;
        self.expect(Token::OpenCurlyBracket, "loop_expr")?;
        let body = self.block_body()?;
        self.expect(Token::CloseCurlyBracket, "loop_expr")?;

        Ok(Loop {
            init,
            condition,
            step,
            body,
        })
    }

    fn variable_assignment(&mut self) -> Result<VariableAssignment, ParseError> {
        self.expect(Token::Let, "variable_assignment")?;
        
        let target = self.expression()?;

        self.expect(Token::Colon, "variable_assignment")?;

        let target_type = self.expression()?;

        self.expect(Token::Assignment, "variable_assignment")?;

        let value = self.expression()?;

        Ok(VariableAssignment {
            target,
            target_type,
            value,
        })
    }
}
