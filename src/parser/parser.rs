use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;

use crate::lexer::tokens::*;
use crate::syntax::parse_tree::*;
use crate::utils;

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

const IDENTIFIER: TokenData = TokenData::Identifier(String::new());
const STRING_LIT: TokenData = TokenData::StringLit(String::new());
const CHAR_LIT: TokenData = TokenData::CharLit('y');
const BOOL_LIT: TokenData = TokenData::BooleanLit(true);
const INT_LIT: TokenData = TokenData::IntegerLit(42);
const FLOAT_LIT: TokenData = TokenData::FloatLit(42.0);

#[derive(Debug)]
pub(crate) struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub(crate) fn new() -> Self {
        Self {
            tokens: Vec::new(),
            current: 0,
        }
    }

    fn reset_state(&mut self, tokens: Vec<Token>) {
        self.tokens = tokens;
        self.current = 0;
    }

    pub(crate) fn parse(&mut self, tokens: Vec<Token>) -> Result<Program, ParseError> {
        self.reset_state(tokens);
        self.program()
    }

    fn expect(&mut self, expected_token: TokenData, currently_parsing: &str) -> Result<TokenData, ParseError> {
        let next_token = self.current_token();

        if utils::discriminants_equal(&expected_token, &next_token) {
            self.current = self.current + 1;
            return Ok(next_token);
        }

        Err(ParseError::Message(format!("expected {} while parsing {} but got {} at {}", expected_token, currently_parsing, next_token, self.current)))
    }

    fn current_token(&self) -> TokenData {
        let token = self.tokens.get(self.current).unwrap();
        token.data.clone()
    }

    fn extract_identifier(t: &TokenData) -> Option<String> {
        // TODO -> maybe get rid of clone
        return match t {
            TokenData::Identifier(id) => Some(id.clone()),
            _ => None
        }
    }

    fn program(&mut self) -> Result<Program, ParseError> {
        let mut declarations = Vec::new();
        loop {
            if let TokenData::EOF = self.current_token() {
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
            TokenData::Type => {
                let message = format!("unexpected reserved type token");
                Err(ParseError::Message(message))
            },
            TokenData::Module => {
                let obj_decl = self.module_declaration()?;
                Ok(Declaration::ModuleDeclarationNode(obj_decl))
            },
            TokenData::Enum => {
                let enum_decl = self.enum_declaration()?;
                Ok(Declaration::EnumDeclarationNode(enum_decl))
            },
            TokenData::Contract => {
                let contract_decl = self.contract_declaration()?;
                Ok(Declaration::ContractDeclarationNode(contract_decl))
            },
            TokenData::Implement => {
                let impl_decl = self.implementation_declaration()?;
                Ok(Declaration::ImplementationDeclarationNode(impl_decl))
            },
            TokenData::Object => {
                let obj_decl = self.object_declaration()?;
                Ok(Declaration::ObjectDeclarationNode(obj_decl))
            },
            TokenData::Func => {
                let func_decl = self.function_declaration()?;
                Ok(Declaration::FunctionDeclarationNode(func_decl))
            },
            TokenData::Use => {
                let use_decl = self.use_declaration()?;
                Ok(Declaration::UseDeclarationNode(use_decl))
            },
            t => {
                let message = format!("Unable to parse any type of declaration at token: {}, pos: {}", t, self.current);
                Err(ParseError::Message(message))
            }
        };
    }

    fn generic_params(&mut self) -> Result<Vec<String>, ParseError> {
        self.expect(TokenData::OpenDoubleAngleBracket, "generic_params")?;
       
        let mut params = Vec::new();

        match self.current_token() {
            TokenData::Identifier(id) => {
                self.expect(IDENTIFIER, "generic_params")?;
                params.push(id);

                loop {
                    match self.current_token() {
                        TokenData::CloseDoubleAngleBracket => {
                            self.expect(TokenData::CloseDoubleAngleBracket, "generic_params")?;
                            break;
                        },
                        TokenData::Comma => {
                            self.expect(TokenData::Comma, "generic_params")?;
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
                self.expect(TokenData::CloseDoubleAngleBracket, "generic_params")?;
            }
        }

        Ok(params)
    }

    fn generic_args(&mut self) -> Result<Vec<Expression>, ParseError> {
        self.expect(TokenData::OpenDoubleAngleBracket, "generic_args")?;
       
        let mut args = Vec::new();

        match self.current_token() {
            TokenData::CloseDoubleAngleBracket => {
                    self.expect(TokenData::CloseDoubleAngleBracket, "generic_args")?;
            },
            _ => {
                let expr = self.expression()?;
                args.push(expr);
                loop {
                    match self.current_token() {
                        TokenData::CloseDoubleAngleBracket => {
                            self.expect(TokenData::CloseDoubleAngleBracket, "generic_args")?;
                            break;
                        },
                        TokenData::Comma => {
                            self.expect(TokenData::Comma, "generic_args")?;
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

    fn module_declaration(&mut self) -> Result<ModuleDeclaration, ParseError> {
        self.expect(TokenData::Module, "module_declaration")?;
        let mod_name_token = self.expect(IDENTIFIER, "enum_declaration")?;
        let mod_name = Self::extract_identifier(&mod_name_token).unwrap();
        
        self.expect(TokenData::OpenCurlyBracket, "module_declaration")?;

        let mut functions = Vec::new();

        loop {
            match self.function_declaration() {
                Ok(func_decl) => functions.push(func_decl),
                Err(_) => break
            }
        }

        self.expect(TokenData::CloseCurlyBracket, "module_declaration")?;

        Ok(ModuleDeclaration {
            mod_name,
            functions,
        })
    }

    fn enum_declaration(&mut self) -> Result<EnumDeclaration, ParseError> {
        self.expect(TokenData::Enum, "enum_declaration")?;
        let type_name_token = self.expect(IDENTIFIER, "enum_declaration")?;
        let type_name = Self::extract_identifier(&type_name_token).unwrap();

        let type_params = match self.current_token() {
            TokenData::OpenDoubleAngleBracket => {
                let generic_params = self.generic_params()?;
                generic_params
            },
            _ => Vec::new(),
        };
        
        self.expect(TokenData::OpenCurlyBracket, "enum_declaration")?;
        let variants = self.variants()?;

        let methods = match self.current_token() {
            TokenData::Methods => self.methods()?,
            _ => Vec::new(),
        };

        self.expect(TokenData::CloseCurlyBracket, "enum_declaration")?;

        Ok(EnumDeclaration {
            type_name,
            type_params,
            variants,
            methods,
        })
    }

    fn object_declaration(&mut self) -> Result<ObjectDeclaration, ParseError> {
        self.expect(TokenData::Object, "object_declaration")?;
        let type_name_token = self.expect(IDENTIFIER, "object_declaration")?;
        let type_name = Self::extract_identifier(&type_name_token).unwrap();

        let type_params = match self.current_token() {
            TokenData::OpenDoubleAngleBracket => {
                let generic_params = self.generic_params()?;
                generic_params
            },
            _ => Vec::new(),
        };

        self.expect(TokenData::OpenCurlyBracket, "object_declaration")?;
        let fields = self.fields()?;
        
        let methods = match self.current_token() {
            TokenData::Methods => self.methods()?,
            _ => Vec::new(),
        };

        self.expect(TokenData::CloseCurlyBracket, "object_declaration")?;

        Ok(ObjectDeclaration {
            type_name,
            type_params,
            fields,
            methods,
        })
    }

    fn contract_declaration(&mut self) -> Result<ContractDeclaration, ParseError> {
        self.expect(TokenData::Contract, "contract_declaration")?;
        let type_name_token = self.expect(IDENTIFIER, "contract_declaration")?;
        let type_name = Self::extract_identifier(&type_name_token).unwrap();

        let type_params = match self.current_token() {
            TokenData::OpenDoubleAngleBracket => {
                let generic_params = self.generic_params()?;
                generic_params
            },
            _ => Vec::new(),
        };

        self.expect(TokenData::OpenCurlyBracket, "contract_declaration")?;

        let mut functions = Vec::new();

        loop {
            match self.function_signature() {
                Ok(func) => functions.push(func),
                Err(_) => break
            }
            self.expect(TokenData::Semicolon, "contract_declaration")?;
        }
        
        self.expect(TokenData::CloseCurlyBracket, "contract_declaration")?;

        Ok(ContractDeclaration {
            type_name,
            type_params, 
            functions,
        })
    }

    fn implementation_declaration(&mut self) -> Result<ImplementationDeclaration, ParseError> {
        self.expect(TokenData::Implement, "implementation_declaration")?;
        let contract = self.expression()?;

        self.expect(TokenData::For, "implementation_declaration")?;
        let implementing_type = self.expression()?;

        self.expect(TokenData::OpenCurlyBracket, "implementation_declaration")?;

        let mut functions = Vec::new();

        loop {
            match self.function_declaration() {
                Ok(func_decl) => functions.push(func_decl),
                Err(_) => break
            }
        }

        self.expect(TokenData::CloseCurlyBracket, "implementation_declaration")?;

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
        self.expect(TokenData::OpenCurlyBracket, "function_declaration")?;
        let body = self.block_body()?;
        self.expect(TokenData::CloseCurlyBracket, "function_declaration")?;
       
        Ok(FunctionDeclaration {
            visibility,
            signature,
            body,
        })
    }

    fn use_declaration(&mut self) -> Result<UseDeclaration, ParseError> {
        self.expect(TokenData::Use, "use_declaration")?;
      
        let name_token = self.expect(IDENTIFIER, "use_declaration")?;
        let name = Self::extract_identifier(&name_token).unwrap();
        let mut import_chain = vec![name];

        loop {
            match self.expect(TokenData::DoubleColon, "use_declaration") {
                Ok(_) => {
                    let name_token = self.expect(IDENTIFIER, "use_declaration")?;
                    let name = Self::extract_identifier(&name_token).unwrap();
                    import_chain.push(name);
                },
                Err(_) => break
            }
        }

        self.expect(TokenData::Semicolon, "use_declaration")?;

        Ok(UseDeclaration {
            import_chain,
        })
    }

    fn visibility(&mut self) -> Result<Visibility, ParseError> {
        return match self.current_token() {
            TokenData::Public => {
                self.expect(TokenData::Public, "visibility")?;
                Ok(Visibility::Public)
            },
            _ => {
                let message = format!("unable to parse visibility at {}", self.current_token());
                Err(ParseError::Message(message))
            }
        }
    }

    fn function_signature(&mut self) -> Result<FunctionSignature, ParseError> {
        self.expect(TokenData::Func, "function_signature")?;
        let name_token = self.expect(IDENTIFIER, "function_signature")?;
        let name = Self::extract_identifier(&name_token).unwrap();

        let type_parameters = match self.current_token() {
            TokenData::OpenDoubleAngleBracket => {
                let generic_params = self.generic_params()?;
                generic_params
            },
            _ => Vec::new(),
        };

        self.expect(TokenData::OpenParen, "function_signature")?;

        let mut parameters = Vec::new();

        match self.current_token() {
            TokenData::CloseParen => (),
            _ => {
                let param = self.function_parameter()?;
                parameters.push(param);
                loop {
                    match self.current_token() {
                        TokenData::Comma => {
                            self.expect(TokenData::Comma, "function_signature")?;
                            let param = self.function_parameter()?;
                            parameters.push(param);
                        },
                        _ => break
                    }
                }
            },
        }

        self.expect(TokenData::CloseParen, "function_signature")?;

        let returns = match self.current_token() {
            TokenData::Arrow => {
                self.expect(TokenData::Arrow, "function_signature")?;
                let expr = self.expression()?;
                self.expect(TokenData::Semicolon, "function_signature")?;
                Some(expr)
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
            TokenData::SelfVariable => {
                self.expect(TokenData::SelfVariable, "function_parameter")?;
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
        self.expect(TokenData::Colon, "typed_variable_declaration")?;
        let type_reference = self.expression()?;

        Ok(TypedVariableDeclaration {
            name,
            type_reference,
        })
    }

    fn variants(&mut self) -> Result<Vec<VariantDeclaration>, ParseError> {
        self.expect(TokenData::Variants, "variants")?;
        self.expect(TokenData::OpenCurlyBracket, "variants")?;
        
        let mut variants = Vec::new();

        loop {
            match self.variant_declaration() {
                Ok(variant) => variants.push(variant),
                Err(_) => break,
            }
        }
        
        self.expect(TokenData::CloseCurlyBracket, "variants")?;

        Ok(variants)
    }

    fn variant_declaration(&mut self) -> Result<VariantDeclaration, ParseError> {
        let name_token = self.expect(IDENTIFIER, "variant_declaration")?;
        let name = Self::extract_identifier(&name_token).unwrap();
        
        let mut contains = None;

        match self.expect(TokenData::OpenParen, "variant_declaration") {
            Ok(_) => {
                let type_reference = self.expression()?;
                self.expect(TokenData::CloseParen, "variant_declaration")?;
                contains = Some(type_reference)
            },
            Err(_) => (),
        }

        self.expect(TokenData::Comma, "variant_declaration")?;

        Ok(VariantDeclaration {
            name,
            contains,
        })
    }

    fn fields(&mut self) -> Result<Vec<TypedVariableDeclaration>, ParseError> {
        self.expect(TokenData::Fields, "fields")?;
        self.expect(TokenData::OpenCurlyBracket, "fields")?;
        
        let mut fields = Vec::new();

        loop {
            match self.typed_variable_declaration() {
                Ok(tvd) => fields.push(tvd),
                Err(_) => break
            }

            self.expect(TokenData::Comma, "fields")?;
        }

        self.expect(TokenData::CloseCurlyBracket, "fields")?;

        Ok(fields)
    }

    fn methods(&mut self) -> Result<Vec<FunctionDeclaration>, ParseError> {
        self.expect(TokenData::Methods, "methods")?;
        self.expect(TokenData::OpenCurlyBracket, "methods")?;
        
        let mut methods = Vec::new();

        loop {
            match self.function_declaration() {
                Ok(method) => methods.push(method),
                Err(e) => break
            }
        }
    
        self.expect(TokenData::CloseCurlyBracket, "methods")?;

        Ok(methods)
    }

    fn functions(&mut self) -> Result<Vec<FunctionDeclaration>, ParseError> {
        self.expect(TokenData::Functions, "functions")?;
        self.expect(TokenData::OpenCurlyBracket, "functions")?;
        
        let mut functions = Vec::new();

        loop {
            match self.function_declaration() {
                Ok(function) => functions.push(function),
                Err(e) => break
            }
        }
    
        self.expect(TokenData::CloseCurlyBracket, "functions")?;

        Ok(functions)
    }

    fn block_body(&mut self) -> Result<BlockBody, ParseError> {
        let mut statements = Vec::new();

        loop {
            if let TokenData::CloseCurlyBracket = self.current_token() {
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
            TokenData::Let => {
                let assignment = self.variable_assignment()?;
                self.expect(TokenData::Semicolon, "statement")?;
                Ok(Statement::VariableAssignmentNode(assignment))
            },
            TokenData::Return => {
                let return_stmt = self.return_stmt()?;
                self.expect(TokenData::Semicolon, "statement")?;
                Ok(Statement::ReturnNode(return_stmt))        
            },
            _ => {
                let expr = self.expression()?;
                self.expect(TokenData::Semicolon, "statement")?;
                Ok(Statement::ExpressionNode(expr))
            }
        }
    }

    fn return_stmt(&mut self) -> Result<Return, ParseError> {
        self.expect(TokenData::Return, "statement")?;
        let value = self.expression()?;

        Ok(Return {
            value,
        })
    }
 
    fn expression(&mut self) -> Result<Expression, ParseError> {
        return match self.current_token() {
            TokenData::LogicalNot => {
                self.expect(TokenData::LogicalNot, "expression")?;
                let expr = self.chainable_expression()?;
                let op = UnaryOperation {
                    op: UnaryOperator::Not,
                    expr: expr,
                };
                Ok(Expression::UnaryOperationNode(Box::new(op)))
            },
            TokenData::Pipe => {
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
        self.expect(TokenData::Pipe, "lambda")?;
       
        let mut params = Vec::new();
        match self.current_token() {
            TokenData::Identifier(id) => {
                let param = self.typed_variable_declaration()?;
                params.push(param);

                loop {
                    match self.current_token() {
                        TokenData::Comma => {
                            self.expect(TokenData::Comma, "lambda")?;
                            let param = self.typed_variable_declaration()?;
                            params.push(param);
                        },
                        _ => break
                    }
                }
            },
            _ => ()
        }

        self.expect(TokenData::Pipe, "lambda")?;
        self.expect(TokenData::OpenCurlyBracket, "lambda")?;
        let body = self.block_body()?;
        self.expect(TokenData::CloseCurlyBracket, "lambda")?;

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
                TokenData::Dot => {
                    let current = self.current;
                    if let Ok(field_access) = self.field_access() {
                        chained.push(ExpressionChain::FieldAccessNode(field_access));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                TokenData::DoubleColon => {
                    let current = self.current;
                    if let Ok(mod_access) = self.module_access() {
                        chained.push(ExpressionChain::ModuleAccessNode(mod_access));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                TokenData::OpenSquareBracket => {
                    let current = self.current;
                    if let Ok(arr_access) = self.array_access() {
                        chained.push(ExpressionChain::ArrayAccessNode(arr_access));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                TokenData::OpenCurlyBracket => {
                    let current = self.current;
                    if let Ok(obj_init) = self.object_initialization() {
                        chained.push(ExpressionChain::ObjectInitializationNode(obj_init));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                TokenData::OpenParen => {
                    let current = self.current;
                    if let Ok(func_app) = self.function_application() {
                        chained.push(ExpressionChain::FunctionApplicationNode(func_app));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                TokenData::OpenDoubleAngleBracket => {
                    let current = self.current;
                    if let Ok(type_application) = self.type_application() {
                        chained.push(ExpressionChain::TypeApplicationNode(type_application));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                TokenData::Plus => {
                    self.expect(TokenData::Plus, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Plus,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::Minus => {
                    self.expect(TokenData::Minus, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Minus,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::Multiply => {
                    self.expect(TokenData::Multiply, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Multiply,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::Divide => {
                    self.expect(TokenData::Divide, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Divide,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::LessThan => {
                    self.expect(TokenData::LessThan, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::LessThan,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::LessThanOrEqual => {
                    self.expect(TokenData::LessThanOrEqual, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::LessThanOrEqual,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::GreaterThan => {
                    self.expect(TokenData::GreaterThan, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::GreaterThan,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::GreaterThanOrEqual => {
                    self.expect(TokenData::GreaterThanOrEqual, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::GreaterThanOrEqual,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::EqualTo => {
                    self.expect(TokenData::EqualTo, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::Equal,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::NotEqualTo => {
                    self.expect(TokenData::NotEqualTo, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::NotEqual,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::LogicalAnd => {
                    self.expect(TokenData::LogicalAnd, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let bin_op = BinaryOperation {
                        op: BinaryOperator::And,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::LogicalOr => {
                    self.expect(TokenData::LogicalOr, "chainable_expr")?;
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
            TokenData::If => {
                let cond_expr = self.conditional_expr()?;
                Ok(ExpressionStart::ConditionalNode(cond_expr))
            },
            TokenData::Match => {
                let match_expr = self.match_expr()?;
                Ok(ExpressionStart::MatchNode(match_expr))
            },
            TokenData::For => {
                let loop_expr = self.loop_expr()?;
                Ok(ExpressionStart::LoopNode(loop_expr))
            },
            // TODO -> there is another case that isn't covered which is array initialization
            // right now this is defined as `[<ty>; <length]`
            // given the difficulty of parsing this, it might be better to come up with a different
            // syntax for this
            TokenData::OpenSquareBracket => {
                let current = self.current;

                if let Ok(arr_ty) = self.array_type_expr() {
                    return Ok(ExpressionStart::ArrayType(arr_ty))
                } else {
                    self.current = current;
                }

                let arr_init = self.array_init_expr()?;
                Ok(ExpressionStart::ArrayInitialization(arr_init))
            },
            TokenData::SelfVariable => {
                self.expect(TokenData::SelfVariable, "expression_start")?;
                Ok(ExpressionStart::VariableNode(Variable::SelfVariable))
            },
            TokenData::SelfType => {
                self.expect(TokenData::SelfType, "expression_start")?;
                Ok(ExpressionStart::VariableNode(Variable::SelfType))
            },
            TokenData::Identifier(id) => {
                self.expect(IDENTIFIER, "expression_start")?;
                Ok(ExpressionStart::VariableNode(Variable::Name(id)))
            },
            TokenData::StringLit(s) => {
                self.expect(STRING_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::StringValue(s)))
            },
            TokenData::CharLit(c) => {
                self.expect(CHAR_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::CharValue(c)))
            },
            TokenData::BooleanLit(b) => {
                self.expect(BOOL_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::BooleanValue(b)))
            },
            TokenData::IntegerLit(i) => {
                self.expect(INT_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::IntegerValue(i)))
            },
            TokenData::FloatLit(f) => {
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
        self.expect(TokenData::Dot, "field_access")?;
        let id_token = self.expect(IDENTIFIER, "field_access")?;
        let id = Self::extract_identifier(&id_token).unwrap();
        Ok(FieldAccess {
            field_name: id,
        })
    }

    fn module_access(&mut self) -> Result<ModuleAccess, ParseError> {
        self.expect(TokenData::DoubleColon, "module_access")?;
        let id_token = self.expect(IDENTIFIER, "module_access")?;
        let id = Self::extract_identifier(&id_token).unwrap();
        Ok(ModuleAccess {
            name: id,
        })
    }

    fn array_access(&mut self) -> Result<ArrayAccess, ParseError> {
        self.expect(TokenData::OpenSquareBracket, "array_access")?;
        let index_expr = self.expression()?;
        self.expect(TokenData::CloseSquareBracket, "array_access")?;

        Ok(ArrayAccess {
            index_expr,
        })
    }

    fn object_initialization(&mut self) -> Result<ObjectInitialization, ParseError> {
        self.expect(TokenData::OpenCurlyBracket, "object_initialization")?;

        let mut fields = HashMap::new();
        loop {
            match self.expect(IDENTIFIER, "object_initialization") {
                Ok(id_token) => {
                    let id = Self::extract_identifier(&id_token).unwrap();
                    self.expect(TokenData::Colon, "object_initialization")?;
                    let expr = self.expression()?;
                    self.expect(TokenData::Comma, "object_initialization")?;
                    fields.insert(id, expr);
                },
                Err(_) => break,
            }
        }

        self.expect(TokenData::CloseCurlyBracket, "object_initialization")?;
        Ok(ObjectInitialization {
            fields,
        })
    }

    fn function_application(&mut self) -> Result<FunctionApplication, ParseError> {
        self.expect(TokenData::OpenParen, "call")?;
        let mut args = Vec::new();

        match self.current_token() {
            TokenData::CloseParen => (),
            _ => {
                let expr = self.expression()?;
                args.push(expr);
                
                loop {
                    match self.current_token() {
                        TokenData::Comma => {
                            self.expect(TokenData::Comma, "call")?;
                            let expr = self.expression()?;
                            args.push(expr);
                        },
                        _ => break
                    }
                }
            },
        }

        self.expect(TokenData::CloseParen, "call")?;

        Ok(FunctionApplication {
            args,
        })
    }

    fn type_application(&mut self) -> Result<TypeApplication, ParseError> {
        self.expect(TokenData::OpenDoubleAngleBracket, "type_application")?;
        let mut args = Vec::new();

        match self.current_token() {
            TokenData::CloseDoubleAngleBracket => (),
            _ => {
                let expr = self.expression()?;
                args.push(expr);
                
                loop {
                    match self.current_token() {
                        TokenData::Comma => {
                            self.expect(TokenData::Comma, "type_application")?;
                            let expr = self.expression()?;
                            args.push(expr);
                        },
                        _ => break
                    }
                }
            },
        }

        self.expect(TokenData::CloseDoubleAngleBracket, "type_application")?;

        Ok(TypeApplication {
            args,
        })
    }

    fn conditional_expr(&mut self) -> Result<Conditional, ParseError> {
        let mut if_exprs = vec![self.if_expr()?];
        let mut else_expr = None;

        loop {
            match self.expect(TokenData::Else, "conditional_expr") {
                Ok(_) => (),
                Err(_) => break,
            }

            match self.current_token() {
                TokenData::If => if_exprs.push(self.if_expr()?),
                TokenData::OpenCurlyBracket => {
                    self.expect(TokenData::OpenCurlyBracket, "conditional_expr")?;
                    let block_body = self.block_body()?;
                    self.expect(TokenData::CloseCurlyBracket, "conditional_expr")?;
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
        self.expect(TokenData::If, "if_expr")?;
        let condition = self.expression()?;
        self.expect(TokenData::OpenCurlyBracket, "if_expr")?;
        let body = self.block_body()?;
        self.expect(TokenData::CloseCurlyBracket, "if_expr")?;

        Ok(If {
            condition,
            body,
        })
    }

    fn match_expr(&mut self) -> Result<Match, ParseError> {
        self.expect(TokenData::Match, "match_expr")?;
        let expr = self.expression()?;
        self.expect(TokenData::OpenCurlyBracket, "match_expr")?;
        let patterns = self.patterns()?;
        self.expect(TokenData::CloseCurlyBracket, "match_expr")?;

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
        self.expect(TokenData::FatArrow, "pattern")?;
        self.expect(TokenData::OpenCurlyBracket, "pattern")?;
        let body = self.block_body()?;
        self.expect(TokenData::CloseCurlyBracket, "pattern")?;
        self.expect(TokenData::Comma, "pattern")?;

        Ok(Pattern {
            to_match: expr,
            body,
        })
    }

    fn loop_expr(&mut self) -> Result<Loop, ParseError> {
        self.expect(TokenData::For, "loop_expr")?;
        let init = self.variable_assignment()?;
        self.expect(TokenData::Semicolon, "loop_expr")?;
        let condition = self.expression()?;
        self.expect(TokenData::Semicolon, "loop_expr")?;
        let step = self.statement()?;
        self.expect(TokenData::OpenCurlyBracket, "loop_expr")?;
        let body = self.block_body()?;
        self.expect(TokenData::CloseCurlyBracket, "loop_expr")?;

        Ok(Loop {
            init,
            condition,
            step,
            body,
        })
    }

    fn array_type_expr(&mut self) -> Result<ArrayType, ParseError> {
        self.expect(TokenData::OpenSquareBracket, "array_type_expr")?;
        let ty = self.expression()?;
        self.expect(TokenData::CloseSquareBracket, "array_type_expr")?;

        Ok(ArrayType {
            ty,
        })
    }

    fn array_init_expr(&mut self) -> Result<ArrayInitialization, ParseError> {
        self.expect(TokenData::OpenSquareBracket, "array_init_expr")?;
        let ty = self.expression()?;
        self.expect(TokenData::Semicolon, "array_init_expr")?;
        let size = self.expression()?;
        self.expect(TokenData::CloseSquareBracket, "array_init_expr")?;

        Ok(ArrayInitialization {
            ty,
            size,
        })
    }

    fn variable_assignment(&mut self) -> Result<VariableAssignment, ParseError> {
        self.expect(TokenData::Let, "variable_assignment")?;
        
        let target = self.expression()?;

        self.expect(TokenData::Colon, "variable_assignment")?;

        let target_type = self.expression()?;

        self.expect(TokenData::Assignment, "variable_assignment")?;

        let value = self.expression()?;

        Ok(VariableAssignment {
            target,
            target_type,
            value,
        })
    }
}
