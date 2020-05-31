use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;

use crate::lexer::tokens::*;
use crate::syntax::parse_tree::*;
use crate::utils;
use crate::utils::LocationContext;

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

    pub(crate) fn parse(&mut self, tokens: Vec<Token>) -> Result<Program, ParseError> {
        self.reset_state(tokens);
        self.program()
    }

    fn reset_state(&mut self, tokens: Vec<Token>) {
        self.tokens = tokens;
        self.current = 0;
    }

    fn expect(&mut self, expected_token: TokenData, currently_parsing: &str) -> Result<Token, ParseError> {
        let next_token = self.tokens.get(self.current).unwrap();

        if utils::discriminants_equal(&expected_token, &next_token.data) {
            self.current = self.current + 1;
            return Ok(next_token.clone());
        }

        Err(ParseError::Message(format!("expected {} while parsing {} but got {} at {:?}",
                                        expected_token,
                                        currently_parsing,
                                        next_token.data,
                                        next_token.location)))
    }

    fn current_token_data(&self) -> TokenData {
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
            if let TokenData::EOF = self.current_token_data() {
                break
            }

            match self.declaration() {
                Ok(decl) => declarations.push(decl),
                Err(e) => return Err(e),
            }
        }

        let location = if declarations.is_empty() {
            self.tokens[self.current].location.clone()
        } else {
            LocationContext::merge(&declarations.first().unwrap().location(), &declarations.last().unwrap().location())
        };

        Ok(Program {
            location,
            declarations,
        })
    }

    fn type_identifier(&mut self) -> Result<TypeIdentifier, ParseError> {
        match self.current_token_data() {
            TokenData::OpenSquareBracket => {
                let array_type_node = self.array_type_expr()?;
                Ok(TypeIdentifier::ArrayTypeNode(Box::new(array_type_node)))
            },
            _ => {
                let generic_type_node = self.generic_type()?;
                Ok(TypeIdentifier::GenericTypeNode(generic_type_node))
            }
        }
    }

    fn generic_type(&mut self) -> Result<GenericType, ParseError> {
        let name_token = self.expect(IDENTIFIER, "generic_type")?;
        let name = Self::extract_identifier(&name_token.data).unwrap();

        let type_application = if let Ok(type_app) = self.type_application() {
            Some(type_app)
        } else {
            None
        };

        let location = if let Some(end_token) = &type_application {
            LocationContext::merge(&name_token.location, &end_token.location)
        } else {
            LocationContext::merge(&name_token.location, &name_token.location)
        };

        Ok(GenericType {
            location,
            name,
            type_application
        })
    }

    fn generic_name(&mut self) -> Result<GenericName, ParseError> {
        let name_token = self.expect(IDENTIFIER, "generic_name")?;
        let name = Self::extract_identifier(&name_token.data).unwrap();
        
        let mut type_parameters = Vec::new();
        let mut end_token = None;

        match self.current_token_data() {
            TokenData::AtSign => {
                self.expect(TokenData::AtSign, "generic_name")?;
                self.expect(TokenData::OpenParen, "generic_name")?;
               
                let param_token = self.expect(IDENTIFIER, "generic_name")?;
                let param_name = Self::extract_identifier(&param_token.data).unwrap(); 
                end_token = Some(param_token);

                type_parameters.push(param_name);

                loop {
                    match self.current_token_data() {
                        TokenData::Comma => {
                            self.expect(TokenData::Comma, "generic_name")?;
                            let param_token = self.expect(IDENTIFIER, "generic_name")?;
                            let param_name = Self::extract_identifier(&param_token.data).unwrap(); 
                            type_parameters.push(param_name);
                        },
                    }
                }
            },
            _ => (),
        }

        let location = if let Some(end_token) = end_token {
            LocationContext::merge(&name_token.location, &end_token.location)
        } else {
            LocationContext::merge(&name_token.location, &name_token.location)
        };

        Ok(GenericName {
            location,
            name,
            type_parameters
        })
    }

    fn declaration(&mut self) -> Result<Declaration, ParseError> {
        match self.current_token_data() {
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
        }
    }

    fn module_declaration(&mut self) -> Result<ModuleDeclaration, ParseError> {
        let start_token = self.expect(TokenData::Module, "module_declaration")?;
        let mod_name_token = self.expect(IDENTIFIER, "enum_declaration")?;
        let mod_name = Self::extract_identifier(&mod_name_token.data).unwrap();
        
        self.expect(TokenData::OpenCurlyBracket, "module_declaration")?;

        let mut functions = Vec::new();

        loop {
            match self.function_declaration() {
                Ok(func_decl) => functions.push(func_decl),
                Err(_) => break
            }
        }

        let end_token = self.expect(TokenData::CloseCurlyBracket, "module_declaration")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(ModuleDeclaration {
            location,
            mod_name,
            functions,
        })
    }

    fn enum_declaration(&mut self) -> Result<EnumDeclaration, ParseError> {
        let start_token = self.expect(TokenData::Enum, "enum_declaration")?;
        let name = self.generic_name()?;
        self.expect(TokenData::OpenCurlyBracket, "enum_declaration")?;
        let variants = self.variants()?;

        let methods = match self.current_token_data() {
            TokenData::Methods => self.methods()?,
            _ => Vec::new(),
        };

        let end_token = self.expect(TokenData::CloseCurlyBracket, "enum_declaration")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(EnumDeclaration {
            location,
            name,
            variants,
            methods,
        })
    }

    fn object_declaration(&mut self) -> Result<ObjectDeclaration, ParseError> {
        let start_token = self.expect(TokenData::Object, "object_declaration")?;
        let name = self.generic_name()?;
        self.expect(TokenData::OpenCurlyBracket, "object_declaration")?;
        let fields = self.fields()?;
        
        let methods = match self.current_token_data() {
            TokenData::Methods => self.methods()?,
            _ => Vec::new(),
        };

        let end_token = self.expect(TokenData::CloseCurlyBracket, "object_declaration")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(ObjectDeclaration {
            location,
            name,
            fields,
            methods,
        })
    }

    fn contract_declaration(&mut self) -> Result<ContractDeclaration, ParseError> {
        let start_token = self.expect(TokenData::Contract, "contract_declaration")?;
        let name = self.generic_name()?;
        self.expect(TokenData::OpenCurlyBracket, "contract_declaration")?;

        let mut functions = Vec::new();

        loop {
            match self.function_signature() {
                Ok(func) => functions.push(func),
                Err(_) => break
            }
            self.expect(TokenData::Semicolon, "contract_declaration")?;
        }
        
        let end_token = self.expect(TokenData::CloseCurlyBracket, "contract_declaration")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(ContractDeclaration {
            location,
            name,
            functions,
        })
    }

    fn implementation_declaration(&mut self) -> Result<ImplementationDeclaration, ParseError> {
        let start_token = self.expect(TokenData::Implement, "implementation_declaration")?;
        let contract = self.generic_name()?;

        self.expect(TokenData::For, "implementation_declaration")?;
        let implementing_type = self.generic_name()?;

        self.expect(TokenData::OpenCurlyBracket, "implementation_declaration")?;

        let mut functions = Vec::new();

        loop {
            match self.function_declaration() {
                Ok(func_decl) => functions.push(func_decl),
                Err(_) => break
            }
        }

        let end_token = self.expect(TokenData::CloseCurlyBracket, "implementation_declaration")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(ImplementationDeclaration {
            location,
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
        let end_token = self.expect(TokenData::CloseCurlyBracket, "function_declaration")?;

        let location = LocationContext::merge(&signature.location, &end_token.location);

        Ok(FunctionDeclaration {
            location,
            visibility,
            signature,
            body,
        })
    }

    fn use_declaration(&mut self) -> Result<UseDeclaration, ParseError> {
        let start_token = self.expect(TokenData::Use, "use_declaration")?;
      
        let name_token = self.expect(IDENTIFIER, "use_declaration")?;
        let name = Self::extract_identifier(&name_token.data).unwrap();
        let mut import_chain = vec![name];

        loop {
            match self.expect(TokenData::DoubleColon, "use_declaration") {
                Ok(_) => {
                    let name_token = self.expect(IDENTIFIER, "use_declaration")?;
                    let name = Self::extract_identifier(&name_token.data).unwrap();
                    import_chain.push(name);
                },
                Err(_) => break
            }
        }

        let end_token = self.expect(TokenData::Semicolon, "use_declaration")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(UseDeclaration {
            location,
            import_chain,
        })
    }

    fn visibility(&mut self) -> Result<Visibility, ParseError> {
        return match self.current_token_data() {
            TokenData::Public => {
                self.expect(TokenData::Public, "visibility")?;
                Ok(Visibility::Public)
            },
            t => {
                let message = format!("unable to parse visibility at {}", t);
                Err(ParseError::Message(message))
            }
        }
    }

    fn function_signature(&mut self) -> Result<FunctionSignature, ParseError> {
        let start_token = self.expect(TokenData::Func, "function_signature")?;
        let name = self.generic_name()?;
        self.expect(TokenData::OpenParen, "function_signature")?;

        let mut parameters = Vec::new();

        match self.current_token_data() {
            TokenData::CloseParen => (),
            _ => {
                let param = self.function_parameter()?;
                parameters.push(param);
                loop {
                    match self.current_token_data() {
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

        let end_token = self.expect(TokenData::CloseParen, "function_signature")?;

        let returns = match self.current_token_data() {
            TokenData::Arrow => {
                self.expect(TokenData::Arrow, "function_signature")?;
                Some(self.type_identifier()?)
            },
            _ => None
        };

        let location = if let Some(r) = &returns {
            LocationContext::merge(&start_token.location, &r.location())
        } else {
            LocationContext::merge(&start_token.location, &end_token.location)
        };

        Ok(FunctionSignature {
            location,
            name,
            parameters,
            returns,
        })
    }

    fn function_parameter(&mut self) -> Result<FunctionParameter, ParseError> {
        return match self.current_token_data() {
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
        let name = Self::extract_identifier(&name_token.data).unwrap();
        self.expect(TokenData::Colon, "typed_variable_declaration")?;
        let type_identifier = self.type_identifier()?;

        let location = LocationContext::merge(&name_token.location, &type_identifier.location());

        Ok(TypedVariableDeclaration {
            location,
            name,
            type_identifier,
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
        let name = Self::extract_identifier(&name_token.data).unwrap();
        
        let mut contains = None;

        match self.expect(TokenData::OpenParen, "variant_declaration") {
            Ok(_) => {
                let type_reference = self.type_identifier()?;
                self.expect(TokenData::CloseParen, "variant_declaration")?;
                contains = Some(type_reference)
            },
            Err(_) => (),
        }

        let end_token = self.expect(TokenData::Comma, "variant_declaration")?;

        let location = LocationContext::merge(&name_token.location, &end_token.location);

        Ok(VariantDeclaration {
            location,
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
            if let TokenData::CloseCurlyBracket = self.current_token_data() {
                break;
            }

            let stmt = self.statement()?;
            statements.push(stmt);
        }

        let location = if statements.is_empty() {
            self.tokens[self.current].location.clone()
        } else {
            LocationContext::merge(&statements.first().unwrap().location(), &statements.last().unwrap().location())
        };

        Ok(BlockBody {
            location,
            statements,
        })
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        return match self.current_token_data() {
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
        let start_token = self.expect(TokenData::Return, "statement")?;
        let value = self.expression()?;

        let location = LocationContext::merge(&start_token.location, &value.location());

        Ok(Return {
            location,
            value,
        })
    }
 
    fn expression(&mut self) -> Result<Expression, ParseError> {
        return match self.current_token_data() {
            TokenData::LogicalNot => {
                let start_token = self.expect(TokenData::LogicalNot, "expression")?;
                let expr = self.chainable_expression()?;
                let location = LocationContext::merge(&start_token.location, &expr.location);
                let op = UnaryOperation {
                    location,
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
        let start_token = self.expect(TokenData::Pipe, "lambda")?;
       
        let mut params = Vec::new();
        match self.current_token_data() {
            TokenData::Identifier(id) => {
                let param = self.typed_variable_declaration()?;
                params.push(param);

                loop {
                    match self.current_token_data() {
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
        let end_token = self.expect(TokenData::CloseCurlyBracket, "lambda")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(Lambda {
            location,
            params,
            body,
        })
    }

    fn chainable_expression(&mut self) -> Result<ChainableExpression, ParseError> {
        let start = self.expression_start()?;

        let mut chained = Vec::new();
        loop {
            match self.current_token_data() {
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
                TokenData::AtSign => {
                    let current = self.current;
                    if let Ok(type_application) = self.type_application() {
                        chained.push(ExpressionChain::TypeApplicationNode(type_application));
                    } else {
                        self.current = current;
                        break;
                    }
                },
                TokenData::Plus => {
                    let start_token = self.expect(TokenData::Plus, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::Plus,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::Minus => {
                    let start_token = self.expect(TokenData::Minus, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::Minus,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::Multiply => {
                    let start_token = self.expect(TokenData::Multiply, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::Multiply,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::Divide => {
                    let start_token = self.expect(TokenData::Divide, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::Divide,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::LessThan => {
                    let start_token = self.expect(TokenData::LessThan, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::LessThan,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::LessThanOrEqual => {
                    let start_token = self.expect(TokenData::LessThanOrEqual, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::LessThanOrEqual,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::GreaterThan => {
                    let start_token = self.expect(TokenData::GreaterThan, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::GreaterThan,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::GreaterThanOrEqual => {
                    let start_token = self.expect(TokenData::GreaterThanOrEqual, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::GreaterThanOrEqual,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::EqualTo => {
                    let start_token = self.expect(TokenData::EqualTo, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::Equal,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::NotEqualTo => {
                    let start_token = self.expect(TokenData::NotEqualTo, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::NotEqual,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::LogicalAnd => {
                    let start_token = self.expect(TokenData::LogicalAnd, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::And,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                TokenData::LogicalOr => {
                    let start_token = self.expect(TokenData::LogicalOr, "chainable_expr")?;
                    let rhs = self.expression()?;
                    let location = LocationContext::merge(&start_token.location, &rhs.location());
                    let bin_op = BinaryOperation {
                        location,
                        op: BinaryOperator::Or,
                        rhs: rhs,
                    };
                    chained.push(ExpressionChain::BinaryOperationNode(bin_op));
                },
                _ => break,
            }
        }

        let location = if chained.is_empty() {
            start.location()
        } else {
            LocationContext::merge(&start.location(), &chained.last().unwrap().location())
        };

        Ok(ChainableExpression {
            location,
            start,
            chained,
        })
    }

    fn expression_start(&mut self) -> Result<ExpressionStart, ParseError> {
        return match self.current_token_data() {
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
                let token = self.expect(TokenData::SelfVariable, "expression_start")?;
                Ok(ExpressionStart::VariableNode(Variable::SelfVariable(token.location)))
            },
            TokenData::SelfType => {
                let token = self.expect(TokenData::SelfType, "expression_start")?;
                Ok(ExpressionStart::VariableNode(Variable::SelfType(token.location)))
            },
            TokenData::Identifier(id) => {
                let token = self.expect(IDENTIFIER, "expression_start")?;
                Ok(ExpressionStart::VariableNode(Variable::Name((id, token.location))))
            },
            TokenData::StringLit(s) => {
                let token = self.expect(STRING_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::StringValue((s, token.location))))
            },
            TokenData::CharLit(c) => {
                let token = self.expect(CHAR_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::CharValue((c, token.location))))
            },
            TokenData::BooleanLit(b) => {
                let token = self.expect(BOOL_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::BooleanValue((b, token.location))))
            },
            TokenData::IntegerLit(i) => {
                let token = self.expect(INT_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::IntegerValue((i, token.location))))
            },
            TokenData::FloatLit(f) => {
                let token = self.expect(FLOAT_LIT, "expression_start")?;
                Ok(ExpressionStart::ValueNode(Value::FloatValue((f, token.location))))
            },
            t => {
                let message = format!("unable to parse expression_start at {}, pos: {}", t, self.current);
                Err(ParseError::Message(message))
            }
        }
    }

    fn field_access(&mut self) -> Result<FieldAccess, ParseError> {
        let start_token = self.expect(TokenData::Dot, "field_access")?;
        let id_token = self.expect(IDENTIFIER, "field_access")?;
        let id = Self::extract_identifier(&id_token.data).unwrap();

        let location = LocationContext::merge(&start_token.location, &id_token.location);

        Ok(FieldAccess {
            location,
            field_name: id,
        })
    }

    fn module_access(&mut self) -> Result<ModuleAccess, ParseError> {
        let start_token = self.expect(TokenData::DoubleColon, "module_access")?;
        let id_token = self.expect(IDENTIFIER, "module_access")?;
        let id = Self::extract_identifier(&id_token.data).unwrap();

        let location = LocationContext::merge(&start_token.location, &id_token.location);

        Ok(ModuleAccess {
            location,
            name: id,
        })
    }

    fn array_access(&mut self) -> Result<ArrayAccess, ParseError> {
        let start_token = self.expect(TokenData::OpenSquareBracket, "array_access")?;
        let index_expr = self.expression()?;
        let end_token = self.expect(TokenData::CloseSquareBracket, "array_access")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(ArrayAccess {
            location,
            index_expr,
        })
    }

    fn object_initialization(&mut self) -> Result<ObjectInitialization, ParseError> {
        let start_token = self.expect(TokenData::OpenCurlyBracket, "object_initialization")?;

        let mut fields = HashMap::new();
        loop {
            match self.expect(IDENTIFIER, "object_initialization") {
                Ok(id_token) => {
                    let id = Self::extract_identifier(&id_token.data).unwrap();
                    self.expect(TokenData::Colon, "object_initialization")?;
                    let expr = self.expression()?;
                    self.expect(TokenData::Comma, "object_initialization")?;
                    fields.insert(id, expr);
                },
                Err(_) => break,
            }
        }

        let end_token = self.expect(TokenData::CloseCurlyBracket, "object_initialization")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(ObjectInitialization {
            location,
            fields,
        })
    }

    fn function_application(&mut self) -> Result<FunctionApplication, ParseError> {
        let start_token = self.expect(TokenData::OpenParen, "call")?;
        let mut args = Vec::new();

        match self.current_token_data() {
            TokenData::CloseParen => (),
            _ => {
                let expr = self.expression()?;
                args.push(expr);
                
                loop {
                    match self.current_token_data() {
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

        let end_token = self.expect(TokenData::CloseParen, "call")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(FunctionApplication {
            location,
            args,
        })
    }

    fn type_application(&mut self) -> Result<TypeApplication, ParseError> {
        let start_token = self.expect(TokenData::AtSign, "type_application")?;
        self.expect(TokenData::OpenParen, "type_application")?;
        let mut args = Vec::new();

        match self.current_token_data() {
            TokenData::CloseParen => (),
            _ => {
                let expr = self.type_identifier()?;
                args.push(expr);
                
                loop {
                    match self.current_token_data() {
                        TokenData::Comma => {
                            self.expect(TokenData::Comma, "type_application")?;
                            let expr = self.type_identifier()?;
                            args.push(expr);
                        },
                        _ => break
                    }
                }
            },
        }

        let end_token = self.expect(TokenData::CloseParen, "type_application")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(TypeApplication {
            location,
            args,
        })
    }

    fn conditional_expr(&mut self) -> Result<Conditional, ParseError> {
        let if_expr = self.if_expr()?;
        let mut if_exprs = vec![if_expr];
        let mut else_expr = None;

        loop {
            match self.expect(TokenData::Else, "conditional_expr") {
                Ok(_) => (),
                Err(_) => break,
            }

            match self.current_token_data() {
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

        let end_location = if let Some(ee) = &else_expr {
            ee.location.clone()
        } else {
            if_exprs.last().unwrap().location.clone()
        };

        let location = LocationContext::merge(&if_exprs.first().unwrap().location, &end_location);

        Ok(Conditional {
            location,
            if_exprs,
            else_expr,
        })
    }

    fn if_expr(&mut self) -> Result<If, ParseError> {
        let start_token = self.expect(TokenData::If, "if_expr")?;
        let condition = self.expression()?;
        self.expect(TokenData::OpenCurlyBracket, "if_expr")?;
        let body = self.block_body()?;
        let end_token = self.expect(TokenData::CloseCurlyBracket, "if_expr")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(If {
            location,
            condition,
            body,
        })
    }

    fn match_expr(&mut self) -> Result<Match, ParseError> {
        let start_token = self.expect(TokenData::Match, "match_expr")?;
        let expr = self.expression()?;
        self.expect(TokenData::OpenCurlyBracket, "match_expr")?;
        let patterns = self.patterns()?;
        let end_token = self.expect(TokenData::CloseCurlyBracket, "match_expr")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(Match {
            location,
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
        let end_token = self.expect(TokenData::Comma, "pattern")?;

        let location = LocationContext::merge(&expr.location(), &end_token.location);

        Ok(Pattern {
            location,
            to_match: expr,
            body,
        })
    }

    fn loop_expr(&mut self) -> Result<Loop, ParseError> {
        let start_token = self.expect(TokenData::For, "loop_expr")?;
        let init = self.variable_assignment()?;
        self.expect(TokenData::Semicolon, "loop_expr")?;
        let condition = self.expression()?;
        self.expect(TokenData::Semicolon, "loop_expr")?;
        let step = self.statement()?;
        self.expect(TokenData::OpenCurlyBracket, "loop_expr")?;
        let body = self.block_body()?;
        let end_token = self.expect(TokenData::CloseCurlyBracket, "loop_expr")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(Loop {
            location,
            init,
            condition,
            step,
            body,
        })
    }

    fn array_type_expr(&mut self) -> Result<ArrayType, ParseError> {
        let start_token = self.expect(TokenData::OpenSquareBracket, "array_type_expr")?;
        let ty = self.type_identifier()?;
        let end_token = self.expect(TokenData::CloseSquareBracket, "array_type_expr")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(ArrayType {
            location,
            ty,
        })
    }

    fn array_init_expr(&mut self) -> Result<ArrayInitialization, ParseError> {
        let start_token = self.expect(TokenData::OpenSquareBracket, "array_init_expr")?;
        let ty = self.expression()?;
        self.expect(TokenData::Semicolon, "array_init_expr")?;
        let size = self.expression()?;
        let end_token = self.expect(TokenData::CloseSquareBracket, "array_init_expr")?;

        let location = LocationContext::merge(&start_token.location, &end_token.location);

        Ok(ArrayInitialization {
            location,
            ty,
            size,
        })
    }

    fn variable_assignment(&mut self) -> Result<VariableAssignment, ParseError> {
        let start_token = self.expect(TokenData::Let, "variable_assignment")?;
        
        let target = self.expression()?;

        self.expect(TokenData::Colon, "variable_assignment")?;

        let target_type = self.type_identifier()?;

        self.expect(TokenData::Assignment, "variable_assignment")?;

        let value = self.expression()?;

        let location = LocationContext::merge(&start_token.location, &value.location());

        Ok(VariableAssignment {
            location,
            target,
            target_type,
            value,
        })
    }
}
