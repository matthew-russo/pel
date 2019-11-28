use crate::syntax::syntax::*;
use super::prelude;

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

pub(crate) type SymbolId = u32;

#[derive(Debug)]
pub(crate) struct SymbolTable {
    next_id: SymbolId,
    symbols: HashMap<SymbolId, Arc<RwLock<Symbol>>>,
}

impl SymbolTable {
    pub const EMPTY_TYPE_VARIABLE_ID: SymbolId = 0;
    pub const BOOL_TYPE_SYMBOL_ID: SymbolId    = 1;
    pub const INT_TYPE_SYMBOL_ID: SymbolId     = 2;
    pub const FLOAT_TYPE_SYMBOL_ID: SymbolId   = 3;
    pub const CHAR_TYPE_SYMBOL_ID: SymbolId    = 4;
    pub const STRING_TYPE_SYMBOL_ID: SymbolId  = 5;

    pub fn new() -> Self {
        let mut symbols = HashMap::new();

        symbols.insert(Self::EMPTY_TYPE_VARIABLE_ID, Arc::new(RwLock::new(Symbol::TypeVariable(None))));
        symbols.insert(Self::BOOL_TYPE_SYMBOL_ID, Arc::new(RwLock::new(Symbol::ValueType(ValueType::BooleanType))));
        symbols.insert(Self::BOOL_TYPE_SYMBOL_ID, Arc::new(RwLock::new(Symbol::ValueType(ValueType::BooleanType))));
        symbols.insert(Self::INT_TYPE_SYMBOL_ID, Arc::new(RwLock::new(Symbol::ValueType(ValueType::IntegerType))));
        symbols.insert(Self::FLOAT_TYPE_SYMBOL_ID, Arc::new(RwLock::new(Symbol::ValueType(ValueType::FloatType))));
        symbols.insert(Self::CHAR_TYPE_SYMBOL_ID, Arc::new(RwLock::new(Symbol::ValueType(ValueType::CharType))));
        symbols.insert(Self::STRING_TYPE_SYMBOL_ID, Arc::new(RwLock::new(Symbol::ValueType(ValueType::StringType))));

        Self {
            next_id: 6,
            symbols,
        }
    }

    pub fn new_symbol(&mut self, sym: Symbol) -> SymbolId {
        let id = self.get_id();
        self.symbols.insert(id, Arc::new(RwLock::new(sym)));
        id
    }
    
    pub fn load_symbol(&self, id: SymbolId) -> Arc<RwLock<Symbol>> {
        // TODO -> don't unwrap this and have it return Result<Symbol, Error>
        self.symbols.get(&id).unwrap().clone()
    }

    pub fn reassign_id(&mut self, dest_id: SymbolId, value_id: SymbolId) {
        let value = self.load_symbol(value_id);
        self.symbols.insert(dest_id, value);
    }

    fn get_id(&mut self) -> SymbolId {
        let temp = self.next_id;
        self.next_id = self.next_id + 1;
        temp
    }

}

#[derive(Debug)]
pub(crate) struct Environment {
    pub parent: Option<Arc<RwLock<Environment>>>,
    locals: HashMap<String, SymbolId>,
}

impl Environment {
    pub fn global(symbol_table: &mut SymbolTable) -> Self {
        Self {
            parent: None,
            locals: prelude::prelude(symbol_table),
        }
    }

    pub fn from_parent(parent: &Arc<RwLock<Environment>>) -> Self {
        Self {
            parent: Some(parent.clone()),
            locals: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: SymbolId) {
        self.locals.insert(name, value);
    }

    pub fn get_symbol_by_name(&self, name: &String) -> Option<SymbolId> {
        if self.locals.contains_key(name) {
            let sym_id = self.locals.get(name).unwrap();
            return Some(*sym_id);
        }

        return match &self.parent {
            Some(env) => env.read().unwrap().get_symbol_by_name(name),
            None => None
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Symbol {
    Object(Object),
    ObjectInstance(ObjectInstance),
    Enum(Enum),
    EnumInstance(EnumInstance),
    Function(Function),
    NativeFunction(NativeFunction),
    ValueType(ValueType),
    Value(Value),
    TypeVariable(Option<Box<Symbol>>),
}

impl Symbol {
    // get_size(&mut self) -> u64;
    // get_type(&mut self) -> Type;
    // defined_at(&mut self) -> Span;
}

#[derive(Clone, Debug)]
pub(crate) enum ValueType {
    StringType,
    IntegerType,
    FloatType,
    CharType,
    BooleanType,
}


#[derive(Clone, Debug)]
pub(crate) struct Object {
    pub type_arguments: Vec<(String, SymbolId)>,
    pub fields: HashMap<String, SymbolId>,
    pub functions: Vec<Function>,
    pub methods: Vec<Function>,
}

#[derive(Clone, Debug)]
pub(crate) struct ObjectInstance {
    // TODO -> need some type of referece back to the original object / type so we can call
    // functions on it.
    pub field_values: HashMap<String, SymbolId>,
}

#[derive(Clone, Debug)]
pub(crate) struct Enum {
    pub type_arguments: Vec<(String, SymbolId)>,
    pub variants: HashMap<String, SymbolId>,
    pub functions: Vec<Function>,
    pub methods: Vec<Function>,
}

#[derive(Clone, Debug)]
pub(crate) struct EnumInstance {
    pub variant: (String, Box<SymbolId>),
}

#[derive(Clone, Debug)]
pub(crate) struct Function {
    pub type_parameters: Vec<(String, SymbolId)>,
    pub parameters: Vec<FunctionParameter>,
    pub returns: Option<Expression>,
    pub body: BlockBody,
    pub environment: Arc<RwLock<Environment>>,
}

impl Function {
    // TODO -> eerily similar to Interpreter::visit_object_declaration
    pub fn from_function_decl_syntax(func_decl: &FunctionDeclaration, current_env: &Arc<RwLock<Environment>>) -> Self {
        let type_params = func_decl.signature.type_params
            .iter()
            .map(|t| (t.clone(), SymbolTable::EMPTY_TYPE_VARIABLE_ID))
            .collect();

        Self {
            type_parameters: type_params,
            parameters: func_decl.signature.params.clone(),
            returns: func_decl.signature.returns.clone(),
            body: func_decl.body.clone(),
            environment: current_env.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum NativeFunction {
    Print,
}

pub(crate) trait Callable<E: Evaluator> {
    fn call(&self, evaluator: &mut E, args: &Vec<Expression>) -> Option<SymbolId>;
}

pub(crate) trait Evaluator {
    fn visit_program(&mut self, program: &Program);
    fn visit_declaration(&mut self, declaration: &Declaration);
    fn visit_enum_declaration(&mut self, enum_decl: &EnumDeclaration);
    fn visit_object_declaration(&mut self, obj_decl: &ObjectDeclaration);
    fn visit_contract_declaration(&mut self, contract_decl: &ContractDeclaration);
    fn visit_implementation_declaration(&mut self, impl_decl: &ImplementationDeclaration);
    fn visit_variants(&mut self, variants: &Variants);
    fn visit_variant_declaration(&mut self, variant_decl: &VariantDeclaration);
    fn visit_fields(&mut self, fields: &Fields);
    fn visit_methods(&mut self, methods: &Methods);
    fn visit_functions(&mut self, functions: &Functions);
    fn visit_function_declaration(&mut self, function_decl: &FunctionDeclaration);
    fn visit_typed_variable_declaration(&mut self, typed_var_decl: &TypedVariableDeclaration);
    fn visit_block_body(&mut self, block_body: &BlockBody);
    fn visit_statement(&mut self, statment: &Statement);
    fn visit_variable_assignment(&mut self, var_assignment: &VariableAssignment);
    fn visit_return(&mut self, return_stmt: &Return);
    fn visit_expression(&mut self, expr: &Expression) ;
    fn visit_chainable_expression(&mut self, chainable_expr: &ChainableExpression) ;
    fn visit_conditional(&mut self, conditional: &Conditional) ;
    fn visit_match(&mut self, match_node: &Match) ;
    fn visit_loop(&mut self, loop_node: &Loop) ;
    fn visit_field_access(&mut self, field_access: &FieldAccess) ;
    fn visit_module_access(&mut self, mod_access: &ModuleAccess) ;
    fn visit_object_initialization(&mut self, obj_init_node: &ObjectInitialization) ;
    fn visit_function_application(&mut self, func_app_node: &FunctionApplication) ;
    fn visit_type_application(&mut self, type_app_node: &TypeApplication) ;
    fn visit_binary_operation(&mut self, bin_op: &BinaryOperation) ;
    fn visit_unary_operation(&mut self, unary_op: &UnaryOperation) ;
    fn visit_lambda(&mut self, lambda: &Lambda) ;
}
