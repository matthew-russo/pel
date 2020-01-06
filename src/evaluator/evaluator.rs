use crate::syntax::parse_tree::*;
use super::{prelude, interpreter::Interpreter};

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock};

pub(crate) type VTableId = u32;

#[derive(Debug)]
pub(crate) struct VTables {
    next_id: VTableId,
    vtables: HashMap<VTableId, Arc<RwLock<VTable>>>,
}

impl VTables {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            vtables: HashMap::new(), 
        }
    }

    pub fn new_vtable(&mut self, vtable: VTable) -> VTableId {
        let id = self.gen_id();
        self.vtables.insert(id, Arc::new(RwLock::new(vtable)));
        id
    }

    pub fn new_symbol_with_id(&mut self, vtable: VTable, id: VTableId) {
        self.vtables.insert(id, Arc::new(RwLock::new(vtable)));
    }

    pub fn gen_id(&mut self) -> VTableId {
        let temp = self.next_id;
        self.next_id = self.next_id + 1;
        temp
    }

    pub fn load_vtable(&self, id: VTableId) -> Arc<RwLock<VTable>> {
        // TODO -> don't unwrap this and have it return Result<Symbol, Error>
        self.vtables.get(&id).unwrap().clone()
    }
}

#[derive(Debug)]
pub(crate) struct VTable {
    pub implementor: SymbolId,
    pub implementing: SymbolId,
    pub functions: HashMap<String, SymbolId>,
}

pub(crate) type SymbolId = u32;

#[derive(Debug)]
pub(crate) struct SymbolTable {
    next_id: SymbolId,
    sym_hash_to_sym_id: HashMap<String, SymbolId>,
    symbols: HashMap<SymbolId, Arc<RwLock<Symbol>>>,
}

impl SymbolTable {
    pub const UNDEFINED_SYMBOL_ID: SymbolId    = std::u32::MAX;
    pub const EMPTY_TYPE_VARIABLE_SYMBOL_ID: SymbolId = 0;
    pub const STRING_TYPE_SYMBOL_ID: SymbolId  = 1;
    pub const INT_TYPE_SYMBOL_ID: SymbolId     = 2;
    pub const FLOAT_TYPE_SYMBOL_ID: SymbolId   = 3;
    pub const CHAR_TYPE_SYMBOL_ID: SymbolId    = 4;
    pub const BOOL_TYPE_SYMBOL_ID: SymbolId    = 5;
    pub const OBJECT_TYPE_SYMBOL_ID: SymbolId  = 6;
    pub const ENUM_TYPE_SYMBOL_ID: SymbolId    = 7;
    pub const VALUE_TYPE_SYMBOL_ID: SymbolId   = 8;
    pub const MAIN_MODULE_SYMBOL_ID: SymbolId  = 9;

    pub fn new() -> Self {
        let mut symbols = HashMap::new();

        symbols.insert(Self::EMPTY_TYPE_VARIABLE_SYMBOL_ID, Arc::new(RwLock::new(Symbol::TypeVariable(None))));
        symbols.insert(Self::MAIN_MODULE_SYMBOL_ID,         Arc::new(RwLock::new(Symbol::Module(Module {
            parent: None,
            name: "main".into(),
            env: Arc::new(RwLock::new(Environment::root())),
        } ))));

        let mut sym_table = Self {
            next_id: 10,
            sym_hash_to_sym_id: HashMap::new(),
            symbols,
        };

        let main_sym = sym_table.load_symbol(Self::MAIN_MODULE_SYMBOL_ID);
        let mut main_sym_writable = main_sym.write().unwrap();
        if let Symbol::Module(main) = main_sym_writable.deref_mut() {
            main.env.write().unwrap().overwrite_with(prelude::prelude(&mut sym_table));
        }

        sym_table
    }

    pub fn new_symbol(&mut self, sym: Symbol) -> SymbolId {
        let id = self.gen_id();
        self.symbols.insert(id, Arc::new(RwLock::new(sym)));
        id
    }

    pub fn new_symbol_with_id(&mut self, sym: Symbol, id: SymbolId) {
        self.symbols.insert(id, Arc::new(RwLock::new(sym)));
    }

    pub fn gen_id(&mut self) -> SymbolId {
        let temp = self.next_id;
        self.next_id = self.next_id + 1;
        temp
    }

    pub fn get_or_create_symbol(&mut self, sym: Symbol) -> SymbolId {
        if let Some(hash) = sym.sym_hash(self) {
            match self.sym_hash_to_sym_id.get(&hash) {
                Some(id) => id.clone(),
                None => {
                    let id = self.gen_id();
                    self.sym_hash_to_sym_id.insert(hash, id);
                    self.symbols.insert(id, Arc::new(RwLock::new(sym)));
                    id
                },
            }
        } else {
            let id = self.gen_id();
            self.symbols.insert(id, Arc::new(RwLock::new(sym)));
            id

        }
    }

    pub fn load_symbol(&self, id: SymbolId) -> Arc<RwLock<Symbol>> {
        // TODO -> don't unwrap this and have it return Result<Symbol, Error>
        self.symbols.get(&id).unwrap().clone()
    }

    pub fn replace_symbol(&mut self, dest_id: SymbolId, value: Arc<RwLock<Symbol>>) {
        self.symbols.insert(dest_id, value); 
    }

    pub fn reassign_id(&mut self, dest_id: SymbolId, value_id: SymbolId) {
        let value = self.load_symbol(value_id);
        self.symbols.insert(dest_id, value);
    }

}

#[derive(Debug)]
pub(crate) struct Environment {
    pub parent: Option<Arc<RwLock<Environment>>>,
    locals: HashMap<String, SymbolId>,
}

impl Environment {
    pub fn root() -> Self {
        Self {
            parent: None,
            locals: HashMap::new(),
        }
    }

    pub fn from_parent(parent: &Arc<RwLock<Environment>>) -> Self {
        Self {
            parent: Some(Arc::clone(parent)),
            locals: HashMap::new(),
        }
    }

    pub fn overwrite_with(&mut self, new_locals: HashMap<String, SymbolId>) {
        self.locals = new_locals;
    }

    pub fn define(&mut self, name: String, value: SymbolId) {
        self.locals.insert(name, value);
    }

    pub fn get_symbol_by_name(&self, name: &String) -> Option<SymbolId> {
        if self.locals.contains_key(name) {
            let sym_id = self.locals.get(name).unwrap();
            return Some(*sym_id);
        }

        match self.parent {
            Some(ref p) => p.read().unwrap().get_symbol_by_name(name),
            None => None
        }
    }
}

pub(crate) trait SymHash {
    fn sym_hash(&self, table: &SymbolTable) -> Option<String>;
}

#[derive(Clone, Debug)]
pub(crate) enum Symbol {
    Module(Module),
    Contract(Contract),
    Object(Object),
    ObjectInstance(ObjectInstance),
    Enum(Enum),
    EnumInstance(EnumInstance),
    SelfVariable(SymbolId),
    FunctionSignature(FunctionSignature),
    Function(Function),
    NativeFunction(NativeFunction),
    FunctionInvocation(FunctionInvocation),
    ValueType(ValueType),
    Value(Value),
    TypeVariable(Option<SymbolId>),
}

// i need a way for Option<<Int>> here
// and Option<<Int>> here to both result
// in the same type id. this gets more
// complicated when modules are involved
// to do this we need the fully qualified module/object
// path and the same for all of its type arguments

impl Symbol {
    // get_size(&mut self) -> u64;
    // defined_at(&mut self) -> Span;

    pub fn get_type(&self) -> SymbolId {
        match self {
            Symbol::Object(_) => {
                SymbolTable::OBJECT_TYPE_SYMBOL_ID
            },
            Symbol::ObjectInstance(oi) => {
                oi.ty.clone()
            },
            Symbol::Enum(_) => {
                SymbolTable::ENUM_TYPE_SYMBOL_ID
            },
            Symbol::EnumInstance(ei) => {
                ei.ty.clone()
            },
            Symbol::Function(_) => {
                SymbolTable::UNDEFINED_SYMBOL_ID
            },
            Symbol::NativeFunction(_) => {
                SymbolTable::UNDEFINED_SYMBOL_ID
            },
            Symbol::ValueType(_) => {
                SymbolTable::VALUE_TYPE_SYMBOL_ID
                
            },
            Symbol::Value(v) => {
                match v {
                    Value::StringValue(_)  => SymbolTable::STRING_TYPE_SYMBOL_ID,
                    Value::IntegerValue(_) => SymbolTable::INT_TYPE_SYMBOL_ID,
                    Value::FloatValue(_)   => SymbolTable::FLOAT_TYPE_SYMBOL_ID,
                    Value::CharValue(_)    => SymbolTable::CHAR_TYPE_SYMBOL_ID,
                    Value::BooleanValue(_) => SymbolTable::BOOL_TYPE_SYMBOL_ID,
                }
            },
            Symbol::TypeVariable(_) => {
                SymbolTable::UNDEFINED_SYMBOL_ID
            }
            _ => unimplemented!(),
        }
    }

    pub fn is_type_variable(&self) -> bool {
        match self {
            Symbol::TypeVariable(_) => true,
            _ => false,
        }
    }

    pub fn is_unbound_type_variable(&self) -> bool {
        match self {
            Symbol::TypeVariable(Some(_)) => true,
            _ => false,
        }
    }

    pub fn is_a(lower_id: SymbolId, upper_id: SymbolId, table: &SymbolTable) -> bool {
        if lower_id == upper_id {
            return true;
        }

        let lower = table.load_symbol(lower_id);
        let lower_readable = lower.read().unwrap();

        match lower_readable.deref() {
            Symbol::Object(o) => {
                o.vtables.contains_key(&upper_id)
            },
            Symbol::Enum(e) => {
                e.vtables.contains_key(&upper_id)
            },
            _ => false,
        }
    }
}

impl SymHash for Symbol {
    fn sym_hash(&self, table: &SymbolTable) -> Option<String> {
        match self {
            Symbol::Module(m) => {
                m.sym_hash(table)
            },
            Symbol::Contract(c) => {
                c.sym_hash(table)
            },
            Symbol::Object(o) => {
                o.sym_hash(table)
            },
            Symbol::Enum(e) => {
                e.sym_hash(table)
            },
            Symbol::ValueType(vt) => {
                vt.sym_hash(table)
            },
            Symbol::FunctionSignature(fs) => {
                fs.sym_hash(table)
            },
            Symbol::Function(f) => {
                f.sym_hash(table)
            },
            Symbol::TypeVariable(maybe_tv_id) => {
                match maybe_tv_id {
                    Some(tv_id) => {
                        let tv_sym = table.load_symbol(*tv_id);
                        let readable_tv_sym = tv_sym.read().unwrap();
                        readable_tv_sym.sym_hash(table)
                    },
                    None => panic!("trying to take sym hash for TypeVariable Hole. is this valid??"),
                }
            },
            _ => None
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Module {
    pub parent: Option<Arc<RwLock<Module>>>,
    pub name: String,
    pub env: Arc<RwLock<Environment>>,
}

impl SymHash for Module {
    fn sym_hash(&self, table: &SymbolTable) -> Option<String> {
        let mut hash = String::new();
        if let Some(ref p) = self.parent {
            hash = p.read().unwrap().sym_hash(table).unwrap();
        }

        Some([hash, self.name.clone()].join("::"))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Contract {
    pub parent: SymbolId,
    pub name: String,
    pub type_arguments: Vec<(String, SymbolId)>,
    pub required_functions: Vec<SymbolId>,
}

impl SymHash for Contract {
    fn sym_hash(&self, table: &SymbolTable) -> Option<String> {
        let parent = table.load_symbol(self.parent);
        let hash = parent.read().unwrap().sym_hash(table).unwrap();

        let to_append = if !self.type_arguments.is_empty() {
            let type_arg_sym_hash = self.type_arguments
                .iter()
                .map(|(_, sym_id)| table.load_symbol(*sym_id).read().unwrap().sym_hash(table).unwrap())
                .collect::<Vec<String>>()
                .join(",");
   
            ["<<".into(), type_arg_sym_hash, ">>".into()].join("")
        } else {
            String::new()
        };
        
        Some([hash, self.name.clone(), to_append].join("::"))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Object {
    pub parent: SymbolId,
    pub name: String,
    pub type_arguments: Vec<(String, SymbolId)>,
    pub fields: HashMap<String, SymbolId>,
    pub methods: HashMap<String, SymbolId>,
    pub vtables: HashMap<SymbolId, VTableId>, // id of the contract -> impls of functions
}

impl SymHash for Object {
    fn sym_hash(&self, table: &SymbolTable) -> Option<String> {
        let parent = table.load_symbol(self.parent);
        let hash = parent.read().unwrap().sym_hash(table).unwrap();

        let to_append = if !self.type_arguments.is_empty() {
            let type_arg_sym_hash = self.type_arguments
                .iter()
                .map(|(type_var_name, sym_id)| {
                    let type_var_sym = table.load_symbol(*sym_id);
                    let readable_type_var_sym = type_var_sym.read().unwrap();
                    match readable_type_var_sym.deref() {
                        Symbol::TypeVariable(Some(tvt)) => {
                            table.load_symbol(*tvt).read().unwrap().sym_hash(table).unwrap()
                        },
                        _ => type_var_name.clone()
                    }
                })
                .collect::<Vec<String>>()
                .join(",");
   
            ["<<".into(), type_arg_sym_hash, ">>".into()].join("")
        } else {
            String::new()
        };
        
        Some([hash, self.name.clone(), to_append].join("::"))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ObjectInstance {
    pub ty: SymbolId,
    pub contract_ty: Option<SymbolId>,
    pub field_values: HashMap<String, SymbolId>,
}

#[derive(Clone, Debug)]
pub(crate) struct Enum {
    pub parent: SymbolId,
    pub name: String,
    pub type_arguments: Vec<(String, SymbolId)>,
    pub variants: HashMap<String, Option<SymbolId>>,
    pub variant_funcs: HashMap<String, Symbol>,
    pub methods: HashMap<String, SymbolId>,
    pub vtables: HashMap<SymbolId, VTableId>, // id of the contract -> impls of functions
}

impl SymHash for Enum {
    fn sym_hash(&self, table: &SymbolTable) -> Option<String> {
        let parent = table.load_symbol(self.parent);
        let hash = parent.read().unwrap().sym_hash(table).unwrap();

        let to_append = if !self.type_arguments.is_empty() {
            let type_arg_sym_hash = self.type_arguments
                .iter()
                .map(|(type_var_name, sym_id)| table.load_symbol(*sym_id).read().unwrap().sym_hash(table).unwrap())
                .collect::<Vec<String>>()
                .join(",");
   
            ["<<".into(), type_arg_sym_hash, ">>".into()].join("")
        } else {
            String::new()
        };
        
        Some([hash, self.name.clone(), to_append].join("::"))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct EnumInstance {
    pub ty: SymbolId,
    pub contract_ty: Option<SymbolId>,
    pub variant: (String, Option<SymbolId>),
}

#[derive(Clone, Debug)]
pub(crate) struct FunctionSignature {
    pub name: String,
    pub type_parameters: Vec<(String, SymbolId)>,
    pub parameters: Vec<(String, SymbolId)>,
    pub returns: Option<SymbolId>,
}

impl FunctionSignature {
    pub fn is_compatible_with(&self, other: &FunctionSignature, table: &SymbolTable) -> bool {
        if self.name != other.name {
            return false;
        }

        for ((n1, s1), (n2, s2)) in self.type_parameters.iter().zip(other.type_parameters.iter()) {
            if !Symbol::is_a(*s2, *s1, table) {
                return false;
            }
        }

        for ((n1, s1), (n2, s2)) in self.parameters.iter().zip(other.parameters.iter()) {
            if !Symbol::is_a(*s2, *s1, table) {
                return false;
            }
        }

        if let Some(ret_id) = self.returns {
            if let Some(other_ret_id) = other.returns {
                return Symbol::is_a(other_ret_id, ret_id, table);
            } else {
                return false;
            }
        }

        return true;
    }
}

impl SymHash for FunctionSignature {
    fn sym_hash(&self, table: &SymbolTable) -> Option<String> {
        let type_params = if !self.type_parameters.is_empty() {
            let tp_str = self.type_parameters
                .iter()
                .map(|(n, id)| {
                    let sym = table.load_symbol(*id);
                    let sym_readable = sym.read().unwrap();
                    sym_readable.sym_hash(table).unwrap()
                })
                .collect::<Vec<String>>()
                .join(", ");

            format!("<{}>", tp_str)
        } else {
            String::new()
        };

        let params = if !self.parameters.is_empty() {
            self.parameters
                .iter()
                .map(|(n, id)| {
                    let sym = table.load_symbol(*id);
                    let sym_readable = sym.read().unwrap();
                    sym_readable.sym_hash(table).unwrap()
                })
                .collect::<Vec<String>>()
                .join(", ")
        } else {
            String::new()
        };

        let returns = match self.returns {
            Some(id) => {
                let return_sym = table.load_symbol(id);
                let return_sym_readable = return_sym.read().unwrap();
                let hash = return_sym_readable.sym_hash(table).unwrap();

                format!(" -> {}", hash)
            },
            None => String::new(),
        };
        
        let hash = format!("func{} {}({}){}", type_params, self.name, params, returns);
        Some(hash)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Function {
    pub parent: SymbolId,
    pub signature: SymbolId,
    pub body: BlockBody,
    pub environment: Arc<RwLock<Environment>>,
}

impl SymHash for Function {
    fn sym_hash(&self, table: &SymbolTable) -> Option<String> {
        let parent = table.load_symbol(self.parent);

        let parent_hash = match parent.read().unwrap().deref() {
            Symbol::Module(ref m) => m.sym_hash(table).unwrap(),
            Symbol::Object(ref o) => o.sym_hash(table).unwrap(),
            Symbol::Enum(ref e)   => e.sym_hash(table).unwrap(),
            _ => unreachable!(),
        };

        let signature = table.load_symbol(self.signature);
        let sig_hash = signature.read().unwrap().sym_hash(table).unwrap();

        Some([parent_hash, sig_hash].join("::"))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct FunctionInvocation {
    pub func_id: SymbolId,
    pub args: Vec<SymbolId>,
}

#[derive(Clone)]
pub(crate) struct NativeFunction {
    pub name: String,
    pub args: Vec<Expression>,
    pub func: fn(&mut Interpreter, Vec<SymbolId>) -> Option<SymbolId>,
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeFunction-{}>", self.name)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum ValueType {
    BooleanType,
    CharType,
    StringType,
    IntegerType,
    FloatType,
}
impl SymHash for ValueType {
    fn sym_hash(&self, _table: &SymbolTable) -> Option<String> {
        let sh = match self {
            ValueType::BooleanType => "bool".into(),
            ValueType::CharType => "char".into(),
            ValueType::StringType => "string".into(),
            ValueType::IntegerType => "int".into(),
            ValueType::FloatType => "float".into(),
        };

        Some(sh)
    }
}

pub(crate) trait Callable<E: Evaluator> {
    fn call(&self, evaluator: &mut E, args: Vec<SymbolId>) -> Option<SymbolId>;
}

// Some of these are just structural / control flow
// and some of them are actually content
// there is probably a better abstraction for this
pub(crate) trait Evaluator {
    fn visit_program(&mut self, program: &Program);
    fn visit_declaration(&mut self, declaration: &Declaration);
    fn visit_enum_declaration(&mut self, enum_decl: &EnumDeclaration);
    fn visit_object_declaration(&mut self, obj_decl: &ObjectDeclaration);
    fn visit_contract_declaration(&mut self, contract_decl: &ContractDeclaration);
    fn visit_implementation_declaration(&mut self, impl_decl: &ImplementationDeclaration);
    fn visit_variant_declaration(&mut self, variant_decl: &VariantDeclaration);
    fn visit_function_declaration(&mut self, function_decl: &FunctionDeclaration);
    fn visit_function_signature(&mut self, function_decl: &crate::syntax::parse_tree::FunctionSignature);
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
