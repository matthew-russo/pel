use crate::syntax::parse_tree::*;
use super::{prelude, interpreter::Interpreter};

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock};

// Type vs Value
//         but a value can "be" a type. ie point to a type

// so a Value can be a(n):
// - Reference to an Object Instance
// - Reference to an Enum Instance
// - Reference to a Contract Instance
// - Reference to a Function
// - Reference to a Module
// - Reference to a Type
// - Scalar

// whereas things that result in a new Type are:
// - Object Declarations
// - Enum Declarations
// - Contract Declarations
// - Module Declarations
// - Function Declarations (both top level and in other Types)

// You cannot create any new Types at Runtime and all references to a
// Type are immutable and will be evaluated at compile time.

// Expressions in syntactic positions where a type is expected are evaluated
// at compile time.
//  ( I need to understand the implications this has on garbage
//          collection and/or static allocations )
//  ( Should I have a syntax construct like Zig's "comptime" )?

#[derive(Clone, Debug)]
pub(crate) enum Value {
    Reference(Reference),
    Scalar(Scalar),
}

#[derive(Clone, Debug)]
pub(crate) struct Reference {
    ty: KindHash,
    is_self: bool,
    address: Address,
    size: u32,
}

pub(crate) enum Item {
    ObjectInstance(Arc<RwLock<ObjectInstance>>),
    EnumInstance(Arc<RwLock<EnumInstance>>),
    Function(Function),
    ModuleReference(KindHash),
    TypeReference(KindHash),
}

impl Clone for Item {
    fn clone(&self) -> Self {
        use Item::*;

        match self {
            ObjectInstance(ref oi_arc)   => ObjectInstance(Arc::clone(oi_arc)),
            EnumInstance(ref ei_arc)     => EnumInstance(Arc::clone(ei_arc)),
            ModuleReference(ref kh)      => ModuleReference(kh),
            TypeReference(ref kh)        => TypeReference(kh),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Scalar {
    Boolean(bool),
    Integer(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    Char(char),
}

impl Value {
    fn to_bool(&self) -> Option<bool> {
        match self {
            Scalar::Boolean(b) => Some(*b),
            _ => None
        }
    }

    fn to_int(&self) -> Option<i32> {
        match self {
            Scalar::Integer(i) => Some(*i),
            _ => None
        }
    }

    fn to_long(&self) -> Option<i64> {
        match self {
            Scalar::Long(i) => Some(*i),
            _ => None
        }
    }

    fn to_float(&self) -> Option<f32> {
        match self {
            Scalar::Float(f) => Some(*f),
            _ => None
        }
    }

    fn to_double(&self) -> Option<f64> {
        match self {
            Scalar::Double(f) => Some(*f),
            _ => None
        }
    }

    fn to_char(&self) -> Option<char> {
        match self {
            Scalar::Char(c) => Some(*c),
            _ => None
        }
    }

    fn to_reference(&self) -> Option<Reference> {
        match self {
            Value::Reference(r) => Some(r),
            _ => None,
        }
    }

    pub fn add(lhs: Self, rhs: Self) -> Self {
        use Scalar::*;

        match lhs {
            Integer(_) => Self::int_add(lhs, rhs),
            Long(_)    => Self::long_add(lhs, rhs),
            Float(_)   => Self::float_add(lhs, rhs),
            Double(_)  => Self::double_add(lhs, rhs),
            v => panic!("unable to add value with lhs: {:?}", v),
        }
    }

    pub fn int_add(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::IntegerValue(lhs + rhs);
    }

    pub fn float_add(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_float().unwrap();
        let rhs = rhs.to_float().unwrap();
        return Value::FloatValue(lhs + rhs);
    }

    // TODO -> This should take floats too
    pub fn subtract(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::IntegerValue(lhs - rhs);
    }

    // TODO -> This should take floats too
    pub fn multiply(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::IntegerValue(lhs * rhs);
    }

    // TODO -> This should take floats too
    pub fn divide(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::IntegerValue(lhs / rhs);
    }

    // TODO -> This should take floats too
    pub fn less_than(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs < rhs);
    }

    // TODO -> This should take floats too
    pub fn less_than_or_equal(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs <= rhs);
    }

    // TODO -> This should take floats too
    pub fn greater_than(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs > rhs);
    }

    // TODO -> This should take floats too
    pub fn greater_than_or_equal(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs >= rhs);
    }

    // TODO -> This should take any value type
    pub fn equal_to(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs == rhs);
    }

    // TODO -> This should take any value type
    pub fn not_equal_to(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs != rhs);
    }

    pub fn or(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_bool().unwrap();
        let rhs = rhs.to_bool().unwrap();
        return Value::BooleanValue(lhs || rhs);
    }

    pub fn and(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_bool().unwrap();
        let rhs = rhs.to_bool().unwrap();
        return Value::BooleanValue(lhs && rhs);
    }
}

type Address = u32;

// heap contains item
pub(crate) struct Heap {
    next_address: Address,
    items: HashMap<Address, Item>,
}

impl Heap {
    pub fn new() -> Self {

    }

    pub fn alloc(&mut self) -> Address {

    }

    pub fn free(&mut self, addr: Address) {
        unimplemented!("gc doesn't exist yet");
    }

    pub fn load(&self, addr: Address) -> Item {
        match self.items.get(&addr) {
            Some(item) => Item::clone(item),
            None => panic!("COMPILER BUG: invalid address being used: {}", addr),
        }
    }

    pub fn store(&mut self, addr: Address, item: Item) {
        self.items.insert(addr, item);
    }

    pub fn load_object_instance(&self, addr: Address) -> Option<Arc<RwLock<ObjectInstance>>> {
        match self.load(addr) {
            Some(item) => {
                match item {
                    Item::ObjectInstance(ref oi_arc) => Some(Arc::clone(oi_arc)),
                    _ => None,
                }
            },
            None => None
        }
    }

    pub fn load_enum_instance(&self, addr: Address) -> Option<Arc<RwLock<EnumInstance>>> {
        match self.load(addr) {
            Some(item) => {
                match item {
                    Item::EnumInstance(ref ei_arc) => Some(Arc::clone(ei_arc)),
                    _ => None,
                }
            },
            None => None
        }
    }

    pub fn load_type_reference(&self, addr: Address) -> Option<KindHash> {
        match self.load(addr) {
            Some(item) => {

            },
            None => None
        }
    }
}

pub(crate) enum Kind {
    Object(Object),
    Enum(Enum),
    Contract(Contract),
    Module(Module),
    FunctionSignature(FunctionSignature),
    ScalarType(),
    Type(KindHash), // probably want this to be different. probably want a table of type equivalence
}

pub(crate) type KindHash = String;

pub(crate) trait KindHashable {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash;
}

impl KindHashable for Kind {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        match self {
            Kind::Object(o) => o.kind_hash(kind_table),
            Kind::Enum(e) => e.kind_hash(kind_table),
            Kind::Contract(c) => c.kind_hash(kind_table),
            Kind::Module(m) => m.kind_hash(kind_table),
            Kind::Function(f) => f.kind_hash(kind_table),
            Kind::Type(k_id) => {
                let kind = kind_table.load(k_id);
                kind.kind_hash(kind_table)
            }
        }
    }
}

impl KindHashable for Object {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        let parent = kind_table.load(self.parent);
        let hash = parent.read().unwrap().kind_hash(kind_table).unwrap();

        let to_append = if !self.type_arguments.is_empty() {
            let type_arg_kind_hash = self.type_arguments
                .iter()
                .map(|(type_var_name, sym_id)| {
                    let type_var_sym = kind_table.load(*sym_id);
                    let readable_type_var_sym = type_var_sym.read().unwrap();
                    match readable_type_var_sym.deref() {
                        Symbol::TypeVariable(Some(tvt)) => {
                            kind_table.load(*tvt).read().unwrap().kind_hash(kind_table).unwrap()
                        },
                        _ => type_var_name.clone()
                    }
                })
                .collect::<Vec<String>>()
                .join(",");
   
            ["<<".into(), type_arg_kind_hash, ">>".into()].join("")
        } else {
            String::new()
        };
        
        [hash, self.name.clone(), to_append].join("::")

    }
}

impl KindHashable for Enum {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        let parent = kind_table.load(self.parent);
        let hash = parent.read().unwrap().kind_hash(kind_table).unwrap();

        let to_append = if !self.type_arguments.is_empty() {
            let type_arg_kind_hash = self.type_arguments
                .iter()
                .map(|(type_var_name, sym_id)| kind_table.load(*sym_id).read().unwrap().kind_hash(kind_table).unwrap())
                .collect::<Vec<String>>()
                .join(",");
   
            ["<<".into(), type_arg_kind_hash, ">>".into()].join("")
        } else {
            String::new()
        };
        
        [hash, self.name.clone(), to_append].join("::")
    }
}

impl KindHashable for Contract {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        let parent = kind_table.load(self.parent);
        let hash = parent.read().unwrap().kind_hash(kind_table).unwrap();

        let to_append = if !self.type_arguments.is_empty() {
            let type_arg_kind_hash = self.type_arguments
                .iter()
                .map(|(_, sym_id)| kind_table.load(*sym_id).read().unwrap().kind_hash(kind_table).unwrap())
                .collect::<Vec<String>>()
                .join(",");
   
            ["<<".into(), type_arg_kind_hash, ">>".into()].join("")
        } else {
            String::new()
        };
        
        [hash, self.name.clone(), to_append].join("::")
    }
}

impl KindHashable for Module {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        let mut hash = String::new();
        if let Some(ref p) = self.parent {
            hash = p.read().unwrap().kind_hash(kind_table).unwrap();
        }

        [hash, self.name.clone()].join("::")
    }
}

impl KindHashable for Function {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        let type_params = if !self.type_parameters.is_empty() {
            let tp_str = self.type_parameters
                .iter()
                .map(|(n, id)| {
                    let sym = kind_table.load(*id);
                    let sym_readable = sym.read().unwrap();
                    sym_readable.kind_hash(kind_table).unwrap()
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
                    let sym = kind_table.load(*id);
                    let sym_readable = sym.read().unwrap();
                    sym_readable.kind_hash(kind_table).unwrap()
                })
                .collect::<Vec<String>>()
                .join(", ")
        } else {
            String::new()
        };

        let returns = match self.returns {
            Some(id) => {
                let return_sym = kind_table.load(id);
                let return_sym_readable = return_sym.read().unwrap();
                let hash = return_sym_readable.kind_hash(kind_table).unwrap();

                format!(" -> {}", hash)
            },
            None => String::new(),
        };
        
        let hash = format!("func{} {}({}){}", type_params, self.name, params, returns);
        Some(hash)

    }
}

pub(crate) struct KindTable {
    kinds: HashMap<KindHash, Arc<RwLock<Kind>>>,
}

impl KindTable {

}

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

    pub fn new_vtable_with_id(&mut self, vtable: VTable, id: VTableId) {
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
    pub implementor: KindHash,
    pub implementing: KindHash,
    pub functions: HashMap<String, KindHash>,
}

#[derive(Debug)]
pub(crate) struct Environment {
    pub parent: Option<Arc<RwLock<Environment>>>,
    locals: HashMap<String, Value>,
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

    pub fn overwrite_with(&mut self, new_locals: HashMap<String, Value>) {
        self.locals = new_locals;
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.locals.insert(name, value);
    }

    pub fn get_value_by_name(&self, name: &String) -> Option<Value> {
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

// #[derive(Clone, Debug)]
// pub(crate) enum Symbol {
//     Module(Module),
//     Contract(Contract),
//     Object(Object),
//     ObjectInstance(ObjectInstance),
//     Enum(Enum),
//     EnumInstance(EnumInstance),
//     SelfVariable(SymbolId),
//     FunctionSignature(FunctionSignature),
//     Function(Function),
//     NativeFunction(NativeFunction),
//     ValueType(ValueType),
//     Value(Value),
//     TypeVariable(Option<SymbolId>),
// }
// 
// // i need a way for Option<<Int>> here
// // and Option<<Int>> here to both result
// // in the same type id. this gets more
// // complicated when modules are involved
// // to do this we need the fully qualified module/object
// // path and the same for all of its type arguments
// 
// impl Symbol {
//     // get_size(&mut self) -> u64;
//     // defined_at(&mut self) -> Span;
// 
//     pub fn get_type(&self) -> SymbolId {
//         match self {
//             Symbol::Object(_) => {
//                 SymbolTable::OBJECT_TYPE_SYMBOL_ID
//             },
//             Symbol::ObjectInstance(oi) => {
//                 oi.ty.clone()
//             },
//             Symbol::Enum(_) => {
//                 SymbolTable::ENUM_TYPE_SYMBOL_ID
//             },
//             Symbol::EnumInstance(ei) => {
//                 ei.ty.clone()
//             },
//             Symbol::Function(_) => {
//                 SymbolTable::UNDEFINED_SYMBOL_ID
//             },
//             Symbol::NativeFunction(_) => {
//                 SymbolTable::UNDEFINED_SYMBOL_ID
//             },
//             Symbol::ValueType(_) => {
//                 SymbolTable::VALUE_TYPE_SYMBOL_ID
//                 
//             },
//             Symbol::Value(v) => {
//                 match v {
//                     Value::StringValue(_)  => SymbolTable::STRING_TYPE_SYMBOL_ID,
//                     Value::IntegerValue(_) => SymbolTable::INT_TYPE_SYMBOL_ID,
//                     Value::FloatValue(_)   => SymbolTable::FLOAT_TYPE_SYMBOL_ID,
//                     Value::CharValue(_)    => SymbolTable::CHAR_TYPE_SYMBOL_ID,
//                     Value::BooleanValue(_) => SymbolTable::BOOL_TYPE_SYMBOL_ID,
//                 }
//             },
//             Symbol::TypeVariable(_) => {
//                 SymbolTable::UNDEFINED_SYMBOL_ID
//             }
//             _ => unimplemented!(),
//         }
//     }
// 
//     pub fn is_type_variable(&self) -> bool {
//         match self {
//             Symbol::TypeVariable(_) => true,
//             _ => false,
//         }
//     }
// 
//     pub fn is_unbound_type_variable(&self) -> bool {
//         match self {
//             Symbol::TypeVariable(Some(_)) => true,
//             _ => false,
//         }
//     }
// 
//     pub fn is_a(lower_id: SymbolId, upper_id: SymbolId, table: &SymbolTable) -> bool {
//         if lower_id == upper_id {
//             return true;
//         }
// 
//         let lower = table.load_symbol(lower_id);
//         let lower_readable = lower.read().unwrap();
// 
//         match lower_readable.deref() {
//             Symbol::Object(o) => {
//                 o.vtables.contains_key(&upper_id)
//             },
//             Symbol::Enum(e) => {
//                 e.vtables.contains_key(&upper_id)
//             },
//             _ => false,
//         }
//     }
// }

#[derive(Clone, Debug)]
pub(crate) struct Module {
    pub parent: Option<Arc<RwLock<KindHash>>>,
    pub name: String,
    pub env: Arc<RwLock<Environment>>,
}

#[derive(Clone, Debug)]
pub(crate) struct Contract {
    pub parent: KindHash,
    pub name: String,
    pub type_arguments: Vec<(String, KindHash)>,
    pub required_functions: Vec<KindHash>,
}

#[derive(Clone, Debug)]
pub(crate) struct Object {
    pub parent: KindHash,
    pub name: String,
    pub type_arguments: Vec<(String, KindHash)>,
    pub fields: HashMap<String, KindHash>,
    pub methods: HashMap<String, KindHash>,
    pub vtables: HashMap<KindHash, VTableId>, // id of the contract -> impls of functions
}

#[derive(Clone, Debug)]
pub(crate) struct ObjectInstance {
    pub ty: KindHash,
    pub contract_ty: Option<KindHash>,
    pub fields: HashMap<String, Value>,
}

#[derive(Clone, Debug)]
pub(crate) struct Enum {
    pub parent: KindHash,
    pub name: String,
    pub type_arguments: Vec<(String, KindHash)>,
    pub variant_tys: HashMap<String, Option<KindHash>>,
    pub variant_values: HashMap<String, Address>,
    pub methods: HashMap<String, KindHash>,
    pub vtables: HashMap<KindHash, VTableId>, // id of the contract -> impls of functions
}

#[derive(Clone, Debug)]
pub(crate) struct EnumInstance {
    pub ty: KindHash,
    pub contract_ty: Option<KindHash>,
    pub variant: (String, Option<Value>),
}

#[derive(Clone, Debug)]
pub(crate) struct FunctionSignature {
    pub name: String,
    pub type_parameters: Vec<(String, KindHash)>,
    pub parameters: Vec<(String, KindHash)>,
    pub returns: Option<KindHash>,
}

impl FunctionSignature {
    pub fn is_compatible_with(&self, other: &FunctionSignature, table: &SymbolTable) -> bool {
        if self.name != other.name {
            return false;
        }

        for ((n1, s1), (n2, s2)) in self.type_parameters.iter().zip(other.type_parameters.iter()) {
            if !Kind::is_a(*s2, *s1, table) {
                return false;
            }
        }

        for ((n1, s1), (n2, s2)) in self.parameters.iter().zip(other.parameters.iter()) {
            if !Kind::is_a(*s2, *s1, table) {
                return false;
            }
        }

        if let Some(ret_id) = self.returns {
            if let Some(other_ret_id) = other.returns {
                return Kind::is_a(other_ret_id, ret_id, table);
            } else {
                return false;
            }
        }

        return true;
    }
}

impl KindHashable for FunctionSignature {
    fn kind_hash(&self, table: &KindTable) -> Option<String> {
        let type_params = if !self.type_parameters.is_empty() {
            let tp_str = self.type_parameters
                .iter()
                .map(|(n, id)| {
                    let sym = table.load(*id);
                    let sym_readable = sym.read().unwrap();
                    sym_readable.kind_hash(table).unwrap()
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
                    let sym = table.load(*id);
                    let sym_readable = sym.read().unwrap();
                    sym_readable.kind_hash(table).unwrap()
                })
                .collect::<Vec<String>>()
                .join(", ")
        } else {
            String::new()
        };

        let returns = match self.returns {
            Some(id) => {
                let return_sym = table.load(id);
                let return_sym_readable = return_sym.read().unwrap();
                let hash = return_sym_readable.kind_hash(table).unwrap();

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
    pub parent: Reference,
    pub signature: KindHash,
    pub body: BlockBody,
    pub environment: Arc<RwLock<Environment>>,
}

impl KindHashable for Function {
    fn kind_hash(&self, table: &KindTable) -> Option<String> {
        let parent = table.load(self.parent);

        let parent_hash = match parent.read().unwrap().deref() {
            Kind::Module(ref m) => m.kind_hash(table).unwrap(),
            Kind::Object(ref o) => o.kind_hash(table).unwrap(),
            Kind::Enum(ref e)   => e.kind_hash(table).unwrap(),
            _ => unreachable!(),
        };

        let signature = table.load_symbol(self.signature);
        let sig_hash = signature.read().unwrap().kind_hash(table).unwrap();

        Some([parent_hash, sig_hash].join("::"))
    }
}

#[derive(Clone)]
pub(crate) struct NativeFunction {
    pub name: String,
    pub args: Vec<Expression>,
    pub func: fn(&mut Interpreter, Vec<Value>) -> Option<Value>,
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeFunction-{}>", self.name)
    }
}

pub(crate) trait Callable<E: Evaluator> {
    fn call(&self, evaluator: &mut E, args: Vec<Value>) -> Option<Value>;
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
