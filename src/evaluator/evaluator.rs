use crate::syntax::parse_tree::*;
use super::{prelude, interpreter::Interpreter};

use std::sync::{Arc, RwLock};
use std::collections::HashMap;

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

impl Value {
    pub fn create_type_reference(kind_hash: KindHash, heap: &mut Heap) -> Self {
        let item = Item::TypeReference(KindHash::clone(&kind_hash));
        let addr = heap.alloc();
        heap.store(addr, item);
        
        Value::Reference(Reference::HeapReference(HeapReference {
            ty: KindHash::from(KIND_KIND_HASH_STR),
            is_self: false,
            address: addr,
            size: std::u32::MAX,
        }))
    }

    pub fn create_function(function: Function, kind_table: &KindTable, heap: &mut Heap) -> Self {
        let kind_hash = function.kind_hash(kind_table);
        let func = Item::Function(function);
        let addr = heap.alloc();
        heap.store(addr, func);
        
        Value::Reference(Reference::HeapReference(HeapReference {
            ty: kind_hash,
            is_self: false,
            address: addr,
            size: std::u32::MAX,
        }))
    }

    pub fn to_ref(&self) -> Option<Reference> {
        match self {
            Value::Reference(r) => Some(Reference::clone(r)),
            _ => None,
        }
    }

    pub fn to_scalar(&self) -> Option<Scalar> {
        match self {
            Value::Scalar(r) => Some(Scalar::clone(r)),
            _ => None,
        }
    }

    pub fn get_ty(&self) -> KindHash {
        match self {
            Value::Reference(Reference::StackReference(r)) => r.ty.kind_hash(),
            Value::Reference(Reference::HeapReference(r)) => KindHash::clone(&r.ty),
            Value::Scalar(s) => {
                match s {
                    Scalar::Boolean(_) => ScalarType::bool_kind_hash(),
                    Scalar::Char(_)    => ScalarType::char_kind_hash(),
                    Scalar::Integer(_) => ScalarType::int_kind_hash(),
                    Scalar::Long(_)    => ScalarType::long_kind_hash(),
                    Scalar::Float(_)   => ScalarType::float_kind_hash(),
                    Scalar::Double(_)  => ScalarType::double_kind_hash(),
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Reference {
    StackReference(StackReference),
    HeapReference(HeapReference),
}

impl Reference {
    pub fn to_stack_ref(&self) -> Option<&StackReference> {
        match self {
            Reference::StackReference(r) => Some(r),
            _ => None,
        }
    }

    pub fn to_heap_ref(&self) -> Option<&HeapReference> {
        match self {
            Reference::HeapReference(r) => Some(r),
            _ => None,
        }
    }

    pub fn get_ty(&self) -> KindHash {
        match self {
            Reference::StackReference(r) => {
                r.ty.kind_hash()
            },
            Reference::HeapReference(r) => {
                KindHash::clone(&r.ty)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct StackReference {
    pub ty: ScalarType,
    pub index: usize,
    pub size: u32,
}

#[derive(Clone, Debug)]
pub(crate) struct HeapReference {
    pub ty: KindHash,
    pub is_self: bool,
    pub address: Address,
    pub size: u32,
}

#[derive(Debug)]
pub(crate) enum Item {
    Array(Arc<RwLock<Array>>),
    ObjectInstance(Arc<RwLock<ObjectInstance>>),
    EnumInstance(Arc<RwLock<EnumInstance>>),
    Function(Function),
    ModuleReference(KindHash),
    TypeReference(KindHash),
}

impl Item {
    pub fn to_object_instance(&self) -> Option<Arc<RwLock<ObjectInstance>>> {
        match self {
            Item::ObjectInstance(oi_arc) => Some(Arc::clone(oi_arc)),
            _ => None,
        }
    }

    pub fn to_enum_instance(&self) -> Option<Arc<RwLock<EnumInstance>>> {
        match self {
            Item::EnumInstance(ei_arc) => Some(Arc::clone(ei_arc)),
            _ => None,
        }
    }

    pub fn to_function(&self) -> Option<Function> {
        match self {
            Item::Function(func) => Some(Function::clone(func)),
            _ => None,
        }
    }

    pub fn to_module_reference(&self) -> Option<KindHash> {
        match self {
            Item::ModuleReference(mod_ref) => Some(KindHash::clone(mod_ref)),
            _ => None,
        }
    }

    pub fn to_type_reference(&self) -> Option<KindHash> {
        match self {
            Item::TypeReference(type_ref) => Some(KindHash::clone(type_ref)),
            _ => None,
        }
    }
}

impl Clone for Item {
    fn clone(&self) -> Self {
        use Item::*;

        match self {
            ObjectInstance(ref oi_arc)   => ObjectInstance(Arc::clone(oi_arc)),
            EnumInstance(ref ei_arc)     => EnumInstance(Arc::clone(ei_arc)),
            ModuleReference(ref kh)      => ModuleReference(KindHash::clone(kh)),
            TypeReference(ref kh)        => TypeReference(KindHash::clone(kh)),
            _ => unimplemented!("oh no"),
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

impl Scalar {
    pub fn to_bool(&self) -> Option<bool> {
        match self {
            Scalar::Boolean(b) => Some(*b),
            _ => None
        }
    }

    pub fn to_int(&self) -> Option<i32> {
        match self {
            Scalar::Integer(i) => Some(*i),
            _ => None
        }
    }

    pub fn to_long(&self) -> Option<i64> {
        match self {
            Scalar::Long(i) => Some(*i),
            _ => None
        }
    }

    pub fn to_float(&self) -> Option<f32> {
        match self {
            Scalar::Float(f) => Some(*f),
            _ => None
        }
    }

    pub fn to_double(&self) -> Option<f64> {
        match self {
            Scalar::Double(f) => Some(*f),
            _ => None
        }
    }

    pub fn to_char(&self) -> Option<char> {
        match self {
            Scalar::Char(c) => Some(*c),
            _ => None
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
        return Scalar::Integer(lhs + rhs);
    }

    pub fn long_add(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_long().unwrap();
        let rhs = rhs.to_long().unwrap();
        return Scalar::Long(lhs + rhs);
    }

    pub fn float_add(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_float().unwrap();
        let rhs = rhs.to_float().unwrap();
        return Scalar::Float(lhs + rhs);
    }

    pub fn double_add(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_double().unwrap();
        let rhs = rhs.to_double().unwrap();
        return Scalar::Double(lhs + rhs);
    }

    // TODO -> This should take floats too
    pub fn subtract(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Scalar::Integer(lhs - rhs);
    }

    // TODO -> This should take floats too
    pub fn multiply(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Scalar::Integer(lhs * rhs);
    }

    // TODO -> This should take floats too
    pub fn divide(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Scalar::Integer(lhs / rhs);
    }

    // TODO -> This should take floats too
    pub fn less_than(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Scalar::Boolean(lhs < rhs);
    }

    // TODO -> This should take floats too
    pub fn less_than_or_equal(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Scalar::Boolean(lhs <= rhs);
    }

    // TODO -> This should take floats too
    pub fn greater_than(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Scalar::Boolean(lhs > rhs);
    }

    // TODO -> This should take floats too
    pub fn greater_than_or_equal(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Scalar::Boolean(lhs >= rhs);
    }

    // TODO -> This should take any value type
    pub fn equal_to(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Scalar::Boolean(lhs == rhs);
    }

    // TODO -> This should take any value type
    pub fn not_equal_to(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Scalar::Boolean(lhs != rhs);
    }

    pub fn or(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_bool().unwrap();
        let rhs = rhs.to_bool().unwrap();
        return Scalar::Boolean(lhs || rhs);
    }

    pub fn and(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_bool().unwrap();
        let rhs = rhs.to_bool().unwrap();
        return Scalar::Boolean(lhs && rhs);
    }
}

impl std::convert::From<&crate::syntax::parse_tree::Value> for Scalar {
    fn from(parse_value: &crate::syntax::parse_tree::Value) -> Self {
        match parse_value {
            crate::syntax::parse_tree::Value::BooleanValue(b) => Scalar::Boolean(*b),
            crate::syntax::parse_tree::Value::CharValue(c)    => Scalar::Char(*c),
            crate::syntax::parse_tree::Value::IntegerValue(i) => Scalar::Integer(*i),
            crate::syntax::parse_tree::Value::FloatValue(f)   => Scalar::Float(*f),
            _ => unimplemented!("huh?"),
        }
    }
}

pub(crate) type Address = u32;

pub(crate) struct Heap {
    next_address: Address,
    items: HashMap<Address, Item>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            next_address: 1,
            items: HashMap::new(),
        }
    }

    pub fn alloc(&mut self) -> Address {
        let addr = self.next_address;
        self.next_address += 1;
        addr
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

    pub fn load_array(&self, addr: Address) -> Option<Arc<RwLock<Array>>> {
        match self.load(addr) {
            Item::Array(ref array_arc) => Some(Arc::clone(array_arc)),
            _ => None,
        }
    }

    pub fn load_object_instance(&self, addr: Address) -> Option<Arc<RwLock<ObjectInstance>>> {
        match self.load(addr) {
            Item::ObjectInstance(ref oi_arc) => Some(Arc::clone(oi_arc)),
            _ => None,
        }
    }

    pub fn load_enum_instance(&self, addr: Address) -> Option<Arc<RwLock<EnumInstance>>> {
        match self.load(addr) {
            Item::EnumInstance(ref ei_arc) => Some(Arc::clone(ei_arc)),
            _ => None,
        }
    }

    pub fn load_type_reference(&self, addr: Address) -> Option<KindHash> {
        match self.load(addr) {
            Item::TypeReference(kind_hash) => Some(KindHash::clone(&kind_hash)),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub(crate) enum Kind {
    Object(Arc<RwLock<Object>>),
    Enum(Arc<RwLock<Enum>>),
    Contract(Arc<RwLock<Contract>>),
    Module(Arc<RwLock<Module>>),
    FunctionSignature(Arc<RwLock<FunctionSignature>>),
    ScalarType(ScalarType),
    Type(KindHash), // probably want this to be different. probably want a table of type equivalence
}

impl Kind {
    pub fn is_a(lower_hash: &KindHash, upper_hash: &KindHash, kind_table: &KindTable) -> bool {
        if lower_hash == upper_hash {
            return true;
        }
        
        let lower = kind_table.load(lower_hash).unwrap();
        match lower {
            Kind::Object(o_arc) => {
                o_arc.read().unwrap().vtables.contains_key(upper_hash)
            },
            Kind::Enum(e_arc) => {
                e_arc.read().unwrap().vtables.contains_key(upper_hash)
            },
            _ => false,
        }
    }

    pub fn to_object(&self) -> Option<Arc<RwLock<Object>>> {
        use Kind::*;

        match self {
            Object(ref o) => Some(Arc::clone(o)),
            _ => None,
        }
    }
    
    pub fn to_enum(&self) -> Option<Arc<RwLock<Enum>>> {
        use Kind::*;

        match self {
            Enum(ref e) => Some(Arc::clone(e)),
            _ => None,
        }
    }

    pub fn to_contract(&self) -> Option<Arc<RwLock<Contract>>> {
        use Kind::*;

        match self {
            Contract(ref c) => Some(Arc::clone(c)),
            _ => None,
        }
    }

    pub fn to_module(&self) -> Option<Arc<RwLock<Module>>> {
        use Kind::*;

        match self {
            Module(ref m) => Some(Arc::clone(m)),
            _ => None,
        }
    }

    pub fn to_func_sig(&self) -> Option<Arc<RwLock<FunctionSignature>>> {
        use Kind::*;

        match self {
            FunctionSignature(ref fs) => Some(Arc::clone(fs)),
            _ => None,
        }
    }
    
    pub fn to_scalar_type(&self) -> Option<ScalarType> {
        match self {
            Kind::ScalarType(ref st) => Some(ScalarType::clone(st)),
            _ => None,
        }
    }

    pub fn to_type(&self) -> Option<KindHash> {
        use Kind::*;

        match self {
            Type(kind_hash) => Some(KindHash::clone(kind_hash)),
            _ => None,
        }
    }
}

impl Clone for Kind {
    fn clone(&self) -> Self {
        match self {
            Kind::Object(o_arc)   => Kind::Object(Arc::clone(o_arc)),
            Kind::Enum(e_arc)     => Kind::Enum(Arc::clone(e_arc)),
            Kind::Contract(c_arc) => Kind::Contract(Arc::clone(c_arc)),
            Kind::Module(m_arc)   => Kind::Module(Arc::clone(m_arc)),
            Kind::FunctionSignature(fs_arc) => Kind::FunctionSignature(Arc::clone(fs_arc)),
            Kind::ScalarType(ref st)  => Kind::ScalarType(ScalarType::clone(st)),
            Kind::Type(kh)        => Kind::Type(KindHash::clone(kh)),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum ScalarType {
    CharType,
    BoolType,
    IntegerType,
    LongType,
    FloatType,
    DoubleType,
}

impl ScalarType {
    pub fn kind_hash(&self) -> KindHash {
        match self {
            ScalarType::CharType    => Self::char_kind_hash(),
            ScalarType::BoolType    => Self::bool_kind_hash(),
            ScalarType::IntegerType => Self::int_kind_hash(),
            ScalarType::LongType    => Self::long_kind_hash(),
            ScalarType::FloatType   => Self::float_kind_hash(),
            ScalarType::DoubleType  => Self::double_kind_hash(),
        }
    }

    pub fn bool_kind_hash() -> KindHash {
        KindHash::from("bool")
    }

    pub fn char_kind_hash() -> KindHash {
        KindHash::from("char")
    }

    pub fn int_kind_hash() -> KindHash {
        KindHash::from("int")
    }

    pub fn long_kind_hash() -> KindHash {
        KindHash::from("long")
    }

    pub fn float_kind_hash() -> KindHash {
        KindHash::from("float")
    }

    pub fn double_kind_hash() -> KindHash {
        KindHash::from("double")
    }
}

pub(crate) type KindHash = String;

pub(crate) const KIND_KIND_HASH_STR: &str = "kind";

pub(crate) trait KindHashable {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash;
}

impl KindHashable for Kind {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        match self {
            Kind::Object(o)   => o.read().unwrap().kind_hash(kind_table),
            Kind::Enum(e)     => e.read().unwrap().kind_hash(kind_table),
            Kind::Contract(c) => c.read().unwrap().kind_hash(kind_table),
            Kind::Module(m)   => m.read().unwrap().kind_hash(kind_table),
            Kind::FunctionSignature(fs) => fs.read().unwrap().kind_hash(kind_table),
            Kind::Type(kh)    => KindHash::clone(kh),
            _ => unimplemented!("what?"),
        }
    }
}

impl KindHashable for Object {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        let to_append = if !self.type_arguments.is_empty() {
            let type_arg_kind_hash = self.type_arguments
                .iter()
                .map(|(_, kh)| KindHash::clone(kh))
                .collect::<Vec<String>>()
                .join(",");
   
            ["<<".into(), type_arg_kind_hash, ">>".into()].join("")
        } else {
            String::new()
        };
        
        [KindHash::clone(&self.parent), self.name.clone(), to_append].join("::")

    }
}

impl KindHashable for Enum {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        let to_append = if !self.type_arguments.is_empty() {
            let type_arg_kind_hash = self.type_arguments
                .iter()
                .map(|(_, kh)| KindHash::clone(kh))
                .collect::<Vec<String>>()
                .join(",");
   
            ["<<".into(), type_arg_kind_hash, ">>".into()].join("")
        } else {
            String::new()
        };
        
        [KindHash::clone(&self.parent), self.name.clone(), to_append].join("::")
    }
}

impl KindHashable for Contract {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        let to_append = if !self.type_arguments.is_empty() {
            let type_arg_kind_hash = self.type_arguments
                .iter()
                .map(|(_, kh)| KindHash::clone(kh))
                .collect::<Vec<KindHash>>()
                .join(",");
   
            ["<<".into(), type_arg_kind_hash, ">>".into()].join("")
        } else {
            String::new()
        };
        
        [KindHash::clone(&self.parent), self.name.clone(), to_append].join("::")
    }
}

impl KindHashable for Module {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        let hash = if let Some(ref p) = self.parent {
            KindHash::clone(p)
        } else {
            String::new()
        };

        [hash, self.name.clone()].join("::")
    }
}

impl KindHashable for Function {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        match self {
            Function::NativeFunction(nf) => nf.read().unwrap().kind_hash(kind_table),
            Function::PelFunction(pf) => pf.read().unwrap().kind_hash(kind_table),
        }
    }
}

impl KindHashable for NativeFunction {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        KindHash::from(format!("<NativeFunction-{}>", self.name))
    }
}

impl KindHashable for PelFunction {
    fn kind_hash(&self, kind_table: &KindTable) -> KindHash {
        [KindHash::clone(&self.parent), KindHash::clone(&self.signature)].join("::")
    }
}

pub(crate) struct KindTable {
    kinds: HashMap<KindHash, Kind>,
}

impl KindTable {
    pub fn new() -> Self {
        Self {
            kinds: HashMap::new(),
        }
    }

    pub fn create(&mut self, kind: Kind) {
        let key = kind.kind_hash(self);
        self.kinds.insert(key, kind);
    }

    pub fn load(&self, to_load: &KindHash) -> Option<Kind> {
        self.kinds.get(to_load).map(Kind::clone)
    }
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
    pub functions: HashMap<String, Value>,
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
            let value = self.locals.get(name).unwrap();
            return Some(Value::clone(value));
        }

        match self.parent {
            Some(ref p) => p.read().unwrap().get_value_by_name(name),
            None => None
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Array {
    pub ty: KindHash,
    pub length: u32,
    pub values: Vec<Value>,
}

#[derive(Clone, Debug)]
pub(crate) struct Module {
    pub parent: Option<KindHash>,
    pub name: String,
    pub env: Arc<RwLock<Environment>>,
}

impl Module {
    pub const MAIN_MODULE_KIND_HASH: &'static str = "main";
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
    pub methods: HashMap<String, Value>,
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
    pub variant_values: HashMap<String, Reference>,
    pub methods: HashMap<String, Value>,
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
    pub fn is_compatible_with(&self, other: &FunctionSignature, table: &KindTable) -> bool {
        if self.name != other.name {
            return false;
        }

        for ((n1, kh1), (n2, kh2)) in self.type_parameters.iter().zip(other.type_parameters.iter()) {
            if !Kind::is_a(kh2, kh1, table) {
                return false;
            }
        }

        for ((n1, kh1), (n2, kh2)) in self.parameters.iter().zip(other.parameters.iter()) {
            if !Kind::is_a(kh2, kh1, table) {
                return false;
            }
        }

        if let Some(ref ret_id) = self.returns {
            if let Some(ref other_ret_id) = other.returns {
                return Kind::is_a(&other_ret_id, &ret_id, table);
            } else {
                return false;
            }
        }

        return true;
    }
}

impl KindHashable for FunctionSignature {
    fn kind_hash(&self, table: &KindTable) -> String {
        let type_params = if !self.type_parameters.is_empty() {
            let tp_str = self.type_parameters
                .iter()
                .map(|(n, kh)| KindHash::clone(kh))
                .collect::<Vec<KindHash>>()
                .join(", ");
            format!("<{}>", tp_str)
        } else {
            String::new()
        };

        let params = if !self.parameters.is_empty() {
            self.parameters
                .iter()
                .map(|(n, kh)| KindHash::clone(kh))
                .collect::<Vec<KindHash>>()
                .join(", ")
        } else {
            String::new()
        };

        let returns = match self.returns {
            Some(ref kh) => format!(" -> {}", kh),
            None => String::new(),
        };
        
        format!("func{} {}({}){}", type_params, self.name, params, returns)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Function {
    PelFunction(Arc<RwLock<PelFunction>>),
    NativeFunction(Arc<RwLock<NativeFunction>>),
}

impl Function {
    pub fn to_pel_function(&self) -> Option<Arc<RwLock<PelFunction>>> {
        match self {
            Function::PelFunction(pf_arc) => Some(Arc::clone(pf_arc)),
            _ => None
        }
    }

    pub fn to_native_function(&self) -> Option<Arc<RwLock<NativeFunction>>> {
        match self {
            Function::NativeFunction(nat_arc) => Some(Arc::clone(nat_arc)),
            _ => None
        }
    }

    pub fn signature(&self, kind_table: &KindTable) -> Option<Arc<RwLock<FunctionSignature>>> {
        match self {
            Function::PelFunction(pf) => kind_table.load(&pf.read().unwrap().signature).unwrap().to_func_sig(),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PelFunction {
    pub parent: KindHash,
    pub signature: KindHash,
    pub body: BlockBody,
    pub environment: Arc<RwLock<Environment>>,
}

#[derive(Clone)]
pub(crate) struct NativeFunction {
    pub name: String,
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
    fn visit_expression(&mut self, expr: &Expression);
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
