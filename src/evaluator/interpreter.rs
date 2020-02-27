use std::collections::HashMap;
use std::fs;
use std::ops::{Deref};
use std::path::Path;
use std::sync::{Arc, RwLock};

use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use crate::syntax::parse_tree;
use crate::evaluator::pel_utils;
use crate::evaluator::prelude;
use crate::evaluator::evaluator::{
    Address,
    Array,
    ArrayInstance,
    Callable,
    Contract,
    Enum,
    EnumInstance,
    Environment,
    Evaluator,
    Function,
    FunctionSignature,
    Heap,
    HeapReference,
    Item,
    KindTable,
    Kind,
    KindHash,
    KindHashable,
    KIND_KIND_HASH_STR,
    Module,
    NativeFunction,
    Object,
    ObjectInstance,
    PelFunction,
    Reference,
    Scalar,
    StackReference,
    Value,
    VTables,
    VTable,
};

//        -- environment --            ----- state -----
//      /                   \        /                   \
// name                       symbol                      value
//
// Kind Table -- Type Information
// +------+------+----+---------+
// | Name | Kind | ID | Address |
// +------+------+----+---------+
//
// Object Table -- Inserted into during ObjectDeclarations
// +----+--------------------+
// | Id | List<FieldOffsets> |
// +----+--------------------+
//
// Object Field Offsets
// +-----------+------+--------+
// | FieldName | Kind | Offset |
// +-----------+------+--------+
//
// Function Table -- Inserted into during FunctionDeclaration
// +------+---------------+---------------+
// | Name | AddressOfBody | EnvironmentId |
// +------+---------------+---------------+

pub(crate) struct Interpreter {
    lexer: Lexer,
    parser: Parser,

    pub kind_table: KindTable,
    pub heap: Heap,
    pub vtables: VTables,
    global_env: Arc<RwLock<Environment>>,
    current_env: Arc<RwLock<Environment>>,
    current_module: KindHash,
    pub stack: Vec<Value>,
    exiting: bool,
    errors: Vec<String>,
}

const SELF_VAR_SYMBOL_NAME: &str = "self";
const SELF_TYPE_SYMBOL_NAME: &str = "Self";

impl Interpreter {
    pub fn new() -> Self {
        let mut heap = Heap::new();

        let mut kind_table = KindTable::new();

        let main_env = Arc::new(RwLock::new(Environment::root()));
        main_env.write().unwrap().overwrite_with(prelude::prelude(&mut kind_table, &mut heap));

        let mut interpreter = Self {
            lexer: Lexer::new(),
            parser: Parser::new(),
            kind_table,
            heap,
            vtables: VTables::new(),
            global_env: Arc::clone(&main_env),
            current_env: main_env,
            current_module: KindHash::from(""),
            stack: Vec::new(),
            exiting: false,
            errors: Vec::new(),
        };

        interpreter.load_stdlib();

        interpreter
    }

    fn load_stdlib(&mut self) {
        if let Err(e) = self.load_dir("stdlib", vec!["pel".into()]) {
            println!("failed to load stdlib: {:?}", e);
        }
    }

    fn load_dir(&mut self, dir: &str, current: Vec<String>) -> Result<(), Box<dyn std::error::Error>> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            let mut new_current = current.clone();
            new_current.push(path.file_stem().unwrap().to_os_string().into_string().unwrap());
            if path.is_dir() {
                self.load_dir(path.as_path().to_str().unwrap(), new_current)?;
            } else {
                self.interpret_file(path, &new_current)?;
            }
        }
    
        Ok(())
    }
    
    pub fn interpret_file<P: AsRef<Path>>(&mut self, src_file: P, module_chain: &Vec<String>) -> Result<(), Box<dyn std::error::Error>> {
        let src = fs::read_to_string(src_file)?;
        self.interpret_code(src, module_chain)
    }

    pub fn interpret_code(&mut self, code: String, module_chain: &Vec<String>) -> Result<(), Box<dyn std::error::Error>> {
        let tokens = self.lexer.lex(code)?;
        let program = self.parser.parse(tokens)?;
        self.visit_program(&program, module_chain);
        Ok(())
    }
    
    pub fn emit_error(&mut self, error: String) {
        self.errors.push(error);
    }

    pub fn set_current_env(&mut self, env: Environment) {
        self.current_env = Arc::new(RwLock::new(env));
    }

    fn get_obj_method_with_name(&self, obj: &Object, name: &String) -> Option<Reference> {
        if let Some(func_ref) = obj.methods.get(name) {
            return Some(Reference::clone(func_ref));
        }

        for (_, vtable_id) in obj.vtables.iter() {
            let vtable = self.vtables.load_vtable(*vtable_id);
            let vtable_readable = vtable.read().unwrap();

            if let Some(func_ref) = vtable_readable.functions.get(name) {
                return Some(Reference::clone(func_ref));
            }
        }

        None
    }

    fn generate_type_holes(&mut self, context: &KindHash, type_params: &Vec<String>) -> Vec<(String, KindHash)> {
        type_params
            .iter()
            .map(|type_param_name| {
                let type_hole = KindHash::from(format!("{}::TypeVariable{{{}}}", context, type_param_name));
                let address = self.heap.alloc();
                let item = Item::TypeReference(KindHash::clone(&type_hole));
                self.heap.store(address, item);
                let reference = Reference::HeapReference(
                    HeapReference {
                        ty: KindHash::clone(&type_hole),
                        is_self: false,
                        address,
                        size: 0,
                    }
                );
                self.current_env.write().unwrap().define(type_param_name.clone(), reference);
                (type_param_name.clone(), type_hole)
            })
            .collect()
    }

    fn store_in_var_name(&mut self, expr: &parse_tree::Expression, val: Value) -> Option<Reference> {
        let ce = if let parse_tree::Expression::ChainableExpressionNode(ce) = &expr {
            ce
        } else {
            let message = format!("Cannot assign to a {:?}", expr);
            panic!(message);
        };

        if !ce.chained.is_empty() {
            return None;
        }

        let var = if let parse_tree::ExpressionStart::VariableNode(var) = &ce.start {
            var
        } else {
            return None;
        };

        let var_name = match var {
            parse_tree::Variable::Name(n) => n.clone(),
            parse_tree::Variable::SelfVariable => SELF_VAR_SYMBOL_NAME.into(),
            parse_tree::Variable::SelfType => SELF_TYPE_SYMBOL_NAME.into(),
        };

        let reference = if let Some(curr_def) = self.current_env.read().unwrap().get_reference_by_name(&var_name) {
            match &curr_def {
                Reference::StackReference(sr) => {
                    self.stack[sr.index] = val;
                },
                Reference::HeapReference(hr) => {
                    let r = val.to_ref().unwrap();
                    let hr = r.to_heap_ref().unwrap();
                    let item = self.heap.load(hr.address);
                    self.heap.store(hr.address, item);
                },
                r => unimplemented!("unknown reference type: {:?}", r),
            }

            curr_def
        } else {
            let reference = match &val {
                Value::Scalar(s) => {
                    self.stack.push(Value::clone(&val));
                    Reference::StackReference(StackReference {
                        ty: s.get_ty(),
                        index: self.stack.len() - 1,
                        size: std::u32::MAX,
                    })
                },
                Value::Reference(Reference::StackReference(sr)) => {
                    let actual_val = Value::clone(&self.stack[sr.index]);
                    let ty = actual_val.get_ty();
                    self.stack.push(actual_val);
                    Reference::StackReference(StackReference {
                        ty,
                        index: self.stack.len() - 1,
                        size: std::u32::MAX,
                    })
                },
                Value::Reference(Reference::HeapReference(hr)) => {
                    let address = self.heap.alloc();
                    let item = self.heap.load(hr.address);
                    self.heap.store(address, item);

                    Reference::HeapReference(HeapReference {
                        ty: val.get_ty(),
                        address,
                        is_self: false,
                        size: std::u32::MAX,
                    })
                },
                v => unimplemented!("unrecognized value variant: {:?}", v)
            };

            self.current_env.write().unwrap().define(var_name, Reference::clone(&reference));
            reference
        };

        Some(reference)
    }

    fn store_in_array(&mut self, expr: &parse_tree::Expression, v: Value) -> Option<Reference> {
        unimplemented!("store in array -- expr: {:?}, v: {:?}", expr, v);
    }

    // this can be:
    //      new var val
    //      preexisting var name
    //      self name
    //      array slot
    fn store_at(&mut self, loc_expr: &parse_tree::Expression, v: Value) {
        self.store_in_var_name(loc_expr, Value::clone(&v))
            .or_else(|| self.store_in_array(loc_expr, Value::clone(&v)))
            .or_else(|| panic!("unable to resolve expression to location: {:?}", loc_expr));
    }

    fn reset_state(&mut self) {
        self.current_env = Arc::clone(&self.global_env);
    }

    fn find_or_create_module(&mut self, module_chain: &Vec<String>) -> KindHash{
        let mut parent_mod = None;
        let mut last_kind_hash = String::new();

        for mod_name in module_chain {
            let module = {
                if let Some(val) = self.current_env.write().unwrap().get_reference_by_name(mod_name) {
                   match val {
                        Reference::HeapReference(hr) => {
                            if let Some(m_ref) = self.heap.load_module_reference(hr.address) {
                                self.kind_table.load(&m_ref).unwrap().to_module().unwrap()
                                
                            } else {
                                panic!("expected a reference to a module but got: {:?}", hr);
                            }
                        },
                        v => panic!("expected a reference to a module but got: {:?}", v),
                   }
                } else {
                    let module = Arc::new(RwLock::new(Module {
                        parent: parent_mod,
                        name: String::clone(mod_name),
                        env: Arc::new(RwLock::new(Environment::from_parent(&self.current_env))),
                    }));
                    self.kind_table.create(Kind::Module(Arc::clone(&module)));
                    module
                }
            };

            let mod_kind_hash = module.read().unwrap().kind_hash(&self.kind_table);
            let mod_ref = Reference::create_module_reference(KindHash::clone(&mod_kind_hash), &mut self.heap);
            self.current_env.write().unwrap().define(String::clone(mod_name), mod_ref);
            self.current_env = Arc::clone(&module.read().unwrap().env);
            parent_mod = Some(KindHash::clone(&mod_kind_hash));

            last_kind_hash = mod_kind_hash;
        }

        last_kind_hash
    }

    fn load_module(&mut self, module_chain: &Vec<String>) {
        if module_chain[0] == String::from("pel") {
            self.load_stdlib_module(module_chain);
        } else {
            self.load_user_module(module_chain);
        }
    }

    fn load_stdlib_module(&mut self, module_chain: &Vec<String>) {
        let current_env = Arc::clone(&self.current_env);
        self.current_env = Arc::clone(&self.global_env);

        let file_tail = module_chain[1..module_chain.len()].join(&format!("{}", std::path::MAIN_SEPARATOR));
        let file_name = format!("stdlib{}{}.pel", std::path::MAIN_SEPARATOR, file_tail);
        if let Err(e) = self.interpret_file(file_name, module_chain) {
            panic!("failed to load stdlib module: {:?}, with error: {:?}", module_chain, e);
        }

        self.current_env = current_env;
    }

    fn load_user_module(&mut self, module_chain: &Vec<String>) {
        unimplemented!("how to load user module");
    }
}

impl Evaluator for Interpreter {
    fn visit_program(&mut self, program: &parse_tree::Program, module_chain: &Vec<String>) {
        self.reset_state();
        self.current_module = self.find_or_create_module(module_chain);

        for decl in program.declarations.iter() {
            self.visit_declaration(decl);
        }
    }

    fn visit_declaration(&mut self, decl: &parse_tree::Declaration) {
        use parse_tree::Declaration::*;

        match decl {
            EnumDeclarationNode(enum_decl)
                => self.visit_enum_declaration(enum_decl),

            ContractDeclarationNode(contract_decl)
                => self.visit_contract_declaration(contract_decl),

            ImplementationDeclarationNode(impl_decl)
                => self.visit_implementation_declaration(impl_decl),

            ObjectDeclarationNode(obj_decl)
                => self.visit_object_declaration(obj_decl),

            FunctionDeclarationNode(func_decl)
                => self.visit_function_declaration(func_decl),

            UseDeclarationNode(use_decl)
                => self.visit_use_declaration(use_decl),
        }
    }

    fn visit_enum_declaration(&mut self, enum_decl: &parse_tree::EnumDeclaration) {
        let mut local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(local_env));

        let mut enu = Enum {
            parent: KindHash::clone(&self.current_module),
            name: String::clone(&enum_decl.type_name),
            type_arguments: Vec::new(),
            variant_tys: HashMap::new(),
            variant_values: HashMap::new(),
            methods: HashMap::new(),
            vtables: HashMap::new(),
        };

        let enu_kind_hash = enu.kind_hash(&self.kind_table);
       
        enu.type_arguments = self.generate_type_holes(&enu_kind_hash, &enum_decl.type_params);
        
        enu.variant_tys = enum_decl.variants
            .iter()
            .map(|vd| {
                let contains = if let Some(expr) = &vd.contains {
                    // type check that last_local is a type/kind
                    self.visit_expression(&expr);
                    let val = self.stack.pop().unwrap();
                    match val {
                        Value::Reference(Reference::HeapReference(r)) => {
                            if r.ty != KindHash::from(KIND_KIND_HASH_STR) &&
                                !r.ty.contains("TypeVariable") {
                                panic!("reference must be pointing to a kind but is actually: {}", r.ty);
                            }
                            Some(self.heap.load_type_reference(r.address).unwrap())
                        },
                        v => {
                            panic!("variant type must be a kind but got {:?}", v);
                        }
                    }
                } else {
                    None
                };

                (vd.name.clone(), contains)
            })
            .collect();


        enu.variant_values = enum_decl.variants
            .iter()
            .map(|vd| {
                if vd.contains.is_none() {
                    let initialized_enum = EnumInstance {
                        ty: KindHash::clone(&enu_kind_hash),
                        contract_ty: None,
                        variant: (vd.name.clone(), None),
                    };

                    let item = Item::EnumInstance(Arc::new(RwLock::new(initialized_enum)));
                    let addr = self.heap.alloc();
                    self.heap.store(addr, item);

                    (String::clone(&vd.name), Reference::HeapReference(HeapReference {
                        ty: KindHash::clone(&enu_kind_hash),
                        is_self: false,
                        address: addr,
                        size: std::u32::MAX,
                    }))
                } else {
                    let enum_constructor = |interp: &mut Interpreter, args: Vec<Reference>| {
                        let enu_kind_hash = interp.heap.load(args[0].to_heap_ref().unwrap().address).to_type_reference().unwrap();

                        let variant_name_item = interp.heap.load(args[1].to_heap_ref().unwrap().address);
                        let variant_name = pel_utils::pel_string_to_rust_string(&variant_name_item, &mut interp.heap).unwrap();

                        let does_contain_ty = interp.stack[args[2].to_stack_ref().unwrap().index].to_scalar().unwrap().to_bool().unwrap();

                        let contains = if does_contain_ty {
                            let contains_kind_hash = interp.heap.load_type_reference(args[3].to_heap_ref().unwrap().address).unwrap();

                            let contains_heap_ref = args[4].to_heap_ref().unwrap();

                            if contains_heap_ref.ty != contains_kind_hash {
                                let message = format!("expected enum variant: {} to contain type {:?}, but got {}",
                                                      variant_name,
                                                      contains_kind_hash,
                                                      contains_heap_ref.ty);
                                panic!(message);
                            }

                            Some(Reference::HeapReference(HeapReference::clone(contains_heap_ref)))
                        } else {
                            None
                        };
                        
                        let enum_instance = EnumInstance {
                            ty: KindHash::clone(&enu_kind_hash),
                            contract_ty: None,
                            variant: (variant_name, contains),
                        };

                        let addr = interp.heap.alloc();
                        let enum_item = Item::EnumInstance(Arc::new(RwLock::new(enum_instance)));
                        interp.heap.store(addr, enum_item);

                        let reference = Reference::HeapReference(HeapReference {
                            ty: enu_kind_hash,
                            is_self: false,
                            address: addr,
                            size: std::u32::MAX,
                        });
                        Some(reference)
                    };

                    // TODO -> need to get the signature of this?
                    let nat_func_sig = FunctionSignature {
                        parent: KindHash::clone(&enu_kind_hash),
                        name: String::clone(&vd.name),
                        type_arguments: Vec::new(),
                        parameters: vec![/*(String, KindHash*/],
                        returns: Some(KindHash::clone(&enu_kind_hash)),
                    };
                    let nat_func_ty = nat_func_sig.kind_hash(&self.kind_table);
                    let nat_func = Function::NativeFunction(Arc::new(RwLock::new(NativeFunction {
                        name: "".into(),
                        signature: KindHash::clone(&nat_func_ty),
                        func: enum_constructor,
                    })));

                    let addr = self.heap.alloc();
                    self.heap.store(addr, Item::Function(nat_func));
                    
                    (vd.name.clone(), Reference::HeapReference(HeapReference {
                        ty: nat_func_ty,
                        is_self: false,
                        address: addr,
                        size: std::u32::MAX,
                    }))
                }
            })
            .collect();

        enu.methods = enum_decl
            .methods
            .iter()
            .map(|fd| {
                let addr = self.heap.alloc();
                self.heap.store(addr, Item::TypeReference(KindHash::clone(&enu_kind_hash)));
                let reference = Value::Reference(Reference::HeapReference(HeapReference {
                    ty: KindHash::clone(&enu_kind_hash),
                    is_self: false,
                    address: addr,
                    size: std::u32::MAX,
                }));
                self.stack.push(reference);
                self.visit_function_declaration(fd);
                let func_val = self.current_env
                    .read()
                    .unwrap()
                    .get_reference_by_name(&fd.signature.name)
                    .unwrap();
                (fd.signature.name.clone(), func_val)
            })
            .collect();

        self.kind_table.create(Kind::Enum(Arc::new(RwLock::new(enu))));

        let reference_to_enum = Reference::create_type_reference(enu_kind_hash, &mut self.heap);
        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
        self.current_env
            .write()
            .unwrap()
            .define(enum_decl.type_name.clone(), reference_to_enum);
    }

    fn visit_object_declaration(&mut self, obj_decl: &parse_tree::ObjectDeclaration) {
        let mut local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(local_env));

        let mut obj = Object {
            parent: KindHash::clone(&self.current_module),
            name: obj_decl.type_name.clone(),
            type_arguments: Vec::new(),
            fields: HashMap::new(),
            methods: HashMap::new(),
            vtables: HashMap::new(),
        };

        let obj_kind_hash = obj.kind_hash(&self.kind_table);
        obj.type_arguments = self.generate_type_holes(&obj_kind_hash, &obj_decl.type_params);
        let obj_kind_hash = obj.kind_hash(&self.kind_table);

        obj.fields = obj_decl
            .fields
            .iter()
            .map(|tvd| {
                self.visit_expression(&tvd.type_reference);
                // type check that last_local is a type/kind
                let evaluated = self.stack.pop().unwrap().to_ref().unwrap();
                let evaluated_heap_ref = evaluated.to_heap_ref().unwrap();
                let type_ref = self.heap.load_type_reference(evaluated_heap_ref.address).unwrap();
                (tvd.name.clone(), type_ref)
            })
            .collect();
        
        self.kind_table.create(Kind::Object(Arc::new(RwLock::new(obj))));
        let reference_to_obj = Reference::create_type_reference(KindHash::clone(&obj_kind_hash), &mut self.heap);
        self.current_env.write().unwrap().define(String::from(SELF_TYPE_SYMBOL_NAME), Reference::clone(&reference_to_obj));

        let methods = obj_decl.methods
            .iter()
            .map(|fd| {
                self.stack.push(Value::Reference(Reference::clone(&reference_to_obj)));
                self.visit_function_declaration(fd);
                let func_ref = self.current_env.read().unwrap().get_reference_by_name(&fd.signature.name).unwrap();
                (fd.signature.name.clone(), Reference::clone(&func_ref))
            })
            .collect();

        let obj_arc = self.kind_table.load(&obj_kind_hash).unwrap().to_object().unwrap();
        obj_arc.write().unwrap().methods = methods;

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
        self.current_env
            .write()
            .unwrap()
            .define(obj_decl.type_name.clone(), reference_to_obj);
    }

    fn visit_contract_declaration(&mut self, contract_decl: &parse_tree::ContractDeclaration) {
        let mut local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(local_env));

        let mut contract = Contract {
            parent: KindHash::clone(&self.current_module),
            name: contract_decl.type_name.clone(),
            type_arguments: Vec::new(),
            required_functions: Vec::new(),
        };

        let contract_kind_hash = contract.kind_hash(&self.kind_table);

        contract.type_arguments = self.generate_type_holes(&contract_kind_hash, &contract_decl.type_params);

        self.kind_table.create(Kind::Contract(Arc::new(RwLock::new(contract))));

        let reference_to_contract = Reference::create_type_reference(KindHash::clone(&contract_kind_hash), &mut self.heap);

        let required_functions = contract_decl.functions
                .iter()
                .map(|fs| {
                    self.stack.push(Value::Reference(Reference::clone(&reference_to_contract)));
                    self.visit_function_signature(&fs);
                    let reference = self.stack.pop().unwrap().to_ref().unwrap();
                    KindHash::clone(&reference.to_heap_ref().unwrap().ty)
                })
                .collect();

        let contract = self.kind_table.load(&contract_kind_hash).unwrap().to_contract().unwrap();
        contract.write().unwrap().required_functions = required_functions;

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
        self.current_env
            .write()
            .unwrap()
            .define(contract_decl.type_name.clone(), reference_to_contract);
    }

    fn visit_implementation_declaration(&mut self, impl_decl: &parse_tree::ImplementationDeclaration) {
        let local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(local_env));

        self.visit_expression(&impl_decl.implementing_type);
        let implementor_type_ref = self.stack.pop().unwrap().to_ref().unwrap();
        let implementor_type_heap_ref = implementor_type_ref.to_heap_ref().unwrap();
        if implementor_type_heap_ref.ty != KindHash::from(KIND_KIND_HASH_STR) {
            panic!("implementor type must be a reference to a kind but got {}", implementor_type_heap_ref.ty);
        }
        let implementor_kind_hash = self.heap.load_type_reference(implementor_type_heap_ref.address).unwrap();

        self.visit_expression(&impl_decl.contract);
        let contract_type_ref = self.stack.pop().unwrap().to_ref().unwrap();
        let contract_type_heap_ref = contract_type_ref.to_heap_ref().unwrap();
        if contract_type_heap_ref.ty != KindHash::from(KIND_KIND_HASH_STR) {
            panic!("contract type type must be a reference to a kind but got {}", implementor_type_heap_ref.ty);
        }
        let contract_kind_hash = self.heap.load_type_reference(contract_type_heap_ref.address).unwrap();

        let vtable = VTable {
            implementor: KindHash::clone(&implementor_kind_hash),
            implementing: KindHash::clone(&contract_kind_hash),
            functions: HashMap::new(),
        };

        let vptr = self.vtables.new_vtable(vtable);

        {
            let implementor_type = self.kind_table.load(&implementor_kind_hash).unwrap();
            match implementor_type {
                Kind::Object(o_arc) => {
                    o_arc
                        .write()
                        .unwrap()
                        .vtables
                        .insert(KindHash::clone(&contract_kind_hash), vptr);
                },
                Kind::Enum(e_arc) => {
                    e_arc
                        .write()
                        .unwrap()
                        .vtables
                        .insert(KindHash::clone(&contract_kind_hash), vptr);
                },
                s => {
                    let message = format!("Expected implementing type to be an Object or Enum but got: {:?}", s);
                    panic!(message);
                },
            }
        }

        let contract = self.kind_table.load(&contract_kind_hash).unwrap();
        let required_functions_by_name: HashMap<String, Arc<RwLock<FunctionSignature>>> = if let Kind::Contract(c) = contract {
            c.read().unwrap().required_functions
                .iter()
                .map(|kind_hash| {
                    let req_func = self.kind_table.load(kind_hash).unwrap();
                    return if let Kind::FunctionSignature(fs_arc) = req_func {
                        (fs_arc.read().unwrap().name.clone(), Arc::clone(&fs_arc))
                    } else {
                        panic!("COMPILER BUG: Contract required function is not a FunctionSignature: {:?}", req_func);
                    }
                })
                .collect()
        } else {
            let message = format!("Expected Contract implementation to be a Contract but got {:?}", contract);
            panic!(message);
        };

        let implementing_functions_by_name: HashMap<String, Reference> = impl_decl.functions
            .iter()
            .map(|fd| {
                self.stack.push(Value::Reference(Reference::create_type_reference(KindHash::clone(&implementor_kind_hash), &mut self.heap)));
                self.visit_function_declaration(fd);
                let func_ref = self.current_env.read().unwrap().get_reference_by_name(&fd.signature.name).unwrap();
               
                // TODO -> need to define in env?
               
                (fd.signature.name.clone(), Reference::clone(&func_ref))
            })
            .collect();

        for (req_name, req_sig) in required_functions_by_name.iter() {
            if !implementing_functions_by_name.contains_key(req_name) {
                let message = format!("Contract implementation for {} by {} requires function {}",
                                      contract_kind_hash,
                                      implementor_kind_hash,
                                      req_sig.read().unwrap().kind_hash(&self.kind_table));
                panic!(message);
            }

            let implementor_func_ref = implementing_functions_by_name.get(req_name).unwrap();
            let implementor_func_heap_ref = implementor_func_ref.to_heap_ref().unwrap();
            let implementor_func = self.heap.load(implementor_func_heap_ref.address).to_function().unwrap();
            let implementor_sig = self.kind_table.load(&implementor_func.to_pel_function().unwrap().read().unwrap().signature).unwrap().to_func_sig().unwrap();

            if !req_sig.read().unwrap().is_compatible_with(implementor_sig.read().unwrap().deref(), &self.kind_table) {
                let message = format!("Function with name: {} has signature: {} but expected: {}",
                                      req_name,
                                      req_sig.read().unwrap().kind_hash(&self.kind_table),
                                      implementor_func.to_pel_function().unwrap().read().unwrap().signature);
                panic!(message);
            }
        }

        for (impl_name, func_val) in implementing_functions_by_name.iter() {
            if !required_functions_by_name.contains_key(impl_name) {
                let message = format!("Contract implementation for {} by {} contains unknown function: {}",
                                      contract_kind_hash,
                                      implementor_kind_hash,
                                      impl_name);
                panic!(message);
            } 
        }
    
        let vtable = self.vtables.load_vtable(vptr);
        vtable.write().unwrap().functions = implementing_functions_by_name;

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
    }

    fn visit_variant_declaration(&mut self, variant_decl: &parse_tree::VariantDeclaration) {
        panic!("unimplemented visit_variant_declaration");
    }

    fn visit_use_declaration(&mut self, use_decl: &parse_tree::UseDeclaration) {
        let mut mod_env = Arc::clone(&self.global_env);

        for mod_name in use_decl.import_chain.iter() {
            let ref_to_match = mod_env.read().unwrap().get_reference_by_name(mod_name);
            match ref_to_match {
                Some(Reference::HeapReference(hr)) => {
                    let item = self.heap.load(hr.address);
                    match item {
                        Item::ModuleReference(mr) => {
                            let module = self.kind_table.load(&mr).unwrap().to_module().unwrap();
                            let readable_mod = module.read().unwrap();
                            mod_env = Arc::clone(&readable_mod.env)
                        },
                        i => panic!("expected heap reference to module but got: {:?}", i)
                    }
                },
                Some(r) => panic!("expected heap reference to module but got: {:?}", r),
                None => {
                    self.load_module(&use_decl.import_chain);
                    self.visit_use_declaration(&use_decl);
                }
            }
        }

        self.current_env.write().unwrap().copy_from(mod_env.read().unwrap().deref());
    }

    fn visit_function_declaration(&mut self, func_decl: &parse_tree::FunctionDeclaration) {
        let name = &func_decl.signature.name;

        self.visit_function_signature(&func_decl.signature);
        let func_sig_ref = self.current_env
            .read()
            .unwrap()
            .get_reference_by_name(&func_decl.signature.name)
            .unwrap();

        let func_sig_heap_ref = func_sig_ref
            .to_heap_ref()
            .unwrap();

        let func_sig_kind_hash = self.heap.load_type_reference(func_sig_heap_ref.address).unwrap();
        let func_sig = self.kind_table.load(&func_sig_kind_hash).unwrap();

        let func = Function::PelFunction(Arc::new(RwLock::new(PelFunction {
            // TODO -> this should be aware of whether its in a module or an object/enum/contract
            // TODO -> this would be changing what calls this function to set the current module,
            // not this method explicitly
            parent: KindHash::clone(&self.current_module),
            signature: KindHash::clone(&func_sig_kind_hash),
            body: func_decl.body.clone(),
            // TODO -> this should be a deep clone i think
            environment: Arc::clone(&self.current_env),
        })));

        let addr = self.heap.alloc();
        self.heap.store(addr, Item::Function(func));
        let func_ref = Reference::HeapReference(HeapReference {
            // TODO -> need a concept of a function handle which is only based on its inputs and
            // outputs and not the name, context that its in. This is for lambdas, functions as
            // values, and checking whether functions satisfy parameter/return constraints
            ty: func_sig_kind_hash,
            is_self: false,
            address: addr,
            size: std::u32::MAX,
        });
        self.current_env
            .write()
            .unwrap()
            .define(name.clone(), func_ref);
    }

    fn visit_function_signature(&mut self, func_sig_syntax: &parse_tree::FunctionSignature) {
        let mut func_sig = FunctionSignature {
            parent: KindHash::clone(&self.current_module),
            name: func_sig_syntax.name.clone(),
            type_arguments: Vec::new(),
            parameters: Vec::new(),
            returns: None,
        };

        let func_sig_kind_hash = func_sig.kind_hash(&self.kind_table);
        func_sig.type_arguments = self.generate_type_holes(&func_sig_kind_hash, &func_sig_syntax.type_parameters);

        let mut parameters = Vec::new();
        for param in func_sig_syntax.parameters.iter() {
            let p = match param {
                parse_tree::FunctionParameter::SelfParam => {
                    let param_ty = self.stack.pop().unwrap().get_ty();
                    (SELF_VAR_SYMBOL_NAME.into(), param_ty)
                },
                parse_tree::FunctionParameter::TypedVariableDeclarationParam(tvd) => {
                    self.visit_expression(&tvd.type_reference);
                    let param_ty_ref = self.stack.pop().unwrap().to_ref().unwrap();
                    let param_ty_heap_ref = param_ty_ref.to_heap_ref().unwrap();
                    let param_ty = self.heap.load(param_ty_heap_ref.address).to_type_reference().unwrap();
                    (tvd.name.clone(), param_ty)
                }
            };

            parameters.push(p);
        }
        func_sig.parameters = parameters;

        func_sig.returns = match func_sig_syntax.returns {
            Some(ref expr) => {
                self.visit_expression(&expr);
                Some(self.stack.pop().unwrap().get_ty())
            },
            None => None,
        };

        let item = Item::TypeReference(func_sig.kind_hash(&self.kind_table));
        let addr = self.heap.alloc();
        self.heap.store(addr, item);
        let func_sig_ref = Reference::HeapReference(HeapReference {
            ty: func_sig.kind_hash(&self.kind_table),
            is_self: false,
            address: addr,
            size: std::u32::MAX,
        });

        self.kind_table.create(Kind::FunctionSignature(Arc::new(RwLock::new(func_sig))));
        self.current_env.write().unwrap().define(func_sig_syntax.name.clone(), func_sig_ref);
    }

    fn visit_typed_variable_declaration(&mut self, typed_var_decl: &parse_tree::TypedVariableDeclaration) {
        panic!("unimplemented typed_variable_declaration");
    }

    fn visit_block_body(&mut self, block_body: &parse_tree::BlockBody) {
        for stmt in block_body.statements.iter() {
            if self.exiting {
                self.exiting = false;
                return;
            }

            self.visit_statement(stmt);
        }

        self.exiting = false;
    }

    fn visit_statement(&mut self, statement: &parse_tree::Statement) {
        use parse_tree::Statement::*;

        match statement {
            VariableAssignmentNode(var_assignment) =>
                self.visit_variable_assignment(var_assignment),

            ReturnNode(return_node) =>
                self.visit_return(&return_node),

            ExpressionNode(expr_node) =>
                self.visit_expression(expr_node),
        }
    }

    fn visit_variable_assignment(&mut self, var_assignment: &parse_tree::VariableAssignment) {
        self.visit_expression(&var_assignment.target_type);
        let target_type_reference = self.stack.pop().unwrap().to_ref().unwrap();
        let target_type_heap_reference = target_type_reference.to_heap_ref().unwrap();

        self.visit_expression(&var_assignment.value);
        let value = self.stack.pop().unwrap();
        // TODO -> type check
        if value.get_ty() != target_type_heap_reference.ty {
            // TODO -> maybe set the contract_ty field of ObjectInstance or EnumInstance so we can
            // track whether this object is acting as a Contract?
        }

        let reference = self.store_at(&var_assignment.target, value);
    }

    fn visit_return(&mut self, return_stmt: &parse_tree::Return) {
        self.visit_expression(&return_stmt.value);
        self.exiting = true;
    }

    fn visit_expression(&mut self, expr: &parse_tree::Expression) {
        use crate::syntax::parse_tree::Expression::*;

        match expr {
            ChainableExpressionNode(cen) => self.visit_chainable_expression(cen),
            UnaryOperationNode(uo) => self.visit_unary_operation(uo),
            LambdaNode(lambda) => self.visit_lambda(lambda),
        }
    }

    fn visit_chainable_expression(&mut self, chainable_expr: &parse_tree::ChainableExpression) {
        use parse_tree::ExpressionChain::*;
        use parse_tree::ExpressionStart::*;

        match chainable_expr.start {
            ConditionalNode(ref cond_node) => self.visit_conditional(cond_node),
            MatchNode(ref match_node) => self.visit_match(match_node),
            LoopNode(ref loop_node) => self.visit_loop(loop_node),
            VariableNode(ref variable) => {
                let name = match variable {
                    parse_tree::Variable::Name(name) => name.clone(),
                    parse_tree::Variable::SelfVariable => "self".to_string(),
                    parse_tree::Variable::SelfType => "Self".to_string(),
                };

                let var_ref = match self.current_env.read().unwrap().get_reference_by_name(&name) {
                    Some(reference) => Reference::clone(&reference),
                    None => {
                        panic!("unknown symbol {:?}", name);
                    }
                };

                self.stack.push(Value::Reference(var_ref));
            }
            ValueNode(ref value) => {
                if let parse_tree::Value::StringValue(s) = value {
                    let str_ref = pel_utils::rust_string_to_pel_string(s, &mut self.heap);
                    self.stack.push(Value::Reference(str_ref));
                } else {
                    let scalar = Scalar::from(value);
                    let scalar_ty = scalar.get_ty();
                    self.stack.push(Value::Scalar(scalar));
                    let stack_ref = Reference::StackReference(StackReference {
                        ty: scalar_ty,
                        index: self.stack.len() - 1,
                        size: std::u32::MAX,
                    });
                    self.stack.push(Value::Reference(stack_ref));
                }
            },
            ArrayType(ref arr_ty) => {
                self.visit_expression(&arr_ty.ty);
                let arr = Arc::new(RwLock::new(Array {
                    ty: self.stack.pop().unwrap().get_ty(),
                }));
                self.kind_table.create(Kind::Array(Arc::clone(&arr)));
                let arr_kind_hash = arr.read().unwrap().kind_hash(&self.kind_table);
                let arr_ref = Reference::create_type_reference(arr_kind_hash, &mut self.heap);
                self.stack.push(Value::Reference(arr_ref));
            },
            ArrayInitialization(ref arr_init) => {
                unimplemented!("array initialization");
            }
        };


        for expr_chain in chainable_expr.chained.iter() {
            match expr_chain {
                FieldAccessNode(field_access)
                    => self.visit_field_access(field_access),

                ModuleAccessNode(mod_access)
                    => self.visit_module_access(mod_access),

                ArrayAccessNode(arr_access)
                    => self.visit_array_access(arr_access),

                ObjectInitializationNode(obj_init)
                    => self.visit_object_initialization(obj_init),

                FunctionApplicationNode(func_app)
                    => self.visit_function_application(func_app),

                TypeApplicationNode(type_app)
                    => self.visit_type_application(type_app),

                BinaryOperationNode(bin_op)
                    => self.visit_binary_operation(bin_op),
            };
        }
    }

    fn visit_conditional(&mut self, conditional: &parse_tree::Conditional) {
        for if_expr in conditional.if_exprs.iter() {
            self.visit_expression(&if_expr.condition);
            let cond_value = self.stack.pop().unwrap();

            let cond_scalar = match cond_value {
                Value::Scalar(ref s) => s,
                Value::Reference(r) => panic!("expected a boolean scalar but got reference of type: {}", r.get_ty()),
            };

            match cond_scalar.to_bool() {
                Some(b) => {
                    if b {
                        self.visit_block_body(&if_expr.body);
                        return;
                    }
                },
                None => {
                    let message = format!("expected a boolean scalar in if condition but got: {}", cond_value.get_ty());
                    panic!(message);
                },
            }
        }

        if let Some(else_body) = &conditional.else_expr {
            self.visit_block_body(else_body);
        }
    }

    fn visit_match(&mut self, match_node: &parse_tree::Match) {
        panic!("unimplemented visit_match");
    }

    fn visit_loop(&mut self, loop_node: &parse_tree::Loop) {
        let loop_local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(loop_local_env));

        self.visit_variable_assignment(&loop_node.init);
       
        let condition_checker = |interp: &mut Interpreter| {
            interp.visit_expression(&loop_node.condition);
            let cond_value = interp.stack.pop().unwrap();
            match cond_value.to_scalar().unwrap().to_bool() {
                Some(b) => b,
                None => {
                    let message = format!("expected a boolean value in loop condition but got: {}", cond_value.get_ty());
                    panic!(message);
                }
            }
        };

        while condition_checker(self) {
            self.visit_block_body(&loop_node.body);
            self.visit_statement(&loop_node.step);
        }

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
    }

    fn visit_field_access(&mut self, field_access: &parse_tree::FieldAccess) {
        let to_access_value = self.stack.pop().unwrap();
        let reference = match to_access_value {
            Value::Reference(Reference::HeapReference(r)) => r,
            Value::Scalar(_) => {
                let message = format!("trying to use field access syntax on a scalar or stack allocated value");
                panic!(message);
            },
            _ => unimplemented!("bugger"),
        };

        let item = self.heap.load(reference.address);

        match item {
            Item::ObjectInstance(oi_arc) => {
                let oi_readable = oi_arc.read().unwrap();
                let obj = self.kind_table.load(&oi_readable.ty).unwrap().to_object().unwrap();

                if reference.is_self {
                    if let Some(v) = oi_readable.fields.get(&field_access.field_name) {
                            self.stack.push(Value::clone(v));
                            return;
                    }

                    // we want to check methods here becuase a reference that is both self and
                    // has a contract_ty, it can still access regular methods
                    if let Some(func) = self.get_obj_method_with_name(obj.read().unwrap().deref(), &field_access.field_name) {
                        // TODO -> push func reference
                        // TODO -> push self reference
                    }
                }

                if let Some(ref contract_kind_hash) = oi_readable.contract_ty {
                    // this object instance is acting as a contract and is nto self, we can only
                    // access this contract's methods
                    let obj_readable = obj.read().unwrap();
                    let vtable_id = obj_readable.vtables.get(contract_kind_hash).unwrap();
                    let vtable = self.vtables.load_vtable(*vtable_id);
                }

                if let Some(func) = self.get_obj_method_with_name(obj.read().unwrap().deref(), &field_access.field_name) {
                        // TODO -> push func reference
                        // TODO -> push self reference
                }

                let message = format!("unknown method '{}' of object: {}.",
                                  field_access.field_name,
                                  oi_readable.ty);
                panic!(message);
            },
            Item::EnumInstance(ei_arc) => {
                // TODO ->
                // find a method with name: field_access
            },
            _ => {
                let message = format!("trying to use dot access syntax on something that can't be accessed: {:?}", item);
                panic!(message);
            }
        }
    }

    fn visit_module_access(&mut self, mod_access: &parse_tree::ModuleAccess) {
        let to_access_value = self.stack.pop().unwrap();
        let reference = match to_access_value {
            Value::Reference(Reference::HeapReference(ref r)) => r,
            _ => {
                panic!("trying to use field access syntax on a scalar or stack allocated value");
            }
        };
        let item = self.heap.load(reference.address);
        match item {
            Item::ModuleReference(ref mod_hash) => {
                unimplemented!("module access: {}", mod_hash);
            },
            Item::TypeReference(ref kind_hash) => {
                let kind = self.kind_table.load(&kind_hash).unwrap();
                if let Some(enu) = kind.to_enum() {
                    match enu.read().unwrap().variant_values.get(&mod_access.name) {
                        Some(reference)  => {
                            match self.heap.load(reference.to_heap_ref().unwrap().address) {
                                Item::EnumInstance(ei_arc) => {
                                    let addr = self.heap.alloc();
                                    let ei_readable = ei_arc.read().unwrap();
                                    let new_instance = EnumInstance::clone(ei_readable.deref());
                                    let new_item = Item::EnumInstance(Arc::new(RwLock::new(new_instance)));
                                    self.heap.store(addr, new_item);
                                },
                                Item::Function(ref func) => {
                                    self.stack.push(Value::clone(&to_access_value));
                                    let var_name = pel_utils::rust_string_to_pel_string(&mod_access.name, &mut self.heap);
                                    self.stack.push(Value::Reference(var_name));

                                    // third arg is whether it contains a type
                                    let enu_readable = enu.read().unwrap();
                                    let variant_ty = enu_readable.variant_tys.get(&mod_access.name).unwrap();
                                    let does_contain_ty = variant_ty.is_some();
                                    let third_arg = Value::Scalar(Scalar::Boolean(does_contain_ty));
                                    self.stack.push(third_arg);
                                  
                                    if does_contain_ty {
                                        // fourth arg should be kind_hash it contains
                                        let variant_kind_hash_value = KindHash::from("unimplemented");
                                        let type_ref = Reference::create_type_reference(variant_kind_hash_value, &mut self.heap);
                                        self.stack.push(Value::Reference(type_ref));

                                        // fifth arg gets pushed as part of the function application

                                        // push num args (4)
                                        // push 4
                                    } else {
                                        // push num args (3)
                                        // push 3
                                    }
                                },
                                _ => unreachable!(),
                            }
                        },
                        None => {
                            let message = format!("Enum {} has no variant {}",
                                          kind_hash,
                                          mod_access.name);
                            panic!(message);
                        }
                    }
                } else {
                    panic!("trying to use module access syntax on something that isn't a module: {:?}", item);
                }
            },
            _ => panic!("trying to use module access syntax on something that isn't a module: {:?}", item),
        }
    }

    fn visit_array_access(&mut self, arr_acces: &parse_tree::ArrayAccess) {
        unimplemented!("visit array access");
    }

    fn visit_object_initialization(&mut self, obj_init: &parse_tree::ObjectInitialization) {
        let value = self.stack.pop().unwrap();
        let reference = value.to_ref().unwrap();
        let heap_ref = reference.to_heap_ref().unwrap();
        let item = self.heap.load(heap_ref.address);
        let obj_hash = item.to_type_reference().unwrap();
        let obj = self.kind_table.load(&obj_hash).unwrap().to_object().unwrap();

        // TODO -> need to validate that the object init doesn't have any fields that aren't in the
        // object

        let mut field_values = HashMap::new();
        for (field_name, expected_ty) in obj.read().unwrap().fields.iter() {
            if !obj_init.fields.contains_key(field_name) {
                let message = format!(
                    "expected field '{}' in object initialization not present in {:?}",
                    field_name,
                    obj_init.fields.keys()
                );
                panic!(message);
            }

            // TODO -> need to resolve type variables to their actual type?

            let field_expr = obj_init.fields.get(field_name).unwrap();
            self.visit_expression(field_expr);
            let field_value = self.stack.pop().unwrap();
            if &field_value.get_ty() != expected_ty {
                let message = format!(
                    "expected field '{}' in object initialization to be type '{}' but got '{}'",
                    field_name,
                    expected_ty,
                    field_value.get_ty(),
                );
                panic!(message);
            }

            field_values.insert(field_name.clone(), field_value);
        }

        let obj_instance = ObjectInstance {
            ty: KindHash::clone(&obj_hash),
            contract_ty: None,
            fields: field_values,
        };

        let address = self.heap.alloc();
        self.heap.store(address, Item::ObjectInstance(Arc::new(RwLock::new(obj_instance))));
        let reference = Reference::HeapReference(HeapReference {
            ty: obj_hash,
            is_self: false,
            address,
            size: std::u32::MAX,
        });
        let value = Value::Reference(reference);
        self.stack.push(value);
    }

    fn visit_function_application(&mut self, func_app: &parse_tree::FunctionApplication) {
        let to_apply_to_value = self.stack.pop().unwrap();
        let reference = match to_apply_to_value {
            Value::Reference(Reference::HeapReference(reference)) => reference,
            v => {
                let message = format!("expected to apply to a heap reference but got: {:?}", v);
                panic!(message)
            }
        };

        let to_apply_to_item = self.heap.load(reference.address);

        let func = match to_apply_to_item.to_function() {
            Some(f) => f,
            None => {
                let message = format!("trying to call something that isn't a function: {:?}", to_apply_to_item);
                panic!(message);
            }
        };

        let args = func_app.args
            .iter()
            .map(|expr| {
                self.visit_expression(expr);
                let top_of_stack = self.stack.pop().unwrap();
                top_of_stack.to_ref().unwrap()
            })
            .collect();

        // TODO -> type_check args?
        
        let result = func.call(self, args);

        let func_sig = self.kind_table.load(&func.signature()).unwrap().to_func_sig().unwrap();
        let func_sig_readable = func_sig.read().unwrap();
        if let Some(ref _ret) = func_sig_readable.returns {
            // TODO -> type check
            self.stack.push(Value::Reference(result.unwrap()))
        }
    }

    fn visit_type_application(&mut self, type_app: &parse_tree::TypeApplication) {
        let to_apply_to_ref = self.stack.pop().unwrap().to_ref().unwrap();
        let to_apply_to_heap_ref = to_apply_to_ref.to_heap_ref().unwrap();

        let mut item = self.heap.load(to_apply_to_heap_ref.address);
        let mut maybe_func = None;

        let type_ref = match item {
            Item::Function(Function::PelFunction(pf)) => {
                maybe_func = Some(Function::PelFunction(Arc::clone(&pf)));
                KindHash::clone(&pf.read().unwrap().signature)
            },
            Item::Function(Function::NativeFunction(nf)) => {
                maybe_func = Some(Function::NativeFunction(Arc::clone(&nf)));
                KindHash::clone(&nf.read().unwrap().signature)
            },
            Item::TypeReference(kh) => {
                KindHash::clone(&kh)
            },
            i => panic!("expected type refernence but got {:?}", i),
        };

        let to_apply_to = self.kind_table.load(&type_ref).unwrap();

        // can only apply to functions, objects, enums, ???

        // TODO -> check num type params versus num type args
        match to_apply_to {
            Kind::Object(ref o_arc) => {
                if o_arc.read().unwrap().type_arguments.len() != type_app.args.len() {
                    let message = format!("invalid number of type parameters in type application. expected {}, got {}", o_arc.read().unwrap().type_arguments.len(), type_app.args.len());
                    panic!(message);
                }
            },
            Kind::Enum(ref e_arc) => {
                if e_arc.read().unwrap().type_arguments.len() != type_app.args.len() {
                    let message = format!("invalid number of type parameters in type application. expected {}, got {}", e_arc.read().unwrap().type_arguments.len(), type_app.args.len());
                    panic!(message);
                }
            },
            Kind::FunctionSignature(ref fs_arc) => {
                if fs_arc.read().unwrap().type_arguments.len() != type_app.args.len() {
                    let message = format!("invalid number of type parameters in type application. expected {}, got {}", fs_arc.read().unwrap().type_arguments.len(), type_app.args.len());
                    panic!(message);
                }
            },
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        }
        
        let mut types = Vec::new();
        for arg in type_app.args.iter() {
            self.visit_expression(arg);
            let evaluated_ref = self.stack.pop().unwrap().to_ref().unwrap();
            let evaluated_heap_ref = evaluated_ref.to_heap_ref().unwrap();
            let evaluated_item = self.heap.load(evaluated_heap_ref.address).to_type_reference().unwrap();

            // TODO -> type check that results are of type: Type

            types.push(evaluated_item);
        }

        // NOTE: This is purposefully actually cloning the underlying type as we are creating
        // a new with one applied types
        // TODO -> merge this with below and clean up, reduce duplication
        let mut new_type = match to_apply_to {
            Kind::Object(ref obj) => Kind::Object(Arc::new(RwLock::new(Object::clone(obj.read().unwrap().deref())))),
            Kind::Enum(ref enu)   => Kind::Enum(Arc::new(RwLock::new(Enum::clone(enu.read().unwrap().deref())))),
            Kind::FunctionSignature(ref fs)   => Kind::FunctionSignature(Arc::new(RwLock::new(FunctionSignature::clone(fs.read().unwrap().deref())))),
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        };

        // adjust fields and types 
        match new_type {
            Kind::Object(ref mut o) => {
                let mut new_type_args = Vec::new();
                let mut new_fields = HashMap::new();
                
                {
                    let o_readable = o.read().unwrap();
                    
                    let zipped_type_args_with_applied_type = o_readable
                        .type_arguments
                        .iter()
                        .zip(types.into_iter());

                    for ((ref type_name, ref type_hole_kind_hash), ref type_to_apply) in zipped_type_args_with_applied_type {
                        new_type_args.push((String::clone(type_name), KindHash::clone(type_to_apply)));

                        for (field_name, field_kind_hash) in o.read().unwrap().fields.iter() {
                            if field_kind_hash == type_hole_kind_hash {
                                new_fields.insert(field_name.clone(), KindHash::clone(type_to_apply));
                            }
                        }
                    }
                }

                o.write().unwrap().type_arguments = new_type_args;
                o.write().unwrap().fields = new_fields;
            },
            Kind::Enum(ref mut e) => {
                // TODO -> this is copied and pasted from above. need to find where to put this logic
                let mut new_type_args = Vec::new();
                let mut new_variants = HashMap::new();
                
                {
                    let e_readable = e.read().unwrap();

                    let zipped_type_args_with_applied_type = e_readable
                        .type_arguments
                        .iter()
                        .zip(types.into_iter());

                    for ((ref type_name, ref type_hole_kind_hash), ref type_to_apply) in zipped_type_args_with_applied_type {
                        new_type_args.push((String::clone(type_name), KindHash::clone(type_to_apply)));

                        for (variant_name, maybe_variant_kind_hash) in e.read().unwrap().variant_tys.iter() {
                            if let Some(variant_kind_hash) = maybe_variant_kind_hash {
                                if variant_kind_hash == type_hole_kind_hash {
                                    new_variants.insert(variant_name.clone(), Some(KindHash::clone(type_to_apply)));
                                }
                            } else {
                                new_variants.insert(variant_name.clone(), None);
                            }
                        }
                    }
                }

                e.write().unwrap().type_arguments = new_type_args;
                e.write().unwrap().variant_tys = new_variants;
            },
            Kind::FunctionSignature(ref mut fs) => {
                let mut new_type_args = Vec::new();
                let mut new_parameters = Vec::new();

                {
                    let fs_readable = fs.read().unwrap();

                    let zipped_type_args_with_applied_type = fs_readable
                        .type_arguments
                        .iter()
                        .zip(types.into_iter());

                    for ((ref type_name, ref type_hole_kind_hash), ref type_to_apply) in zipped_type_args_with_applied_type {
                        new_type_args.push((String::clone(type_name), KindHash::clone(type_to_apply)));

                        for (param_name, param_kind_hash) in fs.read().unwrap().parameters.iter() {
                            if param_kind_hash == type_hole_kind_hash {
                                new_parameters.push((param_name.clone(), KindHash::clone(type_to_apply)));
                            }
                        }
                    }
                }

                println!("NEW_TYPE_ARGS: {:?}", new_type_args);
                println!("NEW_PARAMETERS: {:?}", new_parameters);
                fs.write().unwrap().type_arguments = new_type_args;
                fs.write().unwrap().parameters = new_parameters;
            },
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        }

        // we need to check if the equivalent type already exists
        println!("NEW_TYPE: {:?}", new_type);
        let new_type_kind_hash = new_type.kind_hash(&self.kind_table);
        println!("NEW_TYPE_KIND_HASH: {:?}", new_type_kind_hash);
        self.kind_table.create(new_type);

        let new_item = if let Some(ref mut func) = maybe_func {
            match func {
                Function::PelFunction(pf_arc) => {
                    let mut new_func = PelFunction::clone(pf_arc.read().unwrap().deref());
                    new_func.signature = KindHash::clone(&new_type_kind_hash);
                    Item::Function(Function::PelFunction(Arc::new(RwLock::new(new_func))))
                },
                Function::NativeFunction(nf_arc) => {
                    let mut new_func = NativeFunction::clone(nf_arc.read().unwrap().deref());
                    new_func.signature = KindHash::clone(&new_type_kind_hash);
                    Item::Function(Function::NativeFunction(Arc::new(RwLock::new(new_func))))
                },
                f => panic!("unknown function type: {:?}", f)
            }
        } else {
            Item::TypeReference(KindHash::clone(&new_type_kind_hash))
        };

        println!("NEW ITEM: {:?}", new_item);
        
        let addr = self.heap.alloc();
        self.heap.store(addr, new_item);
        let val = Value::Reference(Reference::HeapReference(HeapReference {
            ty: new_type_kind_hash,
            is_self: false,
            address: addr,
            size: std::u32::MAX,
        }));
        self.stack.push(val);
    }

    fn visit_unary_operation(&mut self, _unary_op: &parse_tree::UnaryOperation) {
        panic!("unimplemented visit_unary_operation");
    }

    fn visit_lambda(&mut self, _lambda: &parse_tree::Lambda) {
        panic!("unimplemented visit_lambda");
    }

    fn visit_binary_operation(&mut self, bin_op: &parse_tree::BinaryOperation) {
        let top_of_stack = self.stack.pop().unwrap();
        let lhs_ref = top_of_stack.to_ref().expect(&format!("expected lhs of binary operation to be a stack reference but was {:?}", top_of_stack));
        let lhs_stack_ref = lhs_ref.to_stack_ref().expect(&format!("expected lhs of binary operation to be a stack reference but was {:?}", lhs_ref));
        let lhs = self.stack[lhs_stack_ref.index].to_scalar().expect(&format!("expected stack reference to point to scalar but was: {:?}", self.stack[lhs_stack_ref.index]));
        
        self.visit_expression(&bin_op.rhs);
        let top_of_stack = self.stack.pop().unwrap();
        let rhs_ref = top_of_stack.to_ref().expect(&format!("expected rhs of binary operation to be a stack reference but was {:?}", top_of_stack));
        let rhs_stack_ref = rhs_ref.to_stack_ref().expect(&format!("expected rhs of binary operation to be a stack reference but was {:?}", rhs_ref));
        let rhs = self.stack[rhs_stack_ref.index].to_scalar().expect(&format!("expected stack reference to point to scalar but was: {:?}", self.stack[rhs_stack_ref.index]));

        let result = match &bin_op.op {
            parse_tree::BinaryOperator::Plus =>               Scalar::add(lhs, rhs),
            parse_tree::BinaryOperator::Minus =>              Scalar::subtract(lhs, rhs),
            parse_tree::BinaryOperator::Multiply =>           Scalar::multiply(lhs, rhs),
            parse_tree::BinaryOperator::Divide =>             Scalar::divide(lhs, rhs),
            parse_tree::BinaryOperator::LessThan =>           Scalar::less_than(lhs, rhs),
            parse_tree::BinaryOperator::LessThanOrEqual =>    Scalar::less_than_or_equal(lhs, rhs),
            parse_tree::BinaryOperator::GreaterThan =>        Scalar::greater_than(lhs, rhs),
            parse_tree::BinaryOperator::GreaterThanOrEqual => Scalar::greater_than_or_equal(lhs, rhs),
            parse_tree::BinaryOperator::Equal =>              Scalar::equal_to(lhs, rhs),
            parse_tree::BinaryOperator::NotEqual =>           Scalar::not_equal_to(lhs, rhs),
            parse_tree::BinaryOperator::Or =>                 Scalar::or(lhs, rhs),
            parse_tree::BinaryOperator::And =>                Scalar::and(lhs, rhs),
        };

        let scalar_ty = result.get_ty();
        self.stack.push(Value::Scalar(result));
        let stack_ref = Reference::StackReference(StackReference {
            ty: scalar_ty,
            index: self.stack.len() - 1,
            size: std::u32::MAX,
        });
        self.stack.push(Value::Reference(stack_ref));
    }
}

impl Callable<Interpreter> for Object {
    fn call(&self, _interpreter: &mut Interpreter, args: Vec<Reference>) -> Option<Reference> {
        panic!("unimplemented");
    }
}

impl Callable<Interpreter> for Enum {
    fn call(&self, _interpreter: &mut Interpreter, args: Vec<Reference>) -> Option<Reference> {
        panic!("unimplemented");
    }
}

impl Callable<Interpreter> for Function {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Reference>) -> Option<Reference> {
        match self {
            Function::NativeFunction(nf_arc) => nf_arc.read().unwrap().call(interpreter, args),
            Function::PelFunction(pf_arc)    => pf_arc.read().unwrap().call(interpreter, args),
        }
    }
}

impl Callable<Interpreter> for PelFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Reference>) -> Option<Reference> {
        let signature = interpreter.kind_table
            .load(&self.signature)
            .unwrap()
            .to_func_sig()
            .unwrap();
        
        if signature.read().unwrap().parameters.len() != args.len() {
            let message = format!(
                "invalid number of parameters in application of function: {}. expected {}, got {}",
                self.signature,
                signature.read().unwrap().parameters.len(),
                args.len()
            );
            panic!(message);
        }

        let mut func_app_local_env = Environment::from_parent(&self.environment);
        for ((ref n, ref param_kind), ref arg_ref) in signature.read().unwrap().parameters.iter().zip(args.into_iter()) {
            if &arg_ref.get_ty() != param_kind {
                panic!("expected argument with type: {}, got: {}",
                       param_kind,
                       arg_ref.get_ty());
            }

            func_app_local_env.define(n.clone(), Reference::clone(arg_ref));
        }

        let current_env = Arc::clone(&interpreter.current_env);
        interpreter.set_current_env(func_app_local_env);
        interpreter.visit_block_body(&self.body);
        interpreter.current_env = current_env;
       
        let sig_readable = signature.read().unwrap();
        if let Some(ref _type_sym) = sig_readable.returns {
            // TODO -> type check return based on the expected `type_sym`
            Some(interpreter.stack.pop().unwrap().to_ref().unwrap())
        } else {
            None
        }
    }
}

impl Callable<Interpreter> for NativeFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Reference>) -> Option<Reference> {
        (self.func)(interpreter, args)
    }
}

