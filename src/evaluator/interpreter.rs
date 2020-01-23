use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock};

use crate::evaluator::evaluator::{
    Callable,
    Contract,
    Enum,
    EnumInstance,
    Environment,
    Evaluator,
    Function,
    FunctionSignature,
    NativeFunction,
    Object,
    ObjectInstance,
    VTables,
    VTable,
    KindTable,
    Kind,
    Value,
    Scalar,
    Reference,
    Item,
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
    pub kind_table: KindTable,
    pub vtables: VTables,
    global_env: Arc<RwLock<Environment>>,
    current_env: Arc<RwLock<Environment>>,
    pub stack: Vec<Value>,
    exiting: bool,
    errors: Vec<String>,
}

const SELF_VAR_SYMBOL_NAME: &str = "self";
const SELF_TYPE_SYMBOL_NAME: &str = "Self";

impl Interpreter {
    pub fn new() -> Self {
        let mut kind_table = KindTable::new();
        let main_mod_kind = kind_table.load_kind(KindTable::MAIN_MODULE_SYMBOL_ID);
        let main_mod_sym_readable = main_mod_kind.read().unwrap();
        let main_mod = match main_mod_sym_readable.deref() {
            Kind::Module(m) => m,
            _ => unreachable!(),
        };

        let root_env = Arc::clone(&main_mod.env);

        Self {
            kind_table,
            vtables: VTables::new(),
            global_env: Arc::clone(&root_env),
            current_env: root_env,
            stack: Vec::new(),
            exiting: false,
            errors: Vec::new(),
        }
    }

    pub fn emit_error(&mut self, error: String) {
        self.errors.push(error);
    }

    fn var_name_to_define(&mut self, assignment_node: &VariableAssignment) -> Option<String> {
        let ce = if let Expression::ChainableExpressionNode(ce) = &assignment_node.target {
            ce
        } else {
            return None;
        };

        if !ce.chained.is_empty() {
            return None;
        }

        let var = if let ExpressionStart::VariableNode(var) = &ce.start {
            var
        } else {
            return None;
        };

        let var_name = match var {
            Variable::Name(n) => n.clone(),
            Variable::SelfVariable => SELF_VAR_SYMBOL_NAME.into(),
            Variable::SelfType => SELF_TYPE_SYMBOL_NAME.into(),
        };

        if let None = self.current_env.read().unwrap().get_symbol_by_name(&var_name) {
            Some(var_name) 
        } else {
            None
        }
    }

    pub fn set_current_env(&mut self, env: Environment) {
        self.current_env = Arc::new(RwLock::new(env));
    }

    fn get_obj_method_with_name(&self, obj: &Object, name: &String) -> Option<Value> {
        if let Some(func_hash) = obj.methods.get(name) {
            // create Reference to func_hash
            // return Reference
        }

        for (_, vtable_id) in obj.vtables.iter() {
            let vtable = self.vtables.load_vtable(*vtable_id);
            let vtable_readable = vtable.read().unwrap();

            if let Some(func_hash) = vtable_readable.functions.get(name) {
                // create Reference to func_hash
                // return Reference
            }
        }
    }

    fn generate_type_holes(&mut self, type_params: &Vec<String>) -> Vec<> {
        type_params
            .iter()
            .map(|type_param_name| {
                let type_hole_name = "___PEL___TYPE_HOLE___".into();
                // let addr = self.heap.alloc();
                // let item = Item::TypeReference(type_hole_name);
                let value = Value::Reference(
                    Reference {
                        ty: type_hole_name,
                        is_self: false,
                        address: std::u32::MAX,
                        size: 0,
                    }
                );
                self.current_env.write().unwrap().define(type_param_name.clone(), value);
                (type_param_name.clone(), type_hole_name)
            })
            .collect()
    }


}

impl Evaluator for Interpreter {
    fn visit_program(&mut self, program: &Program) {
        for decl in program.declarations.iter() {
            self.visit_declaration(decl);
        }
    }

    fn visit_declaration(&mut self, decl: &Declaration) {
        match decl {
            Declaration::EnumDeclarationNode(enum_decl)
                => self.visit_enum_declaration(enum_decl),

            Declaration::ContractDeclarationNode(contract_decl)
                => self.visit_contract_declaration(contract_decl),

            Declaration::ImplementationDeclarationNode(impl_decl)
                => self.visit_implementation_declaration(impl_decl),

            Declaration::ObjectDeclarationNode(obj_decl)
                => self.visit_object_declaration(obj_decl),

            Declaration::FunctionDeclarationNode(func_decl)
                => self.visit_function_declaration(func_decl),
        }
    }

    fn visit_enum_declaration(&mut self, enum_decl: &EnumDeclaration) {
        let mut local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(local_env));

        let type_arguments = self.generate_type_holes(&enum_decl.type_params);

        let variant_tys = enum_decl.variants
            .iter()
            .map(|vd| {
                let contains = if let Some(expr) = &vd.contains {
                    // type check that last_local is a type/kind
                    self.visit_expression(&expr);
                    let val = self.stack.pop().unwrap();
                    match val {
                        Value::Reference(r) => {
                            if r.ty != KIND_KIND_HASH {
                                panic!("reference must be pointing to a kind but is actually: {}", r.ty);
                            }
                            Some(self.heap.load_type_reference(r.address).unwrap())
                        },
                        v => {
                            panic!("variant type must be a kind but got {}", v);
                        }
                    }
                } else {
                    None
                };

                (vd.name.clone(), contains)
            })
            .collect();

        let mut enu = Enum {
            parent: KindTable::MAIN_MODULE_SYMBOL_ID,
            name: enum_decl.type_name.clone(),
            type_arguments,
            variant_tys,
            variant_values: HashMap::new(),
            methods: HashMap::new(),
            vtables: HashMap::new(),
        };

        let enu_kind_hash = enu.kind_hash();
        self.kind_table.insert(Kind::Enum(enu));

        let variant_values = enum_decl.variants
            .iter()
            .map(|vd| {
                if vd.contains.is_none() {
                    let initialized_enum = EnumInstance {
                        ty: KindHash::clone(enu_kind_hash),
                        contract_ty: None,
                        variant: (vd.name.clone(), None),
                    };

                    (vd.name.clone(), Item::EnumInstance(initialized_enum))
                } else {
                    let enum_constructor = |interp: &mut Interpreter, args: Vec<Value>| {
                        let enu_kind_hash = self.heap.load(args[0].to_ref().unwrap().address).string_value();

                        let variant_name_item = self.heap.load(args[1].to_ref().unwrap().address);
                        let variant_name = variant_name_item.string_value();

                        let does_contain_ty = args[2].to_scalar().unwrap().to_bool().unwrap();

                        let contains = if does_contain_ty {
                            let contains_kind_hash = self.heap.load(args[3].to_ref().unwrap().address);

                            let contains_ref = args[4].to_ref().unwrap();

                            if contains_ref.ty != contains_kind_hash {
                                let message = format!("expected enum variant: {} to contain type {}, but got {}",
                                                      variant_name,
                                                      contains_kind_hash,
                                                      contains_ref.ty);
                                panic!(message);
                            }

                            Some(Reference::clone(contains_ref))
                        } else {
                            None
                        };
                        
                        let enum_instance = EnumInstance {
                            ty: enu_kind_hash,
                            contract_ty: None,
                            variant: (variant_name, contains),
                        };

                        let addr = self.heap.alloc();
                        let enum_item = Item::EnumInstance(Arc::new(RwLock::new(enum_instance)));
                        self.heap.store(addr, enum_item);

                        let reference = Reference {
                            ty: enu_kind_hash,
                            is_self: false,
                            address: addr,
                            size: std::u32::MAX,
                        };
                        self.stack.push(Value::Reference(reference));
                    };

                    let nat_func = NativeFunction {
                        name: "".into(),
                        args: Vec::new(),
                        func: enum_constructor,
                    };

                    (vd.name.clone(), Kind::NativeFunction(nat_func))
                }
            })
            .collect();

        let methods = enum_decl
            .methods
            .iter()
            .map(|fd| {
                self.last_local = Some(enu_sym_id);
                self.visit_function_declaration(fd);
                let method_id = self.last_local.take().unwrap();
                (fd.signature.name.clone(), method_id)
            })
            .collect();

        let enu = self.kind_table.load(enu_kind_hash);
        let enu_writable = enu.write().unwrap();
        enu_writable.variant_values = variant_values;
        enu_writable.methods = methods;

        let item = Item::TypeReference(KindHash::clone(enu_kind_hash));
        let addr = self.heap.alloc();
        self.heap.store(addr, item);
        
        let val = Value::Reference(Reference {
            ty: KIND_KIND_HASH,
            is_self: false,
            address: addr,
            size: std::u32::MAX,
        });

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
        self.current_env
            .write()
            .unwrap()
            .define(enum_decl.type_name.clone(), val);
    }

    fn visit_object_declaration(&mut self, obj_decl: &ObjectDeclaration) {
        let mut local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(local_env));

        let type_arguments = self.generate_type_holes(&obj_decl.type_params);

        let fields = obj_decl
            .fields
            .iter()
            .map(|tvd| {
                self.visit_expression(&tvd.type_reference);
                // type check that last_local is a type/kind
                let evaluated = self.last_local.take().unwrap();
                (tvd.name.clone(), evaluated)
            })
            .collect();

        let obj = Object {
            parent: Reference::clone(main_module_reference),
            name: obj_decl.type_name.clone(),
            type_arguments,
            fields,
            methods: HashMap::new(),
            vtables: HashMap::new(),
        };

        let obj_kind_hash = obj.kind_hash();
        self.kind_table.insert(Kind::Object(obj));

        let methods = obj_decl.methods
            .iter()
            .map(|fd| {
                self.stack.push(KindHash::clone(obj_kind_hash));
                self.visit_function_declaration(fd);
                let method_id = self.last_local.take().unwrap();
                (fd.signature.name.clone(), method_id)
            })
            .collect();

        let obj_arc = self.kind_table.load_object(obj_kind_hash);

        let mut obj_writable = obj_arc.write().unwrap();
        if let Kind::Object(ref mut o) = obj_writable.deref_mut() {
            o.methods = methods;
        }

        let item = Item::TypeReference(obj.kind_hash());
        let address = self.heap.alloc();
        self.heap.store(address, item);
        let reference_to_obj = Value::Reference(Reference {
            ty: KIND_KIND_HASH,
            is_self: false,
            address,
            size: std::u32::MAX,
        });

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
        self.current_env
            .write()
            .unwrap()
            .define(obj_decl.type_name.clone(), reference_to_obj);
    }

    fn visit_contract_declaration(&mut self, contract_decl: &ContractDeclaration) {
        let mut local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(local_env));

        let type_arguments = self.generate_type_holes(&contract_decl.type_params);

        let contract = Contract {
            parent: KindTable::MAIN_MODULE_SYMBOL_ID,
            name: contract_decl.type_name.clone(),
            type_arguments,
            required_functions: Vec::new(),
        };


        let contract_kind_hash = contract.kind_hash();
        self.kind_table.insert(Kind::Contract(obj));

        let required_functions = contract_decl.functions
                .iter()
                .map(|fs| {
                    self.stack.push(KindHash::clone(contract_kind_hash));
                    self.visit_function_signature(&fs);
                    self.stack.pop().unwrap()
                })
                .collect();

        let contract_arc = self.kind_table.load(contract_kind_hash);
        let mut contract = contract_arc.write().unwrap();
        if let Kind::Contract(ref mut c) = contract.deref_mut() {
            c.required_functions = required_functions;
        }

        let item = Item::TypeReference(contract.kind_hash());
        let address = self.heap.alloc();
        self.heap.store(address, item);
        let reference_to_contract = Value::Reference(Reference {
            ty: KIND_KIND_HASH,
            is_self: false,
            address,
            size: std::u32::MAX,
        });

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
        self.current_env
            .write()
            .unwrap()
            .define(contract_decl.type_name.clone(), reference_to_contract);
    }

    fn visit_implementation_declaration(&mut self, impl_decl: &ImplementationDeclaration) {
        let local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(local_env));

        self.visit_expression(&impl_decl.implementing_type);
        let implementor_type_ref = self.stack.pop().unwrap().to_ref();
        if implementor_type_ref.ty != KIND_KIND_HASH {
            panic!("implementor type must be a reference to a kind but got {}", implementor_type_ref.ty);
        }
        let implementor_kind_hash = self.heap.load_type_reference(implementor_type_ref.address).unwrap();

        self.visit_expression(&impl_decl.contract);
        let contract_type_ref = self.stack.pop().unwrap().to_ref();
        if contract_type_ref.ty != KIND_KIND_HASH {
            panic!("contract type type must be a reference to a kind but got {}", implementor_type_ref.ty);
        }
        let contract_kind_hash = self.heap.load_type_reference(contract_type_ref.address).unwrap();

        let vtable = VTable {
            implementor: KindHash::clone(implementor_kind_hash),
            implementing: KindHash::clone(contract_kind_hash),
            functions: HashMap::new(),
        };

        let vptr = self.vtables.new_vtable(vtable);

        {
            let implementor_type = self.kind_table.load(implementor_kind_hash);
            let mut implementor_type_writable = implementor_type.write().unwrap();
            match implementor_type_writable.deref_mut() {
                Kind::Object(o) => {
                    o.vtables.insert(KindHash::clone(contract_kind_hash), vptr);
                },
                Kind::Enum(e) => {
                    e.vtables.insert(KindHash::clone(contract_kind_hash), vptr);
                },
                s => {
                    let message = format!("Expected implementing type to be an Object or Enum but got: {}", s);
                    panic!(message);
                },
            }
        }

        let contract = self.kind_table.load(contract_kind_hash);
        let required_functions_by_name: HashMap<String, Arc<RwLock<FunctionSignature>>> = if let Kind::Contract(c) = contract.read().unwrap().deref() {
            c.required_functions
                .iter()
                .map(|kind_hash| {
                    let req_func = self.kind_table.load(kind_hash);
                    let req_func_readable = req_func.read().unwrap();
                    return if let Kind::FunctionSignature(fs) = req_func_readable.deref() {
                        (fs.name.clone(), Arc::clone(req_func))
                    } else {
                        let message = format!("COMPILER BUG: Contract required function is not a FunctionSignature: {:?}", req_func_readable);
                        panic!(message);
                    }
                })
                .collect()
        } else {
            let message = format!("Expected Contract implementation to be a Contract but got {}", contract);
            panic!(message);
        };

        // LEFT OFF HERE
        let implementing_functions_by_name: HashMap<String, Value> = impl_decl.functions
            .iter()
            .map(|fd| {
                self.last_local = Some(implementor_type_sym_id);
                self.visit_function_declaration(fd);
                let sym_id = self.last_local.take().unwrap();
                let impl_func_sym = self.symbol_table.load_symbol(sym_id);
                let impl_func_sym_readable = impl_func_sym.read().unwrap();
                return if let Kind::Function(f) = impl_func_sym_readable.deref() {
                    let impl_sig_sym = self.symbol_table.load_symbol(f.signature);
                    let impl_sig_sym_readable = impl_sig_sym.read().unwrap();
                    let impl_sig = if let Kind::FunctionSignature(fs) = impl_sig_sym_readable.deref() {
                        fs
                    } else {
                        let message = format!("COMPILER BUG: Siganture of function: {:?} is not a FunctionSignature: {:?}", f, impl_sig_sym);
                        panic!(message);
                    };

                    (impl_sig.name.clone(), sym_id)
                } else {
                    let message = format!("COMPILER BUG: Contract required function is not a FunctionSignature: {:?}", impl_func_sym);
                    panic!(message);
                } 
            })
            .collect();

        for (req_name, req_sig_id) in required_functions_by_name.iter() {
            let req_sig_sym = self.symbol_table.load_symbol(*req_sig_id);
            let req_sig_sym_readable = req_sig_sym.read().unwrap();
            let req_sig = if let Kind::FunctionSignature(f) = req_sig_sym_readable.deref() {
                f
            } else {
                let message = format!("COMPILER BUG: Kind pointing to required function signature is a {} but expected a FunctionSignature", 
                                      req_sig_sym.read().unwrap().kind_hash(&self.symbol_table).unwrap());
                panic!(message);
            };

            if !implementing_functions_by_name.contains_key(req_name) {
                let message = format!("Contract implementation for {} by {} requires function {}",
                                      contract_sym.read().unwrap().kind_hash(&self.symbol_table).unwrap(),
                                      implementor_type_sym.read().unwrap().kind_hash(&self.symbol_table).unwrap(),
                                      req_sig.kind_hash(&self.symbol_table).unwrap());
                panic!(message);
            }

            let implementor_id = implementing_functions_by_name.get(req_name).unwrap();
            let implementor_func_sym = self.symbol_table.load_symbol(*implementor_id);
            let implementor_func_sym_readable = implementor_func_sym.read().unwrap();
            let implementor_func = if let Kind::Function(f) = implementor_func_sym_readable.deref() {
                f
            } else {
                let message = format!("COMPILER BUG: Kind pointed to implementing function is a {} but expected a Function", 
                                      implementor_func_sym_readable.kind_hash(&self.symbol_table).unwrap());
                panic!(message);
            };

            let implementor_func_sig_sym = self.symbol_table.load_symbol(implementor_func.signature);
            let implementor_func_sig_sym_readable = implementor_func_sig_sym.read().unwrap();
            let implementor_func_sig = if let Kind::FunctionSignature(fs) = implementor_func_sig_sym_readable.deref() {
                fs
            } else {
                
                let message = format!("COMPILER BUG: Kind pointing function signature is a {} but expected a FunctionSignature", 
                                      implementor_func_sig_sym_readable.kind_hash(&self.symbol_table).unwrap());
                panic!(message);
            };
         
            if !req_sig.is_compatible_with(&implementor_func_sig, &self.symbol_table) {
                let message = format!("Function with name: {} has signature: {} but expected: {}",
                                      req_name,
                                      req_sig.kind_hash(&self.symbol_table).unwrap(),
                                      implementor_func_sig.kind_hash(&self.symbol_table).unwrap());
                panic!(message);
            }
        }

        for (_impl_name, func_id) in implementing_functions_by_name.iter() {
            let func_sym = self.symbol_table.load_symbol(*func_id);
            let func_sym_readable = func_sym.read().unwrap();
            let func = if let Kind::Function(f) = func_sym_readable.deref() {
                f
            } else {
                let message = format!("COMPILER BUG: Kind pointed to implementing function is a {} but expected a Function", 
                                      func_sym_readable.kind_hash(&self.symbol_table).unwrap());
                panic!(message);
            };

            let func_sig_sym = self.symbol_table.load_symbol(func.signature);
            let func_sig_sym_readable = func_sig_sym.read().unwrap();
            let func_sig = if let Kind::FunctionSignature(fs) = func_sig_sym_readable.deref() {
                fs
            } else {
                let message = format!("COMPILER BUG: Kind pointed to by signature of function: {} is a {} but expected a FunctionSignature", 
                                      func.kind_hash(&self.symbol_table).unwrap(),
                                      func_sig_sym_readable.kind_hash(&self.symbol_table).unwrap());
                panic!(message);
            };

            if !required_functions_by_name.contains_key(&func_sig.name) {
                let message = format!("Contract implementation for {} by {} contains unknown function: {}",
                                      contract_sym.read().unwrap().kind_hash(&self.symbol_table).unwrap(),
                                      implementor_type_sym.read().unwrap().kind_hash(&self.symbol_table).unwrap(),
                                      func.kind_hash(&self.symbol_table).unwrap());
                panic!(message);
            }
        }
    
        let vtable = self.vtables.load_vtable(vptr);
        vtable.write().unwrap().functions = implementing_functions_by_name;

        let parent_env = self.current_env.write().unwrap().parent.take().unwrap();
        self.current_env = parent_env;
    }

    fn visit_variant_declaration(&mut self, variant_decl: &VariantDeclaration) {
        panic!("unimplemented visit_variant_declaration");
    }

    fn visit_function_declaration(&mut self, func_decl: &FunctionDeclaration) {
        let name = &func_decl.signature.name;

        let already_exists = {
            self.current_env
                .read()
                .unwrap()
                .get_symbol_by_name(name)
                .is_some()
        };

        if already_exists {
            let message = format!("function '{}' already defined in the current environment",
                                  name);
            panic!(message);
        }

        self.visit_function_signature(&func_decl.signature);
        let func_sig_id = self.last_local.take().unwrap();

        let func = Function {
            // TODO -> remove this and pass module context in
            parent: KindTable::MAIN_MODULE_SYMBOL_ID,
            signature: func_sig_id,
            body: func_decl.body.clone(),
            environment: self.current_env.clone(),
        };

        let func_sym = Kind::Function(func);

        let symbol_id = self.symbol_table.new_symbol(func_sym);
        self.current_env
            .write()
            .unwrap()
            .define(name.clone(), symbol_id);
        self.last_local = Some(symbol_id);
    }

    fn visit_function_signature(&mut self, func_sig: &crate::syntax::parse_tree::FunctionSignature) {
        let type_params = self.generate_type_holes(&func_sig.type_parameters);

        let mut parameters = Vec::new();
        for param in func_sig.parameters.iter() {
            let p = match param {
                FunctionParameter::SelfParam => {
                    let current_sym_id = self.last_local.take().unwrap();
                    (SELF_VAR_SYMBOL_NAME.into(), current_sym_id)
                },
                FunctionParameter::TypedVariableDeclarationParam(tvd) => {
                    self.visit_expression(&tvd.type_reference);
                    (tvd.name.clone(), self.last_local.take().unwrap())
                }
            };

            parameters.push(p);
        }

        let returns = match func_sig.returns {
            Some(ref expr) => {
                self.visit_expression(&expr);
                Some(self.last_local.take().unwrap())
            },
            None => None,
        };

        let func_sig = FunctionSignature {
            name: func_sig.name.clone(),
            type_parameters: type_params,
            parameters,
            returns,
        };

        let item = Item::TypeReference(func_sig.kind_hash());
        let addr = self.heap.alloc();
        self.heap.store(addr, item);
        let value = Value::Reference(Reference {
            ty: func_sig.kind_hash(),
            is_self: false,
            address: addr,
            size: std::u32::MAX,
        });

        self.kind_table.create(Kind::FunctionSignature(func_sig));
        self.current_env.define(func_sig.name.clone(), value);
    }

    fn visit_typed_variable_declaration(&mut self, typed_var_decl: &TypedVariableDeclaration) {
        panic!("unimplemented typed_variable_declaration");
    }

    fn visit_block_body(&mut self, block_body: &BlockBody) {
        for stmt in block_body.statements.iter() {
            if self.exiting {
                self.exiting = false;
                return;
            }

            self.visit_statement(stmt);
        }

        self.exiting = false;
    }

    fn visit_statement(&mut self, statement: &Statement) {
        use Statement::*;

        match statement {
            VariableAssignmentNode(var_assignment) =>
                self.visit_variable_assignment(var_assignment),

            ReturnNode(return_node) =>
                self.visit_return(&return_node),

            ExpressionNode(expr_node) =>
                self.visit_expression(expr_node),
        }
    }

    fn visit_variable_assignment(&mut self, var_assignment: &VariableAssignment) {
        self.visit_expression(&var_assignment.target_type);
        let target_type_reference = self.stack.pop().unwrap();

        self.visit_expression(&var_assignment.value);
        let value = self.stack.pop().unwrap();
        // TODO -> type check
        if value.get_type() != target_type_reference {
            // TODO -> maybe set the contract_ty field of ObjectInstance or EnumInstance so we can
            // track whether this object is acting as a Contract?
        }

        let addr = self.resolve_address(var_assignment.target);
        self.heap.store(addr, value);
    }

    fn visit_return(&mut self, return_stmt: &Return) {
        self.visit_expression(&return_stmt.value);
        self.exiting = true;
    }

    fn visit_expression(&mut self, expr: &Expression) {
        use crate::syntax::parse_tree::Expression::*;

        match expr {
            ChainableExpressionNode(cen) => self.visit_chainable_expression(cen),
            UnaryOperationNode(uo) => self.visit_unary_operation(uo),
            LambdaNode(lambda) => self.visit_lambda(lambda),
        }
    }

    fn visit_chainable_expression(&mut self, chainable_expr: &ChainableExpression) {
        use ExpressionChain::*;
        use ExpressionStart::*;

        match chainable_expr.start {
            ConditionalNode(ref cond_node) => self.visit_conditional(cond_node),
            MatchNode(ref match_node) => self.visit_match(match_node),
            LoopNode(ref loop_node) => self.visit_loop(loop_node),
            VariableNode(ref variable) => {
                let name = match variable {
                    Variable::Name(name) => name.clone(),
                    Variable::SelfVariable => "self".to_string(),
                    Variable::SelfType => "Self".to_string(),
                };

                self.last_local = match self.current_env.read().unwrap().get_symbol_by_name(&name) {
                    Some(sym) => {
                        Some(sym.clone())
                    },
                    None => {
                        let message = format!("unknown symbol {:?}", name);
                        panic!(message);
                    },
                }
            }
            ValueNode(ref value) => {
                // TODO -> need to revisit temporaries. Shouldn't store them in global symbol
                // table...
                let symbol_id = self.symbol_table.new_symbol(Kind::Value(value.clone()));
                self.last_local = Some(symbol_id);
            }
        };


        for expr_chain in chainable_expr.chained.iter() {
            match expr_chain {
                FieldAccessNode(field_access) => self.visit_field_access(field_access),
                ModuleAccessNode(mod_access) => self.visit_module_access(mod_access),
                ObjectInitializationNode(obj_init) => self.visit_object_initialization(obj_init),
                FunctionApplicationNode(func_app) => self.visit_function_application(func_app),
                TypeApplicationNode(type_app) => self.visit_type_application(type_app),
                BinaryOperationNode(bin_op) => self.visit_binary_operation(bin_op),
            };
        }
    }

    fn visit_conditional(&mut self, conditional: &Conditional) {
        for if_expr in conditional.if_exprs.iter() {
            self.visit_expression(&if_expr.condition);
            let cond_value = self.stack.pop().unwrap();
            match cond_value.to_bool() {
                Some(b) => {
                    if b {
                        self.visit_block_body(&if_expr.body);
                        return;
                    }
                },
                None => {
                    let message = format!("expected a boolean value in if condition but got: {}", cond_value.ty);
                    panic!(message);
                },
            }
        }

        if let Some(else_body) = &conditional.else_expr {
            self.visit_block_body(else_body);
        }
    }

    fn visit_match(&mut self, match_node: &Match) {
        panic!("unimplemented visit_match");
    }

    fn visit_loop(&mut self, loop_node: &Loop) {
        let loop_local_env = Environment::from_parent(&self.current_env);
        self.current_env = Arc::new(RwLock::new(loop_local_env));

        self.visit_variable_assignment(&loop_node.init);
       
        let condition_checker = |interp: &mut Interpreter| {
            interp.visit_expression(&loop_node.condition);
            let cond_value = interp.stack.pop().unwrap();
            match cond_value.to_bool() {
                Some(b) => *b,
                None => {
                    let message = format!("expected a boolean value in loop condition but got: {}", cond_value.ty);
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

    fn visit_field_access(&mut self, field_access: &FieldAccess) {
        let to_access_value = self.stack.pop().unwrap();
        let reference = match to_access_value {
            Value::Reference(reference) => reference,
            Value::Scalar(_) => {
                let message = format!("trying to use field access syntax on a scalar");
                panic!(message);
            }
        };

        let item = self.heap.load(reference.address);

        match item {
            Item::ObjectInstance(oi_arc) => {
                let oi_readable = oi_arc.read().unwarp();
                let obj = self.kind_table.load(oi_readable.ty);

                if reference.is_self {
                    if let Some(v) = oi_readable.fields.get(&field_access.field_name) {
                            self.stack.push(Value::clone(v));
                            return;
                    }

                    // we want to check methods here becuase a reference that is both self and
                    // has a contract_ty, it can still access regular methods
                    if let Some(func) = self.get_obj_method_with_name(obj, field_access.field_name) {
                        // TODO -> push func reference
                        // TODO -> push self reference
                    }
                }

                if let Some(contract_kind_hash) = oi_readable.contract_ty {
                    // this object instance is acting as a contract and is nto self, we can only
                    // access this contract's methods
                    let vtable_id = obj.vtables.get(contract_kind_hash).unwrap();
                    let vtable = self.vtables.load(vtable_id);
                }

                if let Some(func) = self.get_obj_method_with_name(obj, field_access.field_name) {
                        // TODO -> push func reference
                        // TODO -> push self reference
                }

                let message = format!("unknown method '{}' of object: {}.",
                                  field_access.field_name,
                                  obj.kind_hash(&self.symbol_table).unwrap());
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

    fn visit_module_access(&mut self, mod_access: &ModuleAccess) {
        let to_access_value = self.stack.pop().unwrap();
        let reference = match to_access_value {
            Value::Reference(reference) => reference,
            Value::Scalar(_) => {
                let message = format!("trying to use field access syntax on a scalar");
                panic!(message);
            }
        };
        let item = self.heap.load(reference.address);
        match item {
            Item::ModuleReference(mod_hash) => {
                unimplemented!("module access: {}", mod_hash);
            },
            Item::TypeReference(kind_hash) => {
                let kind = self.kind_table.load(kind_hash);
                if let Some(enu) = kind.to_enum() {
                    match enu.variant_values.get(&mod_access.name) {
                        Some(addr)  => {
                            let item = self.heap.load(addr);
                            match item {
                                Item::EnumInstance(ei_arc) => {
                                    let addr = self.heap.alloc();
                                    let ei_readable = ei_arc.read().unwrap();
                                    let new_instance = EnumInstance::clone(ei_readable.deref());
                                    let new_item = Item::EnumInstance(Arc::new(RwLock::new(new_instance)));
                                    self.heap.store(addr, new_item);
                                },
                                Item::Function(func) => {
                                    // first push the function reference
                                    self.stack.push(func);

                                    // first arg is kind_hash of the enum
                                    // TODO -> make the kind_hash a heap allocated string?
                                    self.stack.push();
                                    
                                    // second arg is the name of the variant
                                    // TODO -> make the variant_name a heap allocated string?
                                    self.stack.push();
                                   
                                    // third arg is whether it contains a type
                                    let variant_ty = enu.varaint_tys.get(&mod_access.name).unwrap();
                                    let does_contain_ty = variant_ty.is_some();
                                    let third_arg = Value::Scalar(Scalar::Bool(does_contain_ty));
                                    self.stack.push(third_arg);
                                  
                                    if does_contain_ty {
                                        // fourth arg should be kind_hash it contains
                                        let variant_kind_hash_value;
                                        self.stack.push(variant_kind_hash_value);

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
                                          enu.kind_hash(&self.symbol_table).unwrap(),
                                          mod_access.name);
                            panic!(message);
                        }
                    }
                } else {
                    let message = format!("trying to use module access syntax on something that isn't a module: {:?}",
                                  item.get_ty());
                }
            },
        }
    }

    fn visit_object_initialization(&mut self, obj_init: &ObjectInitialization) {
        let value = self.stack.pop().unwrap();
        let reference = value.to_reference();
        let item = self.heap.load(reference.address);
        let obj_hash = item.to_type_reference_hash();
        let obj = self.kind_table.load(obj_hash);

        // TODO -> need to validate that the object init doesn't have any fields that aren't in the
        // object

        let mut field_values = HashMap::new();
        for (field_name, expected_ty) in obj.fields.iter() {
            if !obj_init.fields.contains_key(field_name) {
                let message = format!(
                    "expected field '{}' in object initialization not present in {:?}",
                    field_name,
                    obj_init.fields.keys()
                );
                panic!(message);
            }

            // TODO -> need to resolve type variables to their actual type?

            let field_expr = obj_init.fields.get(field_name).unrwap();
            self.visit_expression(field_expr);
            let field_value = self.stack.pop().unwrap();
            if field_value.get_ty() != expected_ty {
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
            ty: obj_hash,
            contract_ty: None,
            field_values,
        };

        let address = self.heap.alloc();
        self.heap.store(address, Item::ObjectInstance(obj_instance));
        let reference = Reference {
            ty: obj_hash,
            is_self: false,
            address,
            size: std::u32::MAX,
        };
        let value = Value::Reference(reference);
        self.stack.push(value);
    }

    fn visit_function_application(&mut self, func_app: &FunctionApplication) {
        let to_apply_to_value = self.stack.pop().unwrap();
        let reference = match to_apply_to_value {
            Value::Reference(reference) => {

            },
            v => {
                let message = format!("expected to apply to a reference but got: {}", v);
                panic!(message)
            }
        };

        let to_apply_to_item = self.heap.load(reference.address);

        let func = match to_apply_to_item.to_func() {
            Some(f) => f,
            None => {
                let message = format!("trying to call something that isn't a function: {}", to_apply_to_item);
                panic!(message);
            }
        };

        func_app.args
            .iter()
            .for_each(Self::visit_expression);

        // TODO -> type_check args?
        
        let result = func.call(self);

        // TODO -> check signature to see if returns
        if let Some(ret_type) = signature.returns {
            self.stack.push(result.unwrap());
        }
    }

    fn visit_type_application(&mut self, type_app: &TypeApplication) {
        let to_apply_to_sym_id = self.last_local.take().unwrap();
        let to_apply_to = self.symbol_table.load_symbol(to_apply_to_sym_id);

        // can only apply to functions, objects, enums, ???

        // TODO -> check num type params versus num type args
        match to_apply_to.read().unwrap().deref() {
            Kind::Object(o) => {
                if o.type_arguments.len() != type_app.args.len() {
                    let message = format!("invalid number of type parameters in type application. expected {}, got {}", o.type_arguments.len(), type_app.args.len());
                    panic!(message);
                }
            }
            Kind::Enum(e) => {
                if e.type_arguments.len() != type_app.args.len() {
                    let message = format!("invalid number of type parameters in type application. expected {}, got {}", e.type_arguments.len(), type_app.args.len());
                    panic!(message);
                }
            }
            Kind::Function(_f) => {
                panic!("unimplemented");
            }
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        }

        let mut types = Vec::new();
        for arg in type_app.args.iter() {
            self.visit_expression(arg);
            let evaluated_type = self.last_local.take().unwrap();

            // TODO -> type check that results are of type: Type

            types.push(evaluated_type);
        }

        // NOTE: This is purposefully actually cloning the underlying type as we are creating
        // a new with one applied types
        // TODO -> merge this with below and clean up, reduce duplication
        let to_apply_to_readable = to_apply_to.read().unwrap();
        let mut new_type = match to_apply_to_readable.deref() {
            Kind::Object(obj)     => Kind::Object(Object::clone(obj)),
            Kind::Enum(enu)       => Kind::Enum(Enum::clone(enu)),
            Kind::Function(_func) => panic!("unimplemented"),
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        };

        // adjust fields and types 
        match new_type {
            Kind::Object(ref mut o) => {
                // create new sym_id
                // update type args to use new sym_id
                // update fields to use new sym_id

                let mut old_sym_id_to_new_sym_id = HashMap::new();
                let mut new_type_args = Vec::new();
                let mut new_fields = HashMap::new();
                let zipped_type_args_with_applied_type = o
                    .type_arguments
                    .iter()
                    .zip(types.into_iter());

                for ((type_name, type_hole_sym_id), type_to_apply) in zipped_type_args_with_applied_type {
                    let new_sym = Kind::TypeVariable(Some(type_to_apply));
                    let new_sym_id = self.symbol_table.get_or_create_symbol(new_sym);
                    old_sym_id_to_new_sym_id.insert(type_hole_sym_id, new_sym_id);
                    new_type_args.push((type_name.clone(), new_sym_id));

                    for (field_name, field_sym_id) in o.fields.iter() {
                        if field_sym_id == type_hole_sym_id {
                            new_fields.insert(field_name.clone(), new_sym_id);
                        }
                    }
                }

                o.type_arguments = new_type_args;
                o.fields = new_fields;

            },
            Kind::Enum(ref mut e) => {
                // TODO -> this is copied and pasted from above. need to find where to put this logic

                // create new sym_id
                // update type args to use new sym_id
                // update fields to use new sym_id

                let mut old_sym_id_to_new_sym_id = HashMap::new();
                let mut new_type_args = Vec::new();
                let mut new_variants = HashMap::new();
                let zipped_type_args_with_applied_type = e
                    .type_arguments
                    .iter()
                    .zip(types.into_iter());

                for ((type_name, type_hole_sym_id), type_to_apply) in zipped_type_args_with_applied_type {
                    let new_sym = Kind::TypeVariable(Some(type_to_apply));
                    let new_sym_id = self.symbol_table.get_or_create_symbol(new_sym);
                    old_sym_id_to_new_sym_id.insert(type_hole_sym_id, new_sym_id);
                    new_type_args.push((type_name.clone(), new_sym_id));

                    for (field_name, maybe_field_sym_id) in e.variants.iter() {
                        if let Some(field_sym_id) = maybe_field_sym_id {
                            if field_sym_id == type_hole_sym_id {
                                new_variants.insert(field_name.clone(), Some(new_sym_id));
                            }
                        } else {
                            new_variants.insert(field_name.clone(), None);
                        }
                    }
                }

                e.type_arguments = new_type_args;
                e.variants = new_variants;
            },
            Kind::Function(_f) => {
                panic!("unimplemented");
            },
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        }

        // we need to check if the equivalent type already exists
        let sym_id = self.symbol_table.get_or_create_symbol(Kind::clone(&new_type));
        let sym = self.symbol_table.load_symbol(sym_id);
        {
            let mut sym_readable = sym.write().unwrap();
            match sym_readable.deref_mut() {
                Kind::Enum(ref mut e) => {
                    for (_, variant_sym) in e.variant_funcs.iter_mut() {
                        if let Kind::EnumInstance(ei) = variant_sym {
                            ei.ty = sym_id.clone();
                        }
                    }
                },
                _ => ()
            }
        }
        self.symbol_table.replace_symbol(sym_id, sym);
        self.last_local = Some(sym_id);
    }

    fn visit_unary_operation(&mut self, _unary_op: &UnaryOperation) {
        panic!("unimplemented visit_unary_operation");
    }

    fn visit_lambda(&mut self, _lambda: &Lambda) {
        panic!("unimplemented visit_lambda");
    }

    fn visit_binary_operation(&mut self, bin_op: &BinaryOperation) {
        let lhs_value = self.stack.pop().unwrap();
        let lhs = match lhs_value {
            Value::Scalar(scalar) => scalar,
            other => panic!(format!(
                "expected lhs of binary operation to be a scalar but was {:?}",
                other
            )),
        };

        self.visit_expression(&bin_op.rhs);
        let rhs_value = self.stack.pop().unwrap();
        let rhs = match rhs_value {
            Value::Scalar(scalar) => scalar,
            other => panic!(format!(
                "expected rhs of binary operation to be a scalar but was {:?}",
                other
            )), 
        };

        let result = match bin_op.op {
            Plus =>               Value::Scalar(Scalar::add(lhs, rhs)),
            Minus =>              Value::Scalar(Scalar::subtract(lhs, rhs)),
            Multiply =>           Value::Scalar(Scalar::multiply(lhs, rhs)),
            Divide =>             Value::Scalar(Scalar::divide(lhs, rhs)),
            LessThan =>           Value::Scalar(Scalar::less_than(lhs, rhs)),
            LessThanOrEqual =>    Value::Scalar(Scalar::less_than_or_equal(lhs, rhs)),
            GreaterThan =>        Value::Scalar(Scalar::greater_than(lhs, rhs)),
            GreaterThanOrEqual => Value::Scalar(Scalar::greater_than_or_equal(lhs, rhs)),
            Equal =>              Value::Scalar(Scalar::equal_to(lhs, rhs)),
            NotEqual =>           Value::Scalar(Scalar::not_equal_to(lhs, rhs)),
            Or =>                 Value::Scalar(Scalar::or(lhs, rhs)),
            And =>                Value::Scalar(Scalar::and(lhs, rhs)),
        };

        self.stack.push(result);
    }
}

impl Callable<Interpreter> for Object {
    fn call(&self, _interpreter: &mut Interpreter, args: Vec<Value>) -> Option<Value> {
        panic!("unimplemented");
    }
}

impl Callable<Interpreter> for Enum {
    fn call(&self, _interpreter: &mut Interpreter, args: Vec<Value>) -> Option<Value> {
        panic!("unimplemented");
    }
}

impl Callable<Interpreter> for Function {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Option<Value> {
        let signature_sym = interpreter.symbol_table.load_symbol(self.signature);
        let signature_sym_readable = signature_sym.read().unwrap();
        let signature = if let Kind::FunctionSignature(sig) = signature_sym_readable.deref() {
            sig
        } else {
            let message = format!("COMPILER ERROR: expected FunctionSignature but got {}", signature_sym_readable.kind_hash(&interpreter.symbol_table).unwrap());
            panic!(message);
        };
        
        if signature.parameters.len() != args.len() {
            let message = format!(
                "invalid number of parameters in application of function: {}. expected {}, got {}",
                self.kind_hash(&interpreter.symbol_table).unwrap(),
                signature.parameters.len(),
                args.len()
            );
            panic!(message);
        }

        let mut func_app_local_env = Environment::from_parent(&self.environment);
        for ((n, param_id), arg_id) in signature.parameters.iter().zip(args.into_iter()) {
            let param_sym = interpreter.symbol_table.load_symbol(*param_id);
            let param_sym_readable = param_sym.read().unwrap();

            // TODO -> type check load_sym(arg_id).get_type against param_sym

            func_app_local_env.define(n.clone(), arg_id);
        }

        let current_env = Arc::clone(&interpreter.current_env);
        interpreter.set_current_env(func_app_local_env);
        interpreter.visit_block_body(&self.body);
        interpreter.current_env = current_env;
        
        if let Some(_type_sym) = &signature.returns {
            // TODO -> type check return based on the expected `type_sym`
            Some(interpreter.last_local.take().unwrap())
        } else {
            None
        }
    }
}

impl Callable<Interpreter> for NativeFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Option<Value> {
        (self.func)(interpreter, args)
    }
}

