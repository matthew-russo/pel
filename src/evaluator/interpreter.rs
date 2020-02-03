use std::collections::HashMap;
use std::ops::{Deref};
use std::sync::{Arc, RwLock};

use crate::syntax::parse_tree;
use crate::evaluator::evaluator::{
    Address,
    Callable,
    Contract,
    Enum,
    EnumInstance,
    Environment,
    Evaluator,
    Function,
    FunctionSignature,
    Heap,
    NativeFunction,
    Object,
    ObjectInstance,
    PelFunction,
    VTables,
    VTable,
    KindTable,
    Kind,
    KindHash,
    KindHashable,
    KIND_KIND_HASH_STR,
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
    pub heap: Heap,
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
        let heap = Heap::new();

        let mut kind_table = KindTable::new();
        let main_mod = kind_table.load(KindTable::MAIN_MODULE_SYMBOL_ID).unwrap().to_module().unwrap();

        let root_env = Arc::clone(&main_mod.read().unwrap().env);

        Self {
            kind_table,
            heap,
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

    pub fn set_current_env(&mut self, env: Environment) {
        self.current_env = Arc::new(RwLock::new(env));
    }

    fn get_obj_method_with_name(&self, obj: &Object, name: &String) -> Option<Value> {
        if let Some(func_hash) = obj.methods.get(name) {
            return Some(Value::clone(func_hash));
        }

        for (_, vtable_id) in obj.vtables.iter() {
            let vtable = self.vtables.load_vtable(*vtable_id);
            let vtable_readable = vtable.read().unwrap();

            if let Some(func_hash) = vtable_readable.functions.get(name) {
                return Some(Value::clone(func_hash));
            }
        }

        None
    }

    fn generate_type_holes(&mut self, type_params: &Vec<String>) -> Vec<(String, KindHash)> {
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

    fn resolve_value(&mut self, expr: &parse_tree::Expression) -> Address {
        // this can be:
        //      new var name
        //      preexisting var name
        //      field name
        
        let ce = if let parse_tree::Expression::ChainableExpressionNode(ce) = &expr {
            ce
        } else {
            let message = format!("Cannot assign to a {:?}", expr);
            panic!(message);
        };

        // check if single name
        //    -> either new var and need to alloc
        //    -> or preexisting var and return ref addr
        
        // otherwise we need to evalute the chain to a field

        // OLD CODE
        // if !ce.chained.is_empty() {
        //     return None;
        // }

        // let var = if let ExpressionStart::VariableNode(var) = &ce.start {
        //     var
        // } else {
        //     return None;
        // };

        // let var_name = match var {
        //     Variable::Name(n) => n.clone(),
        //     Variable::SelfVariable => SELF_VAR_SYMBOL_NAME.into(),
        //     Variable::SelfType => SELF_TYPE_SYMBOL_NAME.into(),
        // };

        // if let None = self.current_env.read().unwrap().get_symbol_by_name(&var_name) {
        //     Some(var_name) 
        // } else {
        //     None
        // }
    }
}

impl Evaluator for Interpreter {
    fn visit_program(&mut self, program: &parse_tree::Program) {
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
        }
    }

    fn visit_enum_declaration(&mut self, enum_decl: &parse_tree::EnumDeclaration) {
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
                            if r.ty != KindHash::from(KIND_KIND_HASH_STR) {
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

        let enu_kind_hash = enu.kind_hash(&self.kind_table);
        self.kind_table.create(Kind::Enum(Arc::new(RwLock::new(enu))));

        let variant_values = enum_decl.variants
            .iter()
            .map(|vd| {
                if vd.contains.is_none() {
                    let initialized_enum = EnumInstance {
                        ty: KindHash::clone(&enu_kind_hash),
                        contract_ty: None,
                        variant: (vd.name.clone(), None),
                    };

                    (vd.name.clone(), Item::EnumInstance(Arc::new(RwLock::new(initialized_enum))))
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
                                let message = format!("expected enum variant: {} to contain type {:?}, but got {}",
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
                let addr = self.heap.alloc();
                self.heap.store(addr, Item::TypeReference(KindHash::clone(&enu_kind_hash)));
                let reference = Value::Reference(Reference {
                    ty: KindHash::clone(&enu_kind_hash),
                    is_self: false,
                    address: addr,
                    size: std::u32::MAX,
                });
                self.stack.push(reference);
                self.visit_function_declaration(fd);
                let func_val = self.current_env
                    .read()
                    .unwrap()
                    .get_value_by_name(&fd.signature.name)
                    .unwrap();
                (fd.signature.name.clone(), func_val)
            })
            .collect();

        let enu = self.kind_table
            .load(&enu_kind_hash)
            .unwrap()
            .to_enum()
            .unwrap();
        enu.write().unwrap().variant_values = variant_values;
        enu.write().unwrap().methods = methods;

        let reference_to_enum = Value::create_type_reference(&enu_kind_hash, &self.heap);
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

        let type_arguments = self.generate_type_holes(&obj_decl.type_params);

        let fields = obj_decl
            .fields
            .iter()
            .map(|tvd| {
                self.visit_expression(&tvd.type_reference);
                // type check that last_local is a type/kind
                let evaluated = self.stack.pop().unwrap();
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

        let obj_kind_hash = obj.kind_hash(&self.kind_table);
        self.kind_table.create(Kind::Object(Arc::new(RwLock::new(obj))));

        let reference_to_obj = Value::create_type_reference(&obj_kind_hash, &self.heap);

        let methods = obj_decl.methods
            .iter()
            .map(|fd| {
                self.stack.push(Value::clone(&reference_to_obj));
                self.visit_function_declaration(fd);
                let func_val = self.current_env.read().unwrap().get_value_by_name(&fd.signature.name).unwrap();
                (fd.signature.name.clone(), Value::clone(&func_val))
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

        let type_arguments = self.generate_type_holes(&contract_decl.type_params);

        let contract = Contract {
            parent: KindTable::MAIN_MODULE_SYMBOL_ID,
            name: contract_decl.type_name.clone(),
            type_arguments,
            required_functions: Vec::new(),
        };

        let contract_kind_hash = contract.kind_hash(&self.kind_table);
        self.kind_table.create(Kind::Contract(Arc::new(RwLock::new(contract))));

        let reference_to_contract = Value::create_type_reference(&contract_kind_hash, &self.heap);

        let required_functions = contract_decl.functions
                .iter()
                .map(|fs| {
                    self.stack.push(Value::clone(&reference_to_contract));
                    self.visit_function_signature(&fs);
                    self.stack.pop().unwrap().to_ref().unwrap().ty
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
        if implementor_type_ref.ty != KindHash::from(KIND_KIND_HASH_STR) {
            panic!("implementor type must be a reference to a kind but got {}", implementor_type_ref.ty);
        }
        let implementor_kind_hash = self.heap.load_type_reference(implementor_type_ref.address).unwrap();

        self.visit_expression(&impl_decl.contract);
        let contract_type_ref = self.stack.pop().unwrap().to_ref().unwrap();
        if contract_type_ref.ty != KindHash::from(KIND_KIND_HASH_STR) {
            panic!("contract type type must be a reference to a kind but got {}", implementor_type_ref.ty);
        }
        let contract_kind_hash = self.heap.load_type_reference(contract_type_ref.address).unwrap();

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
        let required_functions_by_name: HashMap<String, KindHash> = if let Kind::Contract(c) = contract {
            c.read().unwrap().required_functions
                .iter()
                .map(|kind_hash| {
                    let req_func = self.kind_table.load(kind_hash).unwrap();
                    let req_func_readable = req_func;
                    return if let Kind::FunctionSignature(fs_arc) = req_func {
                        (fs_arc.read().unwrap().name.clone(), Arc::clone(&fs_arc))
                    } else {
                        let message = format!("COMPILER BUG: Contract required function is not a FunctionSignature: {:?}", req_func);
                        panic!(message);
                    }
                })
                .collect()
        } else {
            let message = format!("Expected Contract implementation to be a Contract but got {:?}", contract);
            panic!(message);
        };

        let implementing_functions_by_name: HashMap<String, Value> = impl_decl.functions
            .iter()
            .map(|fd| {
                self.stack.push(KindHash::clone(&implementor_kind_hash));
                self.visit_function_declaration(fd);
                let func_val = self.current_env.get_value_by_name(&fd.signature.name);
               
                // TODO -> need to define in env?
               
                (impl_sig.name.clone(), Value::clone(func_val))
            })
            .collect();

        for (req_name, req_sig) in required_functions_by_name.iter() {
            if !implementing_functions_by_name.contains_key(req_name) {
                let message = format!("Contract implementation for {} by {} requires function {}",
                                      contract_kind_hash,
                                      implementor_kind_hash,
                                      req_sig.kind_hash(&self.kind_table).unwrap());
                panic!(message);
            }

            let implementor_func_ref = implementing_functions_by_name.get(req_name).unwrap().to_ref().unwrap();

            if !req_sig.is_compatible_with(&implementor_func_ref.ty, &self.kind_table) {
                let message = format!("Function with name: {} has signature: {} but expected: {}",
                                      req_name,
                                      req_sig.kind_hash(&self.kind_table).unwrap(),
                                      implementor_func_ref.ty);
                panic!(message);
            }
        }

        for (_impl_name, func_val) in implementing_functions_by_name.iter() {
            let func_ref = func_val.to_ref().unwrap();
            if !required_functions_by_name.contains_value(&func_ref.ty) {
                let message = format!("Contract implementation for {} by {} contains unknown function: {}",
                                      contract_kind_hash,
                                      implementor_kind_hash,
                                      func_ref.ty);
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

    fn visit_function_declaration(&mut self, func_decl: &parse_tree::FunctionDeclaration) {
        let name = &func_decl.signature.name;

        let already_exists = {
            self.current_env
                .read()
                .unwrap()
                .get_value_by_name(name)
                .is_some()
        };

        if already_exists {
            let message = format!("function '{}' already defined in the current environment",
                                  name);
            panic!(message);
        }

        self.visit_function_signature(&func_decl.signature);
        let func_sig_ref = self.current_env
            .read()
            .unwrap()
            .get_value_by_name(&func_decl.signature.name)
            .unwrap()
            .to_ref()
            .unwrap();

        let func_sig_kind_hash = self.heap.load_type_reference(func_sig_ref.address).unwrap();
        let func_sig = self.kind_table.load(&func_sig_kind_hash).unwrap();

        let func = Function {
            // TODO -> remove this and pass module context in
            parent: KindTable::MAIN_MODULE_SYMBOL_ID,
            signature: func_sig_kind_hash,
            body: func_decl.body.clone(),
            // TODO -> this should be a deep clone i think
            environment: Arc::clone(&self.current_env),
        };

        let addr = self.heap.alloc();
        self.heap.store(addr, Item::Function(Arc::new(RwLock::new(func))));
        let func_val = Value::Reference(Reference {
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
            .define(name.clone(), func_val);
    }

    fn visit_function_signature(&mut self, func_sig: &parse_tree::FunctionSignature) {
        let type_params = self.generate_type_holes(&func_sig.type_parameters);

        let mut parameters = Vec::new();
        for param in func_sig.parameters.iter() {
            let param_ty = self.stack.pop().unwrap().get_ty();
            let p = match param {
                parse_tree::FunctionParameter::SelfParam => {
                    // TODO -> might want to pop this regardless or else our stack will always grow
                    (SELF_VAR_SYMBOL_NAME.into(), param_ty)
                },
                parse_tree::FunctionParameter::TypedVariableDeclarationParam(tvd) => {
                    self.visit_expression(&tvd.type_reference);
                    (tvd.name.clone(), param_ty)
                }
            };

            parameters.push(p);
        }

        let returns = match func_sig.returns {
            Some(ref expr) => {
                self.visit_expression(&expr);
                Some(self.stack.pop().unwrap().get_ty())
            },
            None => None,
        };

        let func_sig = FunctionSignature {
            name: func_sig.name.clone(),
            type_parameters: type_params,
            parameters,
            returns,
        };

        let item = Item::TypeReference(func_sig.kind_hash(&self.kind_table));
        let addr = self.heap.alloc();
        self.heap.store(addr, item);
        let value = Value::Reference(Reference {
            ty: func_sig.kind_hash(&self.kind_table),
            is_self: false,
            address: addr,
            size: std::u32::MAX,
        });

        self.kind_table.create(Kind::FunctionSignature(Arc::new(RwLock::new(func_sig))));
        self.current_env.write().unwrap().define(func_sig.name.clone(), value);
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

        self.visit_expression(&var_assignment.value);
        let value = self.stack.pop().unwrap();
        // TODO -> type check
        if value.get_ty() != target_type_reference.ty {
            // TODO -> maybe set the contract_ty field of ObjectInstance or EnumInstance so we can
            // track whether this object is acting as a Contract?
        }

        // TODO -> what if this is a scalar?
        // we can't return an address then
        // std::mem::replace as a hacky workaround?
        // well if its a scalar, then its just a value so all we need to do is update the name in
        // the current_env to point to a new value and there is no heap assignment
        // maybe return a (Option<String>, Option<Address>)?
        let addr = self.resolve_address(var_assignment.target);
        self.heap.store(addr, value);
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

                let var_val = match self.current_env.read().unwrap().get_value_by_name(&name) {
                    Some(val) => Value::clone(&val),
                    None => {
                        let message = format!("unknown symbol {:?}", name);
                        panic!(message);
                    }
                };

                self.stack.push(var_val);
            }
            ValueNode(ref value) => {
                let val = Value::Scalar(Scalar::from(value));
                self.stack.push(val);
            }
        };


        for expr_chain in chainable_expr.chained.iter() {
            match expr_chain {
                FieldAccessNode(field_access)
                    => self.visit_field_access(field_access),

                ModuleAccessNode(mod_access)
                    => self.visit_module_access(mod_access),

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
                Value::Scalar(s) => s,
                Value::Reference(r) => panic!("expected a boolean scalar but got reference of type: {}", r.ty),
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
            Value::Reference(reference) => reference,
            Value::Scalar(_) => {
                let message = format!("trying to use field access syntax on a scalar");
                panic!(message);
            }
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

                if let Some(contract_kind_hash) = oi_readable.contract_ty {
                    // this object instance is acting as a contract and is nto self, we can only
                    // access this contract's methods
                    let vtable_id = obj.read().unwrap().vtables.get(&contract_kind_hash).unwrap();
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
                let kind = self.kind_table.load(&kind_hash).unwrap();
                if let Some(enu) = kind.to_enum() {
                    match enu.read().unwrap().variant_values.get(&mod_access.name) {
                        Some(addr)  => {
                            match self.heap.load(*addr) {
                                Item::EnumInstance(ei_arc) => {
                                    let addr = self.heap.alloc();
                                    let ei_readable = ei_arc.read().unwrap();
                                    let new_instance = EnumInstance::clone(ei_readable.deref());
                                    let new_item = Item::EnumInstance(Arc::new(RwLock::new(new_instance)));
                                    self.heap.store(addr, new_item);
                                },
                                Item::Function(func) => {
                                    self.stack.push(Value::clone(&to_access_value));
                                    let var_name = utils::rust_string_to_pel_string(String::clone(&mod_access.name));
                                    self.stack.push(var_name);

                                    // third arg is whether it contains a type
                                    let variant_ty = enu.read().unwrap().variant_tys.get(&mod_access.name).unwrap();
                                    let does_contain_ty = variant_ty.is_some();
                                    let third_arg = Value::Scalar(Scalar::Boolean(does_contain_ty));
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
                                          kind_hash,
                                          mod_access.name);
                            panic!(message);
                        }
                    }
                } else {
                    let message = format!("trying to use module access syntax on something that isn't a module: {:?}", item);
                }
            },
        }
    }

    fn visit_object_initialization(&mut self, obj_init: &parse_tree::ObjectInitialization) {
        let value = self.stack.pop().unwrap();
        let reference = value.to_ref().unwrap();
        let item = self.heap.load(reference.address);
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
            ty: obj_hash,
            contract_ty: None,
            fields: field_values,
        };

        let address = self.heap.alloc();
        self.heap.store(address, Item::ObjectInstance(Arc::new(RwLock::new(obj_instance))));
        let reference = Reference {
            ty: obj_hash,
            is_self: false,
            address,
            size: std::u32::MAX,
        };
        let value = Value::Reference(reference);
        self.stack.push(value);
    }

    fn visit_function_application(&mut self, func_app: &parse_tree::FunctionApplication) {
        let to_apply_to_value = self.stack.pop().unwrap();
        let reference = match to_apply_to_value {
            Value::Reference(reference) => reference,
            v => {
                let message = format!("expected to apply to a reference but got: {:?}", v);
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
                self.stack.pop().unwrap()
            })
            .collect();

        // TODO -> type_check args?
        
        let result = func.read().unwrap().call(self, args);

        if let Some(ret_type) = signature.read().unwrap().returns {
            self.stack.push(result.unwrap())
        }
    }

    fn visit_type_application(&mut self, type_app: &parse_tree::TypeApplication) {
        let to_apply_to_ref = self.stack.pop().unwrap().to_ref().unwrap();
        let to_apply_to_kind = self.heap.load(to_apply_to_ref.address).to_type_reference().unwrap();
        let to_apply_to = self.kind_table.load(&to_apply_to_kind).unwrap();

        // can only apply to functions, objects, enums, ???

        // TODO -> check num type params versus num type args
        match to_apply_to {
            Kind::Object(o_arc) => {
                if o_arc.read().unwrap().type_arguments.len() != type_app.args.len() {
                    let message = format!("invalid number of type parameters in type application. expected {}, got {}", o_arc.read().unwrap().type_arguments.len(), type_app.args.len());
                    panic!(message);
                }
            },
            Kind::Enum(e_arc) => {
                if e_arc.read().unwrap().type_arguments.len() != type_app.args.len() {
                    let message = format!("invalid number of type parameters in type application. expected {}, got {}", e_arc.read().unwrap().type_arguments.len(), type_app.args.len());
                    panic!(message);
                }
            },
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        }

        let mut types = Vec::new();
        for arg in type_app.args.iter() {
            self.visit_expression(arg);
            let evaluated_ref = self.stack.pop().unwrap().to_ref().unwrap();
            let evaluated_item = self.heap.load(evaluated_ref.address).to_type_reference().unwrap();

            // TODO -> type check that results are of type: Type

            types.push(evaluated_item);
        }

        // NOTE: This is purposefully actually cloning the underlying type as we are creating
        // a new with one applied types
        // TODO -> merge this with below and clean up, reduce duplication
        let mut new_type = match to_apply_to {
            Kind::Object(ref obj)     => Kind::Object(Arc::new(RwLock::new(Object::clone(obj.read().unwrap().deref())))),
            Kind::Enum(ref enu)       => Kind::Enum(Arc::new(RwLock::new(Enum::clone(enu.read().unwrap().deref())))),
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        };

        // adjust fields and types 
        match new_type {
            Kind::Object(ref mut o) => {
                let mut new_type_args = Vec::new();
                let mut new_fields = HashMap::new();
                let zipped_type_args_with_applied_type = o
                    .read()
                    .unwrap()
                    .type_arguments
                    .iter()
                    .zip(types.into_iter());

                for ((type_name, type_hole_kind_hash), type_to_apply) in zipped_type_args_with_applied_type {
                    new_type_args.push((type_name.clone(), type_to_apply));

                    for (field_name, field_kind_hash) in o.read().unwrap().fields.iter() {
                        if field_kind_hash == type_hole_kind_hash {
                            new_fields.insert(field_name.clone(), type_to_apply);
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
                let zipped_type_args_with_applied_type = e
                    .read()
                    .unwrap()
                    .type_arguments
                    .iter()
                    .zip(types.into_iter());

                for ((type_name, type_hole_kind_hash), type_to_apply) in zipped_type_args_with_applied_type {
                    new_type_args.push((type_name.clone(), type_to_apply));

                    for (variant_name, maybe_variant_kind_hash) in e.read().unwrap().variant_tys.iter() {
                        if let Some(variant_kind_hash) = maybe_variant_kind_hash {
                            if variant_kind_hash == type_hole_kind_hash {
                                new_variants.insert(variant_name.clone(), Some(type_to_apply));
                            }
                        } else {
                            new_variants.insert(variant_name.clone(), None);
                        }
                    }
                }

                e.write().unwrap().type_arguments = new_type_args;
                e.write().unwrap().variant_tys = new_variants;
            },
            _ => panic!("trying to apply types to something that cannot take type arguements"),
        }

        // we need to check if the equivalent type already exists
        self.kind_table.create(new_type);
        let new_type_ref = Item::TypeReference(new_type.kind_hash(&self.kind_table));
        let addr = self.heap.alloc();
        self.heap.store(addr, new_type_ref);
        let val = Value::Reference(Reference {
            ty: new_type.kind_hash(&self.kind_table),
            is_self: false,
            address: addr,
            size: std::u32::MAX,
        });
        self.stack.push(val);
    }

    fn visit_unary_operation(&mut self, _unary_op: &parse_tree::UnaryOperation) {
        panic!("unimplemented visit_unary_operation");
    }

    fn visit_lambda(&mut self, _lambda: &parse_tree::Lambda) {
        panic!("unimplemented visit_lambda");
    }

    fn visit_binary_operation(&mut self, bin_op: &parse_tree::BinaryOperation) {
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
        match self {
            Function::NativeFunction(nf) => nf.call(interpreter, args),
            Function::PelFunction(pf)    => pf.call(interpreter, args),
        }
    }
}

impl Callable<Interpreter> for PelFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Option<Value> {
        let signature = interpreter.kind_table
            .load(&self.signature)
            .unwrap()
            .to_func_sig()
            .unwrap();
        
        if signature.read().unwrap().parameters.len() != args.len() {
            let message = format!(
                "invalid number of parameters in application of function: {}. expected {}, got {}",
                self.kind_hash(&interpreter.kind_table),
                signature.read().unwrap().parameters.len(),
                args.len()
            );
            panic!(message);
        }

        let mut func_app_local_env = Environment::from_parent(&self.environment);
        for ((n, param_kind), arg_value) in signature.read().unwrap().parameters.iter().zip(args.into_iter()) {
            if &arg_value.get_ty() != param_kind {
                panic!("expected argument with type: {}, got: {}",
                       param_kind,
                       arg_value.get_ty());
            }

            func_app_local_env.define(n.clone(), arg_value);
        }

        let current_env = Arc::clone(&interpreter.current_env);
        interpreter.set_current_env(func_app_local_env);
        interpreter.visit_block_body(&self.body);
        interpreter.current_env = current_env;
        
        if let Some(_type_sym) = &signature.read().unwrap().returns {
            // TODO -> type check return based on the expected `type_sym`
            Some(interpreter.stack.pop().unwrap())
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

