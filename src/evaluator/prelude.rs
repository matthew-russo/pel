use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use super::evaluator::{
    Environment,
    Heap,
    Kind,
    KindHash,
    KindHashable,
    KindTable,
    Item,
    Function,
    FunctionSignature,
    Module,
    NativeFunction,
    Reference,
    ScalarType,
    Value
};
use super::interpreter::Interpreter;
use super::pel_utils;

pub const BOOL_TY:   &str = "bool";
pub const CHAR_TY:   &str = "char";
pub const INT_TY:    &str = "int";
pub const LONG_TY:   &str = "long";
pub const FLOAT_TY:  &str = "float";
pub const DOUBLE_TY: &str = "double";

pub const STRING_TY: &str = "pel::lang::string::String";
pub const STRING_FIELD: &str = "internal";

pub(super) fn prelude(kind_table: &mut KindTable, heap: &mut Heap) -> HashMap<String, Reference> {
    let mut env = HashMap::new();

    kind_table.create(heap, Kind::ScalarType(ScalarType::BoolType));
    env.insert(BOOL_TY.into(), Reference::create_type_reference(&ScalarType::bool_kind_hash(), heap));

    kind_table.create(heap, Kind::ScalarType(ScalarType::CharType));
    env.insert(CHAR_TY.into(), Reference::create_type_reference(&ScalarType::char_kind_hash(), heap));

    kind_table.create(heap, Kind::ScalarType(ScalarType::IntegerType));
    env.insert(INT_TY.into(), Reference::create_type_reference(&ScalarType::int_kind_hash(), heap));

    kind_table.create(heap, Kind::ScalarType(ScalarType::LongType));
    env.insert(LONG_TY.into(), Reference::create_type_reference(&ScalarType::long_kind_hash(), heap));

    kind_table.create(heap, Kind::ScalarType(ScalarType::FloatType));
    env.insert(FLOAT_TY.into(), Reference::create_type_reference(&ScalarType::float_kind_hash(), heap));

    kind_table.create(heap, Kind::ScalarType(ScalarType::DoubleType));
    env.insert(DOUBLE_TY.into(), Reference::create_type_reference(&ScalarType::double_kind_hash(), heap));

  
    let print_fn = Function::NativeFunction(print_nat_fn(kind_table, heap));
    let print_func_ref = Reference::create_function(print_fn, kind_table, heap);
    env.insert("print".into(), print_func_ref);

    let panic_fn = Function::NativeFunction(panic_nat_fn(kind_table, heap));
    let panic_func_ref = Reference::create_function(panic_fn, kind_table, heap);
    env.insert("panic".into(), panic_func_ref);

    env
}

fn print_nat_fn(kind_table: &mut KindTable, heap: &mut Heap) -> Arc<RwLock<NativeFunction>> {
    let print_name = String::from("print");

    let print_func_sig = FunctionSignature {
        parent: KindHash::from(Module::MAIN_MODULE_KIND_HASH),
        name: String::clone(&print_name),
        type_arguments: Vec::new(),
        parameters: vec![(String::from("to_print"), Reference::create_type_reference(&KindHash::from(STRING_TY), heap))],
        returns: None,
    };

    let func_sig_kind_hash = print_func_sig.kind_hash(kind_table, heap);
    kind_table.create(heap, Kind::FunctionSignature(Arc::new(RwLock::new(print_func_sig))));

    Arc::new(RwLock::new(NativeFunction {
        name: print_name,
        signature: func_sig_kind_hash,
        func: |interp: &mut Interpreter, args: Vec<Reference>| {
            if args.len() != 1 {
                let message = format!(
                    "invalid number of parameters function call. expected 1, got {}",
                    args.len()
                );
                panic!(message);
            }

            let arg_heap_ref = args[0].to_heap_ref().unwrap();
            let item = interp.heap.load(arg_heap_ref.address);
            let maybe_rust_str = pel_utils::pel_string_to_rust_string(&item, &mut interp.heap);
            if let Some(rust_str) = maybe_rust_str {
                println!("{}", rust_str);
            } else {
                println!("expected a string argument but got: {}", arg_heap_ref.ty);
            }

            None
        },
    }))
}

fn panic_nat_fn(kind_table: &mut KindTable, heap: &mut Heap) -> Arc<RwLock<NativeFunction>> {
    let panic_name = String::from("panic");

    let panic_func_sig = FunctionSignature {
        parent: KindHash::from(Module::MAIN_MODULE_KIND_HASH),
        name: String::clone(&panic_name),
        type_arguments: Vec::new(),
        parameters: vec![(String::from("panic_message"), Reference::create_type_reference(&KindHash::from(STRING_TY), heap))],
        returns: None,
    };

    let func_sig_kind_hash = panic_func_sig.kind_hash(kind_table, heap);
    kind_table.create(heap, Kind::FunctionSignature(Arc::new(RwLock::new(panic_func_sig))));

    Arc::new(RwLock::new(NativeFunction {
        name: panic_name,
        signature: func_sig_kind_hash,
        func: |interp: &mut Interpreter, args: Vec<Reference>| {
            if args.len() != 1 {
                let message = format!(
                    "invalid number of parameters function call. expected 1, got {}",
                    args.len()
                );
                panic!(message);
            }
            
            let arg_heap_ref = args[0].to_heap_ref().unwrap();
            let item = interp.heap.load(arg_heap_ref.address);
            let maybe_rust_str = pel_utils::pel_string_to_rust_string(&item, &mut interp.heap);
            if let Some(rust_str) = maybe_rust_str {
                panic!("{}", rust_str);
            } else {
                println!("expected a string argument but got: {}", arg_heap_ref.ty);
            }

            None
        },
    }))
}

