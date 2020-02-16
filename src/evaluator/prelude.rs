use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use super::evaluator::{
    Heap,
    Kind,
    KindHash,
    KindTable,
    Item,
    Function,
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

pub const STRING_TY: &str = "pel::lang::String";
pub const STRING_FIELD: &str = "internal";

pub(super) fn prelude(kind_table: &mut KindTable, heap: &mut Heap) -> HashMap<String, Value> {
    let mut env = HashMap::new();

    kind_table.create(Kind::ScalarType(ScalarType::BoolType));
    env.insert(BOOL_TY.into(), Value::create_type_reference(ScalarType::bool_kind_hash(), heap));

    kind_table.create(Kind::ScalarType(ScalarType::CharType));
    env.insert(CHAR_TY.into(), Value::create_type_reference(ScalarType::char_kind_hash(), heap));

    kind_table.create(Kind::ScalarType(ScalarType::IntegerType));
    env.insert(INT_TY.into(), Value::create_type_reference(ScalarType::int_kind_hash(), heap));

    kind_table.create(Kind::ScalarType(ScalarType::LongType));
    env.insert(LONG_TY.into(), Value::create_type_reference(ScalarType::long_kind_hash(), heap));

    kind_table.create(Kind::ScalarType(ScalarType::FloatType));
    env.insert(FLOAT_TY.into(), Value::create_type_reference(ScalarType::float_kind_hash(), heap));

    kind_table.create(Kind::ScalarType(ScalarType::DoubleType));
    env.insert(DOUBLE_TY.into(), Value::create_type_reference(ScalarType::double_kind_hash(), heap));

   
    let print_func_val = Value::create_function(Function::NativeFunction(print_nat_fn()), &kind_table, heap);
    env.insert("print".into(), print_func_val);

    let panic_func_val = Value::create_function(Function::NativeFunction(panic_nat_fn()), &kind_table, heap);
    env.insert("panic".into(), panic_func_val);

    env
}

fn print_nat_fn() -> Arc<RwLock<NativeFunction>> {
    Arc::new(RwLock::new(NativeFunction {
        name: "print".into(),
        func: |interp: &mut Interpreter, args: Vec<Value>| {
            if args.len() != 1 {
                let message = format!(
                    "invalid number of parameters function call. expected 1, got {}",
                    args.len()
                );
                panic!(message);
            }

            let arg_ref = args[0].to_ref().unwrap();
            let arg_heap_ref = arg_ref.to_heap_ref().unwrap();
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

fn panic_nat_fn() -> Arc<RwLock<NativeFunction>> {
    Arc::new(RwLock::new(NativeFunction {
        name: "panic".into(),
        func: |interp: &mut Interpreter, args: Vec<Value>| {
            if args.len() != 1 {
                let message = format!(
                    "invalid number of parameters function call. expected 1, got {}",
                    args.len()
                );
                panic!(message);
            }

            let arg_ref = args[0].to_ref().unwrap();
            let arg_heap_ref = arg_ref.to_heap_ref().unwrap();
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

