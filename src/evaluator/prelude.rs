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
    FunctionSignature,
    Module,
    Reference,
    Runnable,
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
  
    let print_func_ref = print_nat_fn(kind_table, heap);
    env.insert("print".into(), print_func_ref);

    let panic_func_ref = panic_nat_fn(kind_table, heap);
    env.insert("panic".into(), panic_func_ref);

    env
}

fn print_nat_fn(kind_table: &mut KindTable, heap: &mut Heap) -> Reference {
    let native_func = |interp: &mut Interpreter, args: Vec<Reference>| {
        if args.len() != 1 {
            let message = format!(
                "invalid number of parameters function call. expected 1, got {}",
                args.len()
            );
            panic!(message);
        }

        let mut reference = Reference::clone(&args[0]);

        let mut item = None;
        loop {
            match reference {
                Reference::StackReference(sr) => {
                    let value = Value::clone(&interp.stack[sr.index]);
                    reference = value.to_ref().expect("expected a string but got scalar");
                }
                Reference::HeapReference(hr) => {
                    item = Some(interp.heap.load(hr.address));
                    break;
                }
            }
        }
        let item = item.unwrap();

        let maybe_rust_str = pel_utils::pel_string_to_rust_string(&item, &mut interp.heap);
        if let Some(rust_str) = maybe_rust_str {
            println!("{}", rust_str);
        } else {
            println!("expected a string argument but got: {:?}", item);
        }

        None
    };

    let print_name = String::from("print");

    let print_func_sig = FunctionSignature {
        parent: KindHash::from(Module::MAIN_MODULE_KIND_HASH),
        name: String::clone(&print_name),
        type_arguments: Vec::new(),
        parameters: vec![(String::from("to_print"), Reference::create_type_reference(&KindHash::from(STRING_TY), heap))],
        environment: Arc::new(RwLock::new(Environment::root())),
        body: Some(Runnable::NativeFunction(native_func)),
        returns: None,
    };

    let func_sig_kind_hash = print_func_sig.kind_hash(kind_table, heap);
    kind_table.create(heap, Kind::FunctionSignature(Arc::new(RwLock::new(print_func_sig))));

    Reference::create_type_reference(&func_sig_kind_hash, heap)
}

fn panic_nat_fn(kind_table: &mut KindTable, heap: &mut Heap) -> Reference {
    let native_func = |interp: &mut Interpreter, args: Vec<Reference>| {
        if args.len() != 1 {
            let message = format!(
                "invalid number of parameters function call. expected 1, got {}",
                args.len()
            );
            panic!(message);
        }
        
        let mut reference = Reference::clone(&args[0]);

        let mut item = None;
        loop {
            match reference {
                Reference::StackReference(sr) => {
                    let value = Value::clone(&interp.stack[sr.index]);
                    reference = value.to_ref().expect("expected a string but got scalar");
                }
                Reference::HeapReference(hr) => {
                    item = Some(interp.heap.load(hr.address));
                    break;
                }
            }
        }
        let item = item.unwrap();

        let maybe_rust_str = pel_utils::pel_string_to_rust_string(&item, &mut interp.heap);
        if let Some(rust_str) = maybe_rust_str {
            panic!("{}", rust_str);
        } else {
            println!("expected a string argument but got: {:?}", item);
        }

        None
    };

    let panic_name = String::from("panic");

    let panic_func_sig = FunctionSignature {
        parent: KindHash::from(Module::MAIN_MODULE_KIND_HASH),
        name: String::clone(&panic_name),
        type_arguments: Vec::new(),
        parameters: vec![(String::from("panic_message"), Reference::create_type_reference(&KindHash::from(STRING_TY), heap))],
        environment: Arc::new(RwLock::new(Environment::root())),
        body: Some(Runnable::NativeFunction(native_func)),
        returns: None,
    };

    let func_sig_kind_hash = panic_func_sig.kind_hash(kind_table, heap);
    kind_table.create(heap, Kind::FunctionSignature(Arc::new(RwLock::new(panic_func_sig))));
    
    Reference::create_type_reference(&func_sig_kind_hash, heap)
}

