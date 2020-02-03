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

const STRING_TY: &str = "pel::lang::String";

pub(super) fn prelude(kind_table: &mut KindTable, heap: &mut Heap) -> HashMap<String, Value> {
    let mut env = HashMap::new();

    kind_table.create(Kind::ScalarType(ScalarType::BoolType));
    env.insert(String::from("bool"), Value::create_type_reference(ScalarType::bool_kind_hash(), &heap));

    kind_table.create(Kind::ScalarType(ScalarType::CharType));
    env.insert(String::from("char"), Value::create_type_reference(ScalarType::char_kind_hash(), &heap));

    kind_table.create(Kind::ScalarType(ScalarType::IntegerType));
    env.insert(String::from("int"), Value::create_type_reference(ScalarType::int_kind_hash(), &heap));

    kind_table.create(Kind::ScalarType(ScalarType::LongType));
    env.insert(String::from("long"), Value::create_type_reference(ScalarType::long_kind_hash(), &heap));

    kind_table.create(Kind::ScalarType(ScalarType::FloatType));
    env.insert(String::from("float"), Value::create_type_reference(ScalarType::float_kind_hash(), &heap));

    kind_table.create(Kind::ScalarType(ScalarType::DoubleType));
    env.insert(String::from("double"), Value::create_type_reference(ScalarType::double_kind_hash(), &heap));

   
    let print_func_val = Value::create_function(Function::NativeFunction(print_nat_fn()), &kind_table, &heap);
    env.insert("print".into(), print_func_val);

    env
}

fn print_nat_fn() -> NativeFunction {
    NativeFunction {
        name: "print".into(),
        func: |interp: &mut Interpreter, args: Vec<Value>| {
            if args.len() != 1 {
                let message = format!(
                    "invalid number of parameters function call. expected 1, got {}",
                    args.len()
                );
                panic!(message);
            }

            let reference = args[0].to_ref().unwrap();
            if reference.ty != KindHash::from(STRING_TY) {
                panic!("expected an instance of String but got: {:?}", reference.ty);
            }

            let obj_instance_arc = interp.heap.load(reference.address).to_object_instance().unwrap();
            let char_array = obj_instance_arc
                .read()
                .unwrap()
                .fields
                .get("internal")
                .unwrap();
            let rust_str = pel_utils::pel_char_array_to_rust_string(char_array);
            println!("{}", rust_str);
            None
        },
    }
}

