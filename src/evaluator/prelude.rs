use std::collections::HashMap;

use super::evaluator::{Kind, KindHash, KindTable, Item, NativeFunction, ObjectInstance, ScalarType, Value};
use crate::evaluator::interpreter::Interpreter;

pub(super) fn prelude(kind_table: &mut KindTable) -> HashMap<String, KindHash> {
    let mut env = HashMap::new();

    kind_table.create(Kind::ScalarType(ScalarType::BoolType));
    env.insert(String::from("bool"), ScalarType::bool_kind_hash());

    kind_table.create(Kind::ScalarType(ScalarType::CharType));
    env.insert(String::from("char"), ScalarType::char_kind_hash());

    kind_table.create(Kind::ScalarType(ScalarType::IntegerType));
    env.insert(String::from("int"), ScalarType::int_kind_hash());

    kind_table.create(Kind::ScalarType(ScalarType::LongType));
    env.insert(String::from("long"), ScalarType::long_kind_hash());

    kind_table.create(Kind::ScalarType(ScalarType::FloatType));
    env.insert(String::from("float"), ScalarType::float_kind_hash());

    kind_table.create(Kind::ScalarType(ScalarType::DoubleType));
    env.insert(String::from("double"), ScalarType::double_kind_hash());

    let print_symbol_id = kind_table.create(Kind::NativeFunction(print_nat_fn()));
    env.insert("print".into(), print_symbol_id);

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
            if reference.ty != STRING_TY {
                panic!("expected an instance of String but got: {:?}", reference.ty);
            }

            let obj_instance_arc = interp.heap.load(reference.address).to_object_instance().unwrap();
            let char_array = obj_instance_arc
                .read()
                .unwrap()
                .fields
                .get("__internal__")
                .unwrap();
            let rust_str = utils::pel_char_array_to_rust_string(char_array);
            println!("{}", rust_str);
        },
    }
}

