use std::collections::{HashMap};
use std::sync::{Arc, RwLock};

use crate::evaluator::evaluator::{Array, Heap, HeapReference, KindHash, Item, ObjectInstance, Reference, Scalar, Value};
use crate::evaluator::prelude::{CHAR_TY, STRING_TY, STRING_FIELD};

pub(crate) fn rust_string_to_pel_string(input: &String, heap: &mut Heap) -> Reference {
    let arr_ref = alloc_array(input.chars().collect(), heap);
    alloc_obj_instance(arr_ref, heap)
}

fn alloc_array(chars: Vec<char>, heap: &mut Heap) -> Reference {
    let char_vals = chars
        .iter()
        .map(|c| Value::Scalar(Scalar::Char(*c)))
        .collect();

    let array = Array {
        ty: CHAR_TY.into(),
        length: chars.len() as u32,
        values: char_vals,
    };

    let addr = heap.alloc();
    heap.store(addr, Item::Array(Arc::new(RwLock::new(array))));

    Reference::HeapReference(HeapReference {
        ty: format!("[{}]", CHAR_TY), // TODO -> don't hard code this?
        is_self: false,
        address: addr,
        size: std::u32::MAX,
    })
}

fn alloc_obj_instance(arr_ref: Reference, heap: &mut Heap) -> Reference {
    let mut fields = HashMap::new();
    fields.insert(STRING_FIELD.into(), Value::Reference(arr_ref));
    let obj_instance = ObjectInstance {
        ty: STRING_TY.into(),
        contract_ty: None,
        fields,
    };

    let addr = heap.alloc();
    heap.store(addr, Item::ObjectInstance(Arc::new(RwLock::new(obj_instance))));

    Reference::HeapReference(HeapReference {
        ty: STRING_TY.into(),
        is_self: false,
        address: addr,
        size: std::u32::MAX,
    })
}

pub(crate) fn pel_string_to_rust_string(item: &Item, heap: &mut Heap) -> Option<String> {
    to_string_object_of_type(item, STRING_TY.into())
        .and_then(|ps| get_char_array_of_pel_string(ps, heap))
        .and_then(pel_char_array_to_rust_string)
}

fn to_string_object_of_type(item: &Item, ty: KindHash) -> Option<Arc<RwLock<ObjectInstance>>> {
    let obj = item.to_object_instance()?;
    if obj.read().unwrap().ty != ty {
        return None;
    }

    Some(obj)
}

fn get_char_array_of_pel_string(pel_string: Arc<RwLock<ObjectInstance>>, heap: &mut Heap) -> Option<Arc<RwLock<Array>>> {
    let arr_ref = pel_string
        .read()
        .unwrap()
        .fields
        .get("internal".into())
        .unwrap()
        .to_ref()
        .unwrap();

    let arr_heap_ref = arr_ref
        .to_heap_ref()
        .unwrap();

    heap.load_array(arr_heap_ref.address)
}

fn pel_char_array_to_rust_string(arr: Arc<RwLock<Array>>) -> Option<String> {
    // TODO -> type check?
    Some(arr
         .read()
         .unwrap()
         .values
         .iter()
         .map(|v| v.to_scalar().unwrap())
         .map(|s| s.to_char().unwrap())
         .collect())
}

