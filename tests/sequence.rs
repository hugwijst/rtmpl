#![feature(macro_rules)]
#![feature(phase)]

#[phase(plugin, link)]
extern crate rtmpl;

use rtmpl::{Model, StringTemplate};
use rtmpl::model::AttrType;

#[model]
struct SimpleSequenceModel {
    strings: Vec<String>,
    uints: Vec<u8>,
    ints: Vec<i32>,
}

#[test]
fn simple_sequence_model() {
    let model = SimpleSequenceModel {
        strings: vec!["string 1".to_string(), "string 2".to_string()],
        uints: vec![15, 0, 42],
        ints: vec![-1501, 3, std::i32::MAX, std::i32::MIN],
    };

    assert!(Model::__get_type("strings", None::<SimpleSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::String));
    assert!(Model::__get_type("uints", None::<SimpleSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(Model::__get_type("ints", None::<SimpleSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Int));
}
