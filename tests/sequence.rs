#![feature(box_syntax)]
#![feature(plugin)]

#[plugin]
extern crate rtmpl;

use rtmpl::{Model};
use rtmpl::model::AttrType;

#[model]
struct BasicTypeSequenceModel {
    strs: Vec<&'static str>,
    strings: Vec<String>,
    usizes:  Vec<usize>,
    uints8:  Vec<u8>,
    uints16: Vec<u16>,
    uints32: Vec<u32>,
    uints64: Vec<u64>,
    isizes: Vec<isize>,
    ints8:  Vec<i8>,
    ints16: Vec<i16>,
    ints32: Vec<i32>,
    ints64: Vec<i64>,
}

#[test]
fn simple_sequence_model() {
    let model = BasicTypeSequenceModel {
        strs: vec!["str 1", "str 2"],
        strings: vec!["string 1".to_string(), "string 2".to_string()],
        usizes:  vec![42,  0, std::usize::MIN, std::usize::MAX],
        uints8:  vec![8,  0, std::u8::MIN, std::u8::MAX],
        uints16: vec![16, 0, std::u16::MIN, std::u16::MAX],
        uints32: vec![32, 0, std::u32::MIN, std::u32::MAX],
        uints64: vec![64, 0, std::u64::MIN, std::u64::MAX],
        isizes: vec![-42,  0, std::isize::MIN, std::isize::MAX],
        ints8:  vec![-8,  0, std::i8::MIN, std::i8::MAX],
        ints16: vec![-16, 0, std::i16::MIN, std::i16::MAX],
        ints32: vec![-32, 0, std::i32::MIN, std::i32::MAX],
        ints64: vec![-64, 0, std::i64::MIN, std::i64::MAX],
    };

    assert!(Model::__get_type("strs", None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::String));
    assert!(Model::__get_type("strings", None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::String));
    assert!(Model::__get_type("usizes",   None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(Model::__get_type("uints8",  None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(Model::__get_type("uints16", None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(Model::__get_type("uints32", None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(Model::__get_type("uints64", None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(Model::__get_type("isizes",  None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Int));
    assert!(Model::__get_type("ints8",  None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Int));
    assert!(Model::__get_type("ints16", None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Int));
    assert!(Model::__get_type("ints32", None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Int));
    assert!(Model::__get_type("ints64", None::<BasicTypeSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Int));

    let attr = model.__get_attr("strs").unwrap();
    let attr_vals : Vec<&str> = attr.iter().map(|ref attr| attr.get_string()).collect();
    assert!(attr_vals == vec!["str 1", "str 2"]);

    let attr = model.__get_attr("strings").unwrap();
    let attr_vals : Vec<&str> = attr.iter().map(|ref attr| attr.get_string()).collect();
    assert!(attr_vals == vec!["string 1", "string 2"]);

    let attr = model.__get_attr("usizes").unwrap();
    let attr_vals : Vec<u64> = attr.iter().map(|ref attr| attr.get_uint()).collect();
    assert!(attr_vals == vec![42, 0, std::usize::MIN as u64, std::usize::MAX as u64]);

    let attr = model.__get_attr("uints8").unwrap();
    let attr_vals : Vec<u64> = attr.iter().map(|ref attr| attr.get_uint()).collect();
    assert!(attr_vals == vec![8, 0, std::u8::MIN as u64, std::u8::MAX as u64]);

    let attr = model.__get_attr("uints16").unwrap();
    let attr_vals : Vec<u64> = attr.iter().map(|ref attr| attr.get_uint()).collect();
    assert!(attr_vals == vec![16, 0, std::u16::MIN as u64, std::u16::MAX as u64]);

    let attr = model.__get_attr("uints32").unwrap();
    let attr_vals : Vec<u64> = attr.iter().map(|ref attr| attr.get_uint()).collect();
    assert!(attr_vals == vec![32, 0, std::u32::MIN as u64, std::u32::MAX as u64]);

    let attr = model.__get_attr("uints64").unwrap();
    let attr_vals : Vec<u64> = attr.iter().map(|ref attr| attr.get_uint()).collect();
    assert!(attr_vals == vec![64, 0, std::u64::MIN as u64, std::u64::MAX as u64]);

    let attr = model.__get_attr("isizes").unwrap();
    let attr_vals : Vec<i64> = attr.iter().map(|ref attr| attr.get_int()).collect();
    assert!(attr_vals == vec![-42, 0, std::isize::MIN as i64, std::isize::MAX as i64]);

    let attr = model.__get_attr("ints8").unwrap();
    let attr_vals : Vec<i64> = attr.iter().map(|ref attr| attr.get_int()).collect();
    assert!(attr_vals == vec![-8, 0, std::i8::MIN as i64, std::i8::MAX as i64]);

    let attr = model.__get_attr("ints16").unwrap();
    let attr_vals : Vec<i64> = attr.iter().map(|ref attr| attr.get_int()).collect();
    assert!(attr_vals == vec![-16, 0, std::i16::MIN as i64, std::i16::MAX as i64]);

    let attr = model.__get_attr("ints32").unwrap();
    let attr_vals : Vec<i64> = attr.iter().map(|ref attr| attr.get_int()).collect();
    assert!(attr_vals == vec![-32, 0, std::i32::MIN as i64, std::i32::MAX as i64]);

    let attr = model.__get_attr("ints64").unwrap();
    let attr_vals : Vec<i64> = attr.iter().map(|ref attr| attr.get_int()).collect();
    assert!(attr_vals == vec![-64, 0, std::i64::MIN as i64, std::i64::MAX as i64]);
}

#[model]
struct SequenceOfSequenceModel {
    usizes: Vec<Vec<u64>>,
}

#[test]
fn sequence_of_sequence_model() {
    let vec_of_vecs = vec![
        vec![1, 2, 3],
        vec![2, 3],
        vec![],
        vec![4],
    ];

    let model = SequenceOfSequenceModel {
        usizes: vec_of_vecs.clone(),
    };

    assert!(Model::__get_type("usizes", None::<SequenceOfSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Sequence(box AttrType::Uint)));

    let attr = model.__get_attr("usizes").unwrap();
    let attr_vals : Vec<Vec<_>> = attr
        .iter().map(|ref attr| attr
             .iter().map(|ref attr| {
                 attr.get_uint()
             }).collect()
        ).collect();
    assert!(attr_vals == vec_of_vecs);
}

#[model]
struct UnusualSequenceModel {
    d_list: std::collections::DList<u64>,
}

#[test]
fn unusual_sequence_model() {
    let vals1 = vec![1, 2, 3, 4];

    let model = UnusualSequenceModel {
        d_list: vals1.iter().map(|&v| v).collect(),
    };

    assert!(Model::__get_type("d_list", None::<UnusualSequenceModel>).unwrap() == AttrType::Sequence(box AttrType::Uint));

    let attr = model.__get_attr("d_list").unwrap();
    let attr_vals : Vec<_> = attr.iter().map(|ref attr| attr.get_uint()).collect();
    assert!(attr_vals == vec![1, 2, 3, 4]);
}
