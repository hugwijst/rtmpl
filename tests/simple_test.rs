#![feature(plugin)]

#[plugin]
extern crate rtmpl;

use rtmpl::{EmptyModel, Model, StringTemplate};

#[model]
struct TestModel {
    str: &'static str,
    string: String,
    uint: uint,
    uint8:  u8,
    uint16: u16,
    uint32: u32,
    uint64: u64,
    int: int,
    int8:  i8,
    int16: i16,
    int32: i32,
    int64: i64,
}

#[test]
fn compound_str() {
    let model = TestModel {
        str: "str",
        string: "String".to_string(),
        uint: std::uint::MAX,
        uint8:  std::u8 ::MAX,
        uint16: std::u16::MAX,
        uint32: std::u32::MAX,
        uint64: std::u64::MAX,
        int:   std::int::MIN,
        int8:  std::i8 ::MIN,
        int16: std::i16::MIN,
        int32: std::i32::MIN,
        int64: std::i64::MIN,
    };

    // XXX: The none::<> stuff is a hack until Uniform Function Call Syntax (UFCS) is implemented,
    // see http://stackoverflow.com/questions/23674538/name-resolving-error-when-implementing-static-method-from-a-trait
    assert!(Model::__get_type("str", None::<TestModel>).unwrap() == rtmpl::model::AttrType::String);
    assert!(Model::__get_type("string", None::<TestModel>).unwrap() == rtmpl::model::AttrType::String);
    assert!(Model::__get_type("uint"  , None::<TestModel>).unwrap() == rtmpl::model::AttrType::Uint);
    assert!(Model::__get_type("uint8" , None::<TestModel>).unwrap() == rtmpl::model::AttrType::Uint);
    assert!(Model::__get_type("uint16", None::<TestModel>).unwrap() == rtmpl::model::AttrType::Uint);
    assert!(Model::__get_type("uint32", None::<TestModel>).unwrap() == rtmpl::model::AttrType::Uint);
    assert!(Model::__get_type("uint64", None::<TestModel>).unwrap() == rtmpl::model::AttrType::Uint);
    assert!(Model::__get_type("int"  , None::<TestModel>).unwrap() == rtmpl::model::AttrType::Int);
    assert!(Model::__get_type("int8" , None::<TestModel>).unwrap() == rtmpl::model::AttrType::Int);
    assert!(Model::__get_type("int16", None::<TestModel>).unwrap() == rtmpl::model::AttrType::Int);
    assert!(Model::__get_type("int32", None::<TestModel>).unwrap() == rtmpl::model::AttrType::Int);
    assert!(Model::__get_type("int64", None::<TestModel>).unwrap() == rtmpl::model::AttrType::Int);
    assert!(Model::__get_type("n/a", None::<TestModel>).is_none());
    assert!(model.__get_string("str").unwrap() == "str");
    assert!(model.__get_string("string").unwrap() == "String");
    assert!(model.__get_uint("uint"  ).unwrap() as uint == ::std::uint::MAX);
    assert!(model.__get_uint("uint8" ).unwrap() as u8   == ::std::u8  ::MAX);
    assert!(model.__get_uint("uint16").unwrap() as u16  == ::std::u16 ::MAX);
    assert!(model.__get_uint("uint32").unwrap() as u32  == ::std::u32 ::MAX);
    assert!(model.__get_uint("uint64").unwrap() as u64  == ::std::u64 ::MAX);
    assert!(model.__get_int("int"  ).unwrap() as int == ::std::int::MIN);
    assert!(model.__get_int("int8" ).unwrap() as i8  == ::std::i8 ::MIN);
    assert!(model.__get_int("int16").unwrap() as i16 == ::std::i16::MIN);
    assert!(model.__get_int("int32").unwrap() as i32 == ::std::i32::MIN);
    assert!(model.__get_int("int64").unwrap() as i64 == ::std::i64::MIN);
}
