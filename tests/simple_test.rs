#![feature(plugin)]

#[plugin]
extern crate rtmpl;

use rtmpl::Model;
use rtmpl::attr_type::AttrType;

#[model]
struct TestModel {
    str: &'static str,
    string: String,
    usize: usize,
    uint8:  u8,
    uint16: u16,
    uint32: u32,
    uint64: u64,
    isize: isize,
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
        usize: std::usize::MAX,
        uint8:  std::u8 ::MAX,
        uint16: std::u16::MAX,
        uint32: std::u32::MAX,
        uint64: std::u64::MAX,
        isize: std::isize::MIN,
        int8:  std::i8 ::MIN,
        int16: std::i16::MIN,
        int32: std::i32::MIN,
        int64: std::i64::MIN,
    };

    // XXX: The none::<> stuff is a hack until Uniform Function Call Syntax (UFCS) is implemented,
    // see http://stackoverflow.com/questions/23674538/name-resolving-error-when-implementing-static-method-from-a-trait
    assert!(<TestModel as Model>::__get_type("str").unwrap() == AttrType::String);
    assert!(<TestModel as Model>::__get_type("string").unwrap() == AttrType::String);
    assert!(<TestModel as Model>::__get_type("usize" ).unwrap() == AttrType::Uint);
    assert!(<TestModel as Model>::__get_type("uint8" ).unwrap() == AttrType::Uint);
    assert!(<TestModel as Model>::__get_type("uint16").unwrap() == AttrType::Uint);
    assert!(<TestModel as Model>::__get_type("uint32").unwrap() == AttrType::Uint);
    assert!(<TestModel as Model>::__get_type("uint64").unwrap() == AttrType::Uint);
    assert!(<TestModel as Model>::__get_type("isize" ).unwrap() == AttrType::Int);
    assert!(<TestModel as Model>::__get_type("int8" ).unwrap() == AttrType::Int);
    assert!(<TestModel as Model>::__get_type("int16").unwrap() == AttrType::Int);
    assert!(<TestModel as Model>::__get_type("int32").unwrap() == AttrType::Int);
    assert!(<TestModel as Model>::__get_type("int64").unwrap() == AttrType::Int);
    assert!(<TestModel as Model>::__get_type("n/a").is_none());
    assert!(model.__get_string("str").unwrap() == "str");
    assert!(model.__get_string("string").unwrap() == "String");
    assert!(model.__get_uint("usize"  ).unwrap() as usize == ::std::usize::MAX);
    assert!(model.__get_uint("uint8" ).unwrap() as u8   == ::std::u8  ::MAX);
    assert!(model.__get_uint("uint16").unwrap() as u16  == ::std::u16 ::MAX);
    assert!(model.__get_uint("uint32").unwrap() as u32  == ::std::u32 ::MAX);
    assert!(model.__get_uint("uint64").unwrap() as u64  == ::std::u64 ::MAX);
    assert!(model.__get_int("isize").unwrap() as isize == ::std::isize::MIN);
    assert!(model.__get_int("int8" ).unwrap() as i8  == ::std::i8 ::MIN);
    assert!(model.__get_int("int16").unwrap() as i16 == ::std::i16::MIN);
    assert!(model.__get_int("int32").unwrap() as i32 == ::std::i32::MIN);
    assert!(model.__get_int("int64").unwrap() as i64 == ::std::i64::MIN);
}
