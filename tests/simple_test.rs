#![feature(plugin)]

#[plugin]
extern crate rtmpl;

use rtmpl::Model;
use rtmpl::attr_type::AttrType;
use rtmpl::attr::{Attr, ToAttr};

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

    macro_rules! assert_attr ( ($to_attr: expr, $attr: pat) => ({
        let panic_msg = concat!("Attributes are not equal: \"", stringify!($to_attr), ".to_attr()\", \"", stringify!($attr), "\".");
        if let $attr = *$to_attr {} else { panic!( panic_msg ) };
    }) );

    const US_MAX: u64 = ::std::usize::MAX as u64;
    const U8_MAX: u64 = ::std::u8::MAX as u64;
    const U16_MAX: u64 = ::std::u16::MAX as u64;
    const U32_MAX: u64 = ::std::u32::MAX as u64;

    const IS_MIN: i64 = ::std::isize::MIN as i64;
    const I8_MIX: i64 = ::std::i8::MIN as i64;
    const I16_MIN: i64 = ::std::i16::MIN as i64;
    const I32_MIN: i64 = ::std::i32::MIN as i64;

    assert_attr!(model.__get_attr("str").unwrap(), Attr::String("str"));
    assert_attr!(model.__get_attr("string").unwrap(), Attr::String("String"));
    assert_attr!(model.__get_attr("usize" ).unwrap(), Attr::Uint(US_MAX));
    assert_attr!(model.__get_attr("uint8" ).unwrap(), Attr::Uint(U8_MAX));
    assert_attr!(model.__get_attr("uint16").unwrap(), Attr::Uint(U16_MAX));
    assert_attr!(model.__get_attr("uint32").unwrap(), Attr::Uint(U32_MAX));
    assert_attr!(model.__get_attr("uint64").unwrap(), Attr::Uint(::std::u64::MAX));
    assert_attr!(model.__get_attr("isize").unwrap(), Attr::Int(IS_MIN));
    assert_attr!(model.__get_attr("int8" ).unwrap(), Attr::Int(I8_MIN));
    assert_attr!(model.__get_attr("int16").unwrap(), Attr::Int(I16_MIN));
    assert_attr!(model.__get_attr("int32").unwrap(), Attr::Int(I32_MIN));
    assert_attr!(model.__get_attr("int64").unwrap(), Attr::Int(::std::i64::MIN));
}
