#![feature(macro_rules)]
#![feature(phase)]

#[phase(plugin, link)]
extern crate rtmpl;

use rtmpl::{EmptyModel, Model, StringTemplate};

#[test]
fn empty_string() {
    let templ = "";

    assert!(StringTemplate::from_str(templ).render(&EmptyModel).as_slice() == "");
}

#[test]
fn hello_world() {
    let templ = "hello world";

    assert!(StringTemplate::from_str(templ).render(&EmptyModel).as_slice() == "hello world");
}

// Test file inclusion
// TODO: Should be moved to library proper?
macro_rules! template_from_file( ($file:expr) => (
    StringTemplate::from_str(include_str!($file))
) )

#[test]
fn from_file() {
    //trace_macros!(true);
    let template = template_from_file!("hello.tmpl");

    let res = template.render(&EmptyModel);

    assert!(res.as_slice() == "Hello World!\n");
}


// Test template with a model with all basic types
#[model]
struct BasicTypesModel {
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
fn test_basic_types() {
    let model = BasicTypesModel {
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

    let template_str = "str: {str}
string: {string}
uint: {uint}
uint8: {uint8}
uint16: {uint16}
uint32: {uint32}
uint64: {uint64}
int: {int}
int8: {int8}
int16: {int16}
int32: {int32}
int64: {int64}";

    let res = StringTemplate::from_str(template_str).render(&model);

    assert_eq!(
        res,
        format!(
"str: str
string: String
uint: {}
uint8: {}
uint16: {}
uint32: {}
uint64: {}
int: {}
int8: {}
int16: {}
int32: {}
int64: {}",
            std::uint::MAX,
            std::u8 ::MAX,
            std::u16::MAX,
            std::u32::MAX,
            std::u64::MAX,
            std::int::MIN,
            std::i8 ::MIN,
            std::i16::MIN,
            std::i32::MIN,
            std::i64::MIN,
        )
    );

}
