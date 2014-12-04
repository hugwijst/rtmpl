#![feature(macro_rules)]
#![feature(phase)]

#[phase(plugin, link)]
extern crate rtmpl;

use rtmpl::{EmptyModel, Model, StringTemplate};


#[model]
struct TestModel {
    string: String,
    uint32: u32,
    int32: i32,
}

macro_rules! template_from_file( ($file:expr) => (
    StringTemplate::from_str(include_str!($file))
) )

#[test]
fn simple_str() {
    let templ = "hello world";

    assert!(StringTemplate::from_str(templ).render(&EmptyModel).as_slice() == "hello world");
}

#[test]
fn compound_str() {
    let templ_str = "Hello {string}!";

    let tmpl = StringTemplate::from_str(templ_str);

    let model = TestModel { string: "World".to_string(), uint32: 184, int32: -330 };

    // XXX: The none::<> stuff is a hack until Uniform Function Call Syntax (UFCS) is implemented,
    // see http://stackoverflow.com/questions/23674538/name-resolving-error-when-implementing-static-method-from-a-trait
    assert!(Model::__get_type("string", None::<TestModel>).unwrap() == rtmpl::model::AttrType::String);
    assert!(Model::__get_type("uint32", None::<TestModel>).unwrap() == rtmpl::model::AttrType::Uint);
    assert!(Model::__get_type("int32", None::<TestModel>).unwrap() == rtmpl::model::AttrType::Int);
    assert!(Model::__get_type("n/a", None::<TestModel>).is_none());
    assert!(model.__get_string("string") == "World");
    assert!(model.__get_uint("uint32") == 184);
    assert!(model.__get_int("int32") == -330);

    assert!(tmpl.render(&model).as_slice() == "Hello World!");
}

#[test]
fn from_file() {
    //trace_macros!(true);
    let template = template_from_file!("hello.tmpl");

    let res = template.render(&EmptyModel);

    assert!(res.as_slice() == "Hello World!\n");
}

