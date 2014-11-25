#![feature(macro_rules)]
#![feature(phase)]

#[phase(plugin, link)]
extern crate rtmpl;

use rtmpl::{EmptyModel, Model, StringTemplate};


#[model]
struct WorldModel {
    world: String,
    min_temp: u32,
    max_temp: i32,
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
    let templ_str = "Hello {world}!";

    let tmpl = StringTemplate::from_str(templ_str);

    let model = WorldModel { world: String::from_str("Earth"), min_temp: 184, max_temp: 330 };

    // XXX: The none::<> stuff is a hack until Uniform Function Call Syntax (UFCS) is implemented,
    // see http://stackoverflow.com/questions/23674538/name-resolving-error-when-implementing-static-method-from-a-trait
    assert!(Model::__get_type("world", None::<WorldModel>).unwrap() == rtmpl::model::StringType);
    assert!(Model::__get_type("min_temp", None::<WorldModel>).unwrap() == rtmpl::model::UintType);
    assert!(Model::__get_type("max_temp", None::<WorldModel>).unwrap() == rtmpl::model::IntType);
    assert!(Model::__get_type("n/a", None::<WorldModel>).is_none());
    assert!(model.__get_string("world") == "Earth");
    assert!(model.__get_uint("min_temp") == 184);
    assert!(model.__get_int("max_temp") == 330);

    assert!(tmpl.render(&model).as_slice() == "Hello Earth!");
}

#[test]
fn from_file() {
    //trace_macros!(true);
    let template = template_from_file!("hello.tmpl");

    let res = template.render(&EmptyModel);

    assert!(res.as_slice() == "Hello World!\n");
}

