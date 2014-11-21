#![feature(macro_rules)]
#![feature(phase)]

#[phase(plugin, link)]
extern crate rtmpl;

use rtmpl::Model;
use rtmpl::EmptyModel;

trait Template<M: Model> {
    fn render(model: &M) -> String;
}

struct TestTemplate<M: Model> {
    res: String,
}

#[model]
struct WorldModel {
    world: String,
    min_temp: u32,
    max_temp: i32,
}

macro_rules! template_from_file( ($file:expr) => (
    TestTemplate::from_str(include_str!($file))
) )

impl <M: Model> TestTemplate<M> {
    fn from_str(template: &str) -> TestTemplate<M> {
        TestTemplate { res: String::from_str(template) }
    }

    fn render(&self, model: &M) -> &str {
        self.res.as_slice()
    }
}

#[test]
fn simple_str() {
    let templ = "hello world";

    assert!(TestTemplate::from_str(templ).render(&EmptyModel) == "hello world");
}

#[test]
fn compound_str() {
    let templ_str = "Hello {world}!";

    let tmpl = TestTemplate::from_str(templ_str);

    /* GOAL TEMPLATE CODE:
       struct Template__WorldModel {
           fn render(&self, model: &M) -> String {
               format!("Hello {:s}!", model.world);
           }
       }
     */

    let model = WorldModel { world: String::from_str("Earth"), min_temp: 184, max_temp: 330 };

    // XXX: The none::<> stuff is a hack until Uniform Function Call Syntax (UFCS) is implemented,
    // see http://stackoverflow.com/questions/23674538/name-resolving-error-when-implementing-static-method-from-a-trait
    assert!(Model::__get_type("world", None::<WorldModel>).unwrap() == rtmpl::StringType);
    assert!(Model::__get_type("min_temp", None::<WorldModel>).unwrap() == rtmpl::UintType);
    assert!(Model::__get_type("max_temp", None::<WorldModel>).unwrap() == rtmpl::IntType);
    assert!(Model::__get_type("n/a", None::<WorldModel>).is_none());
    assert!(model.__get_string("world") == "Earth".to_string());
    assert!(model.__get_uint("min_temp") == 184);
    assert!(model.__get_int("max_temp") == 330);

    assert!(tmpl.render(&model) == "Hello Earth!");
}

#[test]
fn from_file() {
    //trace_macros!(true);
    let template = template_from_file!("hello.tmpl");

    let res = template.render(&EmptyModel);

    assert!(res == "Hello World!\n");
}

