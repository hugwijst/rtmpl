#![feature(plugin)]

//#![feature(collections)]

/** Test renaming of types */

#[plugin]
extern crate rtmpl;

use rtmpl::Model;
use rtmpl::attr_type::AttrType;
use rtmpl::attr::Attr;

use std::string::String as MyString;

#[model]
struct MyStringModel {
    string: MyString,
}

#[test]
fn renamed_type() {
    let model = MyStringModel {
        string: "foo".to_string(),
    };

    assert!(<MyStringModel as Model>::__get_type("string").unwrap() == AttrType::String);

    macro_rules! assert_attr ( ($to_attr: expr, $attr: pat) => ({
        let panic_msg = concat!("Attributes are not equal: \"", stringify!($to_attr), ".to_attr()\", \"", stringify!($attr), "\".");
        if let $attr = *$to_attr {} else { panic!( panic_msg ) };
    }) );

    assert_attr!(model.__get_attr("string").unwrap(), Attr::String("foo"));
}
