use attr::Attr;
use attr_type::AttrType;

pub trait Model {
    fn __get_type(attr: &str, _ignored: Option<Self>) -> Option<AttrType>;
    fn __get_attr(&self, attr: &str) -> Option<Box<Attr>>;
    fn __get_string<'a>(&'a self, attr: &str) -> Option<&'a str>;
    fn __get_int(&self, attr: &str) -> Option<i64>;
    fn __get_uint(&self, attr: &str) -> Option<u64>;
}

#[derive(Copy,PartialEq)]
pub struct EmptyModel;

impl Model for EmptyModel {
    fn __get_type(_attr: &str, _ignored: Option<EmptyModel>) -> Option<AttrType> {
        // This model does not have any attributes
        return None;
    }
    fn __get_attr(&self, _attr: &str) -> Option<Box<Attr>> {
        None
    }
    fn __get_string<'a>(&'a self, _attr: &str) -> Option<&'a str> {
        None
    }
    fn __get_int(&self, _attr: &str) -> Option<i64> {
        None
    }
    fn __get_uint(&self, _attr: &str) -> Option<u64> {
        None
    }
}
