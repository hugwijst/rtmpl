use attr::Attr;
use attr_type::AttrType;

pub trait Model {
    fn __get_type(attr: &str) -> Option<AttrType>;
    fn __get_attr(&self, attr: &str) -> Option<Box<Attr>>;
}

#[derive(Copy,PartialEq)]
pub struct EmptyModel;

impl Model for EmptyModel {
    fn __get_type(_attr: &str) -> Option<AttrType> {
        // This model does not have any attributes
        return None;
    }
    fn __get_attr(&self, _attr: &str) -> Option<Box<Attr>> {
        None
    }
}
