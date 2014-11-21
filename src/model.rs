#[deriving(Show,PartialEq)]
pub enum AttrType {
    StringType,
    IntType,
    UintType,
}

pub trait Model {
    fn __get_type(attr: &str, _ignored: Option<Self>) -> Option<AttrType>;
    fn __get_string(&self, attr: &str) -> String;
    fn __get_int(&self, attr: &str) -> i64;
    fn __get_uint(&self, attr: &str) -> u64;
}

pub struct EmptyModel;

impl Model for EmptyModel {
    fn __get_type(_attr: &str, _ignored: Option<EmptyModel>) -> Option<AttrType> {
        // This model does not have any attributes
        return None;
    }
    fn __get_string(&self, _attr: &str) -> String {
        panic!("This function should not be called with an incorrect identifier!");
    }
    fn __get_int(&self, _attr: &str) -> i64 {
        panic!("This function should not be called with an incorrect identifier!");
    }
    fn __get_uint(&self, _attr: &str) -> u64 {
        panic!("This function should not be called with an incorrect identifier!");
    }
}
