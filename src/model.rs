use syntax::ast::{Expr, Ident, UnOp};
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ext::build::AstBuilder;
use syntax::ptr::P;

use attr::Attr;

#[deriving(PartialEq,Show)]
pub enum AttrType {
    String,
    Int,
    Uint,
    Sequence(Box<AttrType>),
    //Map,
    //Set,
}

impl AttrType {
    fn get_idents(&self, cx: &ExtCtxt) -> Vec<Ident> {
        use self::AttrType::*;

        let name = match self {
            &String => "String",
            &Int    => "Int",
            &Uint   => "Uint",
            &Sequence(_) => "Sequence",
        };

        vec![
            cx.ident_of("rtmpl"),
            cx.ident_of("model"),
            cx.ident_of("AttrType"),
            cx.ident_of(name),
        ]
    }

    pub fn to_expr(&self, cx: &ExtCtxt, span: Span) -> P<Expr> {
        use self::AttrType::*;

        match self {
            &String | &Int | &Uint => cx.expr_path(cx.path_global(span, self.get_idents(cx))),
            &Sequence(ref attr_ty) => {
                //let ty = cx.ty_path(attr_ty.to_path(cx, span), None);

                //println!("{}", cx.parse_expr("box std::string::String::new()".to_string()));

                cx.expr_call_global(span, self.get_idents(cx), vec![cx.expr_unary(span, UnOp::UnUniq, attr_ty.to_expr(cx, span))])
            }
        }
    }
}

pub trait Model {
    fn __get_type(attr: &str, _ignored: Option<Self>) -> Option<AttrType>;
    fn __get_attr(&self, attr: &str) -> Option<Box<Attr>>;
    fn __get_string<'a>(&'a self, attr: &str) -> Option<&'a str>;
    fn __get_int(&self, attr: &str) -> Option<i64>;
    fn __get_uint(&self, attr: &str) -> Option<u64>;
}

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
