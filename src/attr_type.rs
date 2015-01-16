use syntax::ast::{Expr, Ident, UnOp};
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ext::build::AstBuilder;
use syntax::ptr::P;

#[derive(PartialEq,Show)]
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
            cx.ident_of("attr_type"),
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

trait AsAttrType {
    fn as_attr_type() -> AttrType;
}

macro_rules! impl_attr_type ( ( $for_ty:ty, $attr_ty:ident ) => {
    impl AsAttrType for $for_ty {
        fn as_attr_type() -> AttrType {
            AttrType::$attr_ty
        }
    }
} );

impl_attr_type!(String, String);
impl_attr_type!(str, String);

impl_attr_type!(isize, Int);
impl_attr_type!(i8, Int);
impl_attr_type!(i16, Int);
impl_attr_type!(i32, Int);
impl_attr_type!(i64, Int);

impl_attr_type!(usize, Uint);
impl_attr_type!(u8, Uint);
impl_attr_type!(u16, Uint);
impl_attr_type!(u32, Uint);
impl_attr_type!(u64, Uint);

macro_rules! impl_attr_type_seq ( ( $for_ty:ty ) => {
    impl<T: AsAttrType> AsAttrType for $for_ty {
        fn as_attr_type() -> AttrType {
            AttrType::Sequence( Box::new(<T as AsAttrType>::as_attr_type()) )
        }
    }
} );

impl_attr_type_seq!(Vec<T>);
impl_attr_type_seq!(::std::collections::DList<T>);

impl<'a, T: AsAttrType> AsAttrType for &'a T {
    fn as_attr_type() -> AttrType {
        <T as AsAttrType>::as_attr_type()
    }
}
