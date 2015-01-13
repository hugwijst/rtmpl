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
