use rustc::plugin::Registry;
use syntax::ast::Arm;
use syntax::ast::{Expr, Ident, Item, ItemStruct, MetaItem, MetaWord};
use syntax::ast::{NamedField, UnnamedField};
use syntax::ast::Visibility;
use syntax::codemap::{Span};
use syntax::ext::base::{Decorator, ExtCtxt};
use syntax::ext::build::AstBuilder;
use syntax::ext::deriving::generic::{combine_substructure, MethodDef, Substructure, SubstructureFields, TraitDef};
use syntax::ext::deriving::generic::ty::{borrowed, borrowed_explicit_self, LifetimeBounds, Literal, Path, Ty};
use syntax::parse::token;
use syntax::ptr::P;

use attr_type::AttrType;

fn model_template(ecx: &mut ExtCtxt, span: Span, meta_item: &MetaItem, item: &Item, mut push: Box<FnMut(P<Item>)>) {
    match meta_item.node {
        MetaWord(_) => {
            // Perform checks on struct format
            // TODO: Add support from more struct types
            // TODO: Check if there are more unsupported cases not handled here
            let struct_def = match item.node {
                ItemStruct(ref struct_def, _) => struct_def,
                _ => panic!("We only support structures for now."),
            };

            let get_type = |&: cx: &mut ExtCtxt, span: Span, substr: &Substructure| -> P<Expr> {
                let mut fields : Vec<Arm> = struct_def.fields.iter().filter_map(|field| {
                    match field.node.kind {
                        // Known type, named field
                        NamedField(ident, _) => {
                            let ident_istring = token::get_ident(ident);
                            // The pattern of the match arm, should be the name of the
                            // field
                            let pattern = vec!(cx.pat_lit(span, cx.expr_str(span, ident_istring)));

                            /*
                            // The global path to a value of the AttrType enum
                            let field_type_expr = ty.to_expr(cx, span);
                            // The expression of the match arm
                            let expression = cx.expr_some(span, field_type_expr);

                            */

                            let self_type = field.node.ty.clone();
                            let trait_ref = cx.trait_ref(cx.path_global(span, vec![
                                cx.ident_of("rtmpl"),
                                cx.ident_of("attr_type"),
                                cx.ident_of("AsAttrType"),
                            ]));
                            let method_ident = cx.ident_of("as_attr_type");
                            let qpath = cx.qpath_all(self_type, P(trait_ref), method_ident, Vec::new(), Vec::new(), Vec::new());
                            let res = cx.expr_call(span, cx.expr_qpath(span, qpath), vec!());
                            let expression = cx.expr_some(span, res);

                            Some( cx.arm(span, pattern, expression) )
                        },
                        // Either unknown type or unnamed field
                        UnnamedField(_) => {
                            cx.span_warn(span, format!("Unnamed fields are not (yet) supported for model instantiation.").as_slice());
                            None
                        }
                    }
                }).collect();

                // Add the final arm: "_ => None"
                fields.push(cx.arm(span, vec!(cx.pat_wild(span)), cx.expr_none(span)));

                // Return the constructed match statement
                cx.expr_match(span,
                    substr.nonself_args[0].clone(),
                    fields,
                )
            };

            type MatchType = fn(&AttrType) -> bool;
            type GetArmExpr = fn(Span, &ExtCtxt, Ident, &AttrType) -> P<Expr>;

            let get_attr = |&: cx: &mut ExtCtxt, span: Span, substr: &Substructure| -> P<Expr> {
                // Use the ToAttr trait
                let use_to_attr = cx.stmt_item(
                    span,
                    cx.item_use_simple(
                        span,
                        Visibility::Inherited,
                        cx.path_global(span, vec![cx.ident_of("rtmpl"), cx.ident_of("attr"), cx.ident_of("ToAttr")]),
                    )
                );
                // Get the name and type of each named field
                let fields = match *substr.fields {
                    SubstructureFields::Struct(ref field_info) => field_info,
                    _ => panic!("We only support structures for now."),
                };

                let mut arms : Vec<Arm> = fields.iter().map(|field| {
                    let ident = token::get_ident( field.name.unwrap() );
                    let pattern = vec!(cx.pat_lit(span, cx.expr_str(span, ident)));

                    let call = cx.expr_method_call(span, field.self_.clone(), cx.ident_of("to_attr"), Vec::new());
                    let expression = cx.expr_some(span, call);

                    cx.arm(span, pattern, expression)
                }).collect();

                // Add the final arm: "_ => None"
                arms.push(cx.arm(span, vec!(cx.pat_wild(span)), cx.expr_none(span)));

                // Return the constructed match statement
                let ret_expr = cx.expr_match(span,
                    substr.nonself_args[0].clone(),
                    arms,
                );

                cx.expr_block( cx.block(span, vec![use_to_attr], Some(ret_expr)) )
            };

            macro_rules! md (
                ($name:expr, $bounds:expr, $expl_self:expr, $args:expr, $ret:expr, $f:ident) => { {
                    //let inline = cx.meta_word(span, InternedString::new("inline"));
                    let attrs = Vec::new();//vec!(cx.attribute(span, inline));
                    MethodDef {
                        name: $name,
                        generics: LifetimeBounds { lifetimes: vec!( ("'a", Vec::new()) ), bounds: $bounds },
                        explicit_self: $expl_self,
                        args: $args,
                        ret_ty: $ret,
                        attributes: attrs,
                        combine_substructure: combine_substructure(box |a, b, c| {
                            $f(a, b, c)
                        })
                    }
                } }
                );

            macro_rules! md_get (
                ($name:expr, $bounds:expr, $expl_self:expr, $ret:expr, $f:ident) => {
                    md!(
                        $name,
                        $bounds,
                        $expl_self,
                        vec!(borrowed( box Literal( Path::new(vec!("str")) ))),
                        $ret,
                        $f
                    )
                }
                );

            let lit_attr_type = Literal( Path::new(vec!("rtmpl", "attr_type", "AttrType")) );
            let lit_option_attr_type = Literal( Path::new_(vec!("std", "option", "Option"), None, vec!(box lit_attr_type), true) );

            let lit_attr = box Literal( Path::new(vec!["rtmpl", "attr", "Attr"]) );
            let lit_box_attr = Literal( Path::new_(vec!["std", "boxed", "Box"], None, vec![lit_attr], true) );

            fn option(ty : Ty) -> Ty {
                Literal( Path::new_(vec!["std", "option", "Option"], None, vec![box ty], true) )
            }

            let trait_def = TraitDef {
                span: span,
                attributes: Vec::new(),
                path: Path::new(vec!("rtmpl", "Model")),
                additional_bounds: Vec::new(),
                generics: LifetimeBounds::empty(),
                methods: vec!(
                    md_get!("__get_type", Vec::new(), None, lit_option_attr_type, get_type),
                    md_get!("__get_attr", Vec::new(), borrowed_explicit_self(), option(lit_box_attr), get_attr),
                ),
                associated_types: Vec::new(),
            };
            trait_def.expand(ecx, meta_item, item, |i| push(i))
        }
        _ => {
            ecx.span_err(meta_item.span, "unsupported trait list in `model`");
        }
    };
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(token::intern("model"), Decorator(box model_template));
}

