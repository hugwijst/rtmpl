use rustc::plugin::Registry;
use syntax::ast::Arm;
use syntax::ast::{AngleBracketedParameters, AngleBracketedParameterData};
use syntax::ast::{Expr, Ident, Item, ItemStruct, MetaItem, MetaWord};
use syntax::ast::MutImmutable;
use syntax::ast::{NamedField, StructField, UnnamedField};
use syntax::ast::{Ty_, MutTy, PathSegment};
use syntax::ast::Path as AstPath;
use syntax::ast::Ty as AstTy;
use syntax::ast::UnOp;
use syntax::codemap::{Span};
use syntax::ext::base::{Decorator, ExtCtxt};
use syntax::ext::build::AstBuilder;
use syntax::ext::deriving::generic::{combine_substructure, MethodDef, Substructure, SubstructureFields, TraitDef};
use syntax::ext::deriving::generic::ty::{borrowed, borrowed_explicit_self, Borrowed, LifetimeBounds, Literal, Path, Ptr, Ty};
use syntax::parse::token;
use syntax::ptr::P;

use attr_type::AttrType;

fn ty_to_attr_type(ty: &P<AstTy>) -> Option<AttrType> {
    match ty.node {
        Ty_::TyPath(ref path, _) => path_to_attr_type(path),
        Ty_::TyRptr(_, MutTy { ty: ref ty_, .. }) => {
            // TODO: figure out how we want to handle pointers
            ty_to_attr_type(ty_)
        }
        ref ty => panic!("Unsupported type! ty: {:?}", ty),
    }
}

fn path_to_attr_type(path: &AstPath) -> Option<AttrType> {
    let path_str : Vec<String> = path.segments.iter().map(
        |&PathSegment { identifier: Ident { name: ref n, .. }, .. }| n.as_str().to_string()
    ).collect();

    match path_str.connect("::").as_slice() {
        "std::string::String" | "String" | "str" => Some(AttrType::String),
        "isize" | "i8" | "i16" | "i32" | "i64" => Some(AttrType::Int),
        "usize" | "u8" | "u16" | "u32" | "u64" => Some(AttrType::Uint),
        "std::vec::Vec" | "Vec"
            | "std::collections::DList" | "DList" => {
            let param_attr = match path.segments.iter().last() {
                Some(&PathSegment {
                    parameters: AngleBracketedParameters(AngleBracketedParameterData {
                        types: ref tys,
                        ..
                    }),
                    ..
                }) => {
                    assert!(tys.len() == 1, "Paths must have one and only one type parameter");
                    ty_to_attr_type(&tys[0])
                },
                segm => panic!("There should be a angle brackets in Vec type: {:?}", segm),
            };

            match param_attr {
                Some(p) => Some( AttrType::Sequence(box p) ),
                None => None
            }
        }
        _ => None,
    }
}

// Get the name and type of each field
// TODO: add support for unnamed fields?
// TODO: find a better way to identify types
// TODO: annotate fields with attributes to pass extra information
fn get_field_info(field: &StructField) -> (Option<Ident>, Option<AttrType>) {
    let field_name = match field.node.kind {
        NamedField(ident, _) => Some(ident),
        UnnamedField(_) => None,
    };

    let field_type = ty_to_attr_type(&field.node.ty);

    (field_name, field_type)
}

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
                    let info = get_field_info(field);

                    match info {
                        // Known type, named field
                        (Some(ident), Some(ty)) => {
                            let ident_istring = token::get_ident(ident);
                            // The pattern of the match arm, should be the name of the
                            // field
                            let pattern = vec!(cx.pat_lit(span, cx.expr_str(span, ident_istring)));

                            // The global path to a value of the AttrType enum
                            let field_type_expr = ty.to_expr(cx, span);
                            // The expression of the match arm
                            let expression = cx.expr_some(span, field_type_expr);

                            Some( cx.arm(span, pattern, expression) )
                        },
                        // Either unknown type or unnamed field
                        (id, ty) => {
                            if ty.is_none() {
                                // Unknown type, generate warning
                                cx.span_warn(field.node.ty.span, format!("Unsupported attribute type for automatic model instantiation!").as_slice());
                            }
                            if id.is_none() {
                                cx.span_warn(span, format!("Unnamed fields are not (yet) supported for model instantiation.").as_slice());
                            }
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

            fn match_for_type(is_ty: MatchType,
                              str_fields: &Vec<StructField>,
                              cx: &mut ExtCtxt,
                              span: Span,
                              substr: &Substructure,
                              arm_expr_fn: GetArmExpr) -> P<Expr> {
                // Get the name and type of each named field
                let mut fields : Vec<Arm> = str_fields.iter().filter_map(|field| {
                    let info = get_field_info(field);

                    match info {
                        // Known type, named field
                        (Some(ident), Some(ref field_ty)) if is_ty(field_ty) => {
                            let ident_istring = token::get_ident(ident);
                            // The pattern of the match arm, should be the name of the
                            // field
                            let pattern = vec!(cx.pat_lit(span, cx.expr_str(span, ident_istring)));

                            // The expression of the match arm
                            let expression = arm_expr_fn(span, cx, ident, field_ty);

                            Some( cx.arm(span, pattern, expression) )
                        },
                        // We don't care for other types or unnamed fields
                        _ => None
                    }
                }).collect();

                // Add the final arm: "_ => None"
                fields.push(cx.arm(span, vec!(cx.pat_wild(span)), cx.expr_none(span)));

                // Return the constructed match statement
                cx.expr_match(span,
                    substr.nonself_args[0].clone(),
                    fields,
                )
            }

            let get_attr = |&: cx: &mut ExtCtxt, span: Span, substr: &Substructure| -> P<Expr> {
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
                cx.expr_match(span,
                    substr.nonself_args[0].clone(),
                    arms,
                )
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

