use rustc::plugin::Registry;
use syntax::ast::Arm;
use syntax::ast::{AngleBracketedParameters, AngleBracketedParameterData};
use syntax::ast::{Expr, Ident, Item, ItemStruct, MetaItem, MetaWord};
use syntax::ast::MutImmutable;
use syntax::ast::{NamedField, StructField, UnnamedField};
use syntax::ast::{Ty, Ty_, MutTy, PathSegment};
use syntax::ast::Path as AstPath;
use syntax::ast::UnOp;
use syntax::codemap::{Span};
use syntax::ext::base::{Decorator, ExtCtxt};
use syntax::ext::build::AstBuilder;
use syntax::ext::deriving::generic::{combine_substructure, MethodDef, Substructure, TraitDef};
use syntax::ext::deriving::generic::ty::{borrowed, borrowed_explicit_self, Borrowed, LifetimeBounds, Literal, Path, Ptr, Self};
use syntax::parse::token;
use syntax::ptr::P;

use model::AttrType;

fn ty_to_attr_type(ty: &P<Ty>) -> Option<AttrType> {
    match ty.node {
        Ty_::TyPath(ref path, _) => path_to_attr_type(path),
        Ty_::TyRptr(_, MutTy { ty: ref ty_, .. }) => {
            // TODO: figure out how we want to handle pointers
            ty_to_attr_type(ty_)
        }
        ref ty => panic!("Unsupported type! ty: {}", ty),
    }
}

fn path_to_attr_type(path: &AstPath) -> Option<AttrType> {
    let path_str : Vec<String> = path.segments.iter().map(
        |&PathSegment { identifier: Ident { name: ref n, .. }, .. }| n.as_str().to_string()
    ).collect();

    match path_str.connect("::").as_slice() {
        "std::string::String" | "String" | "str" => Some(AttrType::String),
        "int" | "i8" | "i16" | "i32" | "i64" => Some(AttrType::Int),
        "uint" | "u8" | "u16" | "u32" | "u64" => Some(AttrType::Uint),
        "std::vec::Vec" | "Vec" => {
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
                segm => panic!("There should be a angle brackets in Vec type: {}", segm),
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
fn get_field_info(ecx: &mut ExtCtxt, field: &StructField) -> (Option<Ident>, Option<AttrType>) {
    let field_name = match field.node.kind {
        NamedField(ident, _) => Some(ident),
        UnnamedField(_) => None,
    };

    let field_type = ty_to_attr_type(&field.node.ty);

    if field_type == None {
        ecx.span_warn(field.node.ty.span, format!("Unsupported type expression {} for automatic model instantiation!", field.node.ty.node).as_slice());
    }

    (field_name, field_type)
}

fn model_template(ecx: &mut ExtCtxt, span: Span, meta_item: &MetaItem, item: &Item, push: |P<Item>|) {
    match meta_item.node {
        MetaWord(_) => {
            let pat_attr_type = Path::new(vec!("rtmpl", "model", "AttrType"));
            let lit_attr_type = Literal(pat_attr_type);
            let pat_option_attr_type = Path::new_(vec!("std", "option", "Option"), None, vec!(box lit_attr_type), true);
            let lit_option_attr_type = Literal(pat_option_attr_type);

            // Perform checks on struct format
            // TODO: Add support from more struct types
            // TODO: Check if there are more unsupported cases not handled here
            let struct_def = match item.node {
                ItemStruct(ref struct_def, _) => struct_def,
                _ => panic!("We only support structures for now."),
            };

            let get_type = |cx: &mut ExtCtxt, span: Span, substr: &Substructure| -> P<Expr> {
                // Get the name and type of each named field
                let mut fields : Vec<Arm> = struct_def.fields.iter().filter_map(|field| {
                    let info = get_field_info(cx, field);

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

            type MatchType<'a> = |&AttrType|: 'a -> bool;
            type GetArmExpr<'a> = |Span, &ExtCtxt, Ident, &AttrType|: 'a -> P<Expr>;

            fn match_for_type(is_ty: MatchType,
                              str_fields: &Vec<StructField>,
                              cx: &mut ExtCtxt,
                              span: Span,
                              substr: &Substructure,
                              arm_expr_fn: GetArmExpr) -> P<Expr> {
                // Get the name and type of each named field
                let mut fields : Vec<Arm> = str_fields.iter().filter_map(|field| {
                    let info = get_field_info(cx, field);

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
                let failure_str = "This function should not be called with an incorrect identifier!";
                let failure_istr = token::get_ident(cx.ident_of(failure_str));
                fields.push(cx.arm(span, vec!(cx.pat_wild(span)), cx.expr_fail(span, failure_istr)));

                // Return the constructed match statement
                cx.expr_match(span,
                    substr.nonself_args[0].clone(),
                    fields,
                )
            }

            let get_str = |cx: &mut ExtCtxt, span: Span, substr: &Substructure| -> P<Expr> {
                fn is_ty(ty: &AttrType) -> bool {
                    *ty == AttrType::String
                }
                fn arm_expr_fn(span: Span, cx: &ExtCtxt, ident: Ident, _ty: &AttrType) -> P<Expr> {
                    cx.expr_method_call(span, cx.expr_field_access(span, cx.expr_self(span), ident), cx.ident_of("as_slice"), Vec::new())
                };
                match_for_type(is_ty, &struct_def.fields, cx, span, substr, arm_expr_fn)
            };

            let get_int = |cx: &mut ExtCtxt, span: Span, substr: &Substructure| -> P<Expr> {
                fn is_ty(ty: &AttrType) -> bool {
                    *ty == AttrType::Int
                }
                fn arm_expr_fn(span: Span, cx: &ExtCtxt, ident: Ident, _ty: &AttrType) -> P<Expr> {
                    cx.expr_cast(span, cx.expr_field_access(span, cx.expr_self(span), ident), cx.ty_ident(span, cx.ident_of("i64")))
                };
                match_for_type(is_ty, &struct_def.fields, cx, span, substr, arm_expr_fn)
            };

            let get_uint = |cx: &mut ExtCtxt, span: Span, substr: &Substructure| -> P<Expr> {
                fn is_ty(ty: &AttrType) -> bool {
                    *ty == AttrType::Uint
                }
                fn arm_expr_fn(span: Span, cx: &ExtCtxt, ident: Ident, _ty: &AttrType) -> P<Expr> {
                    cx.expr_cast(span, cx.expr_field_access(span, cx.expr_self(span), ident), cx.ty_ident(span, cx.ident_of("u64")))
                };
                match_for_type(is_ty, &struct_def.fields, cx, span, substr, arm_expr_fn)
            };

            let get_attr = |cx: &mut ExtCtxt, span: Span, substr: &Substructure| -> P<Expr> {
                fn is_ty(ty: &AttrType) -> bool {
                    match ty {
                        &AttrType::Sequence(_) => true,
                        _ => false,
                    }
                }
                fn arm_expr_fn(span: Span, cx: &ExtCtxt, ident: Ident, ty: &AttrType) -> P<Expr> {
                    let fields = vec![
                        cx.field_imm(span, cx.ident_of("data_ty"), ty.to_expr(cx, span)),
                        cx.field_imm(span, cx.ident_of("data"), cx.expr_unary(span, UnOp::UnUniq, cx.expr_method_call(span, cx.expr_field_access(span, cx.expr_self(span), ident), cx.ident_of("iter"), Vec::new()))),
                    ];
                    let attr = cx.expr_struct(span, cx.path(span, vec!["rtmpl", "attr", "SeqAttr"].iter().map(|&s| cx.ident_of(s)).collect()), fields);

                    let attr_ty = cx.ty_path( cx.path(span, vec!["rtmpl", "attr", "Attr"].iter().map(|&s| cx.ident_of(s)).collect()) );
                    let box_ty = cx.ty_path( cx.path_all(
                        span,
                        true,
                        vec!["std", "boxed", "Box"].iter().map(|&s| cx.ident_of(s)).collect(),
                        Vec::new(),
                        vec![attr_ty]
                    ) );
                    cx.expr_cast(span, cx.expr_unary(span, UnOp::UnUniq, attr), box_ty)
                };
                match_for_type(is_ty, &struct_def.fields, cx, span, substr, arm_expr_fn)
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
                        combine_substructure: combine_substructure(|a, b, c| {
                            $f(a, b, c)
                        })
                    }
                } }
                );

            let md_get_type =  md!(
                "__get_type",
                Vec::new(),
                None,
                vec!(
                    borrowed( box Literal( Path::new(vec!("str")) )),
                    Literal( Path::new_(vec!("std", "option", "Option"), None, vec!(box Self), true) ),
                ),
                lit_option_attr_type,
                get_type
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

            let attr_type = box Literal( Path::new(vec!["rtmpl", "attr", "Attr"]) );
            let boxed_attr_type = Literal( Path::new_(vec!["std", "boxed", "Box"], None, vec![attr_type], true) );

            let trait_def = TraitDef {
                span: span,
                attributes: Vec::new(),
                path: Path::new(vec!("rtmpl", "Model")),
                additional_bounds: Vec::new(),
                generics: LifetimeBounds::empty(),
                methods: vec!(
                    md_get_type,
                    md_get!("__get_string", Vec::new(), Some(Some(Borrowed(Some("'a"), MutImmutable))), Ptr( box Literal(Path::new(vec!("str"))), Borrowed(Some("'a"), MutImmutable) ), get_str),
                    md_get!("__get_int", Vec::new(), borrowed_explicit_self(), Literal(Path::new(vec!("i64"))), get_int),
                    md_get!("__get_uint", Vec::new(), borrowed_explicit_self(), Literal(Path::new(vec!("u64"))), get_uint),
                    md_get!("__get_attr", Vec::new(), borrowed_explicit_self(), boxed_attr_type, get_attr),
                    )
            };
            trait_def.expand(ecx, meta_item, item, push)
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

