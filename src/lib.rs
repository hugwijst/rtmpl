#![feature(macro_rules)]
#![feature(trace_macros)]

#![feature(plugin_registrar)]

extern crate syntax;
extern crate rustc;

use rustc::plugin::Registry;
use syntax::ast::{CookedStr, Expr, Ident, Item, ItemStruct, LitInt, LitStr, MetaItem, MetaWord, MutImmutable, Plus, UnsuffixedIntLit};
use syntax::ast::{NamedField, StructField, UnnamedField};
use syntax::ast::{TyPath, PathSegment};
use syntax::codemap::{Span, Spanned};
use syntax::ext::base::{Decorator, ExtCtxt};
use syntax::ext::build::AstBuilder;
use syntax::ext::deriving::generic::{combine_substructure, MethodDef, Substructure, TraitDef};
use syntax::ext::deriving::generic::ty::{borrowed, borrowed_explicit_self, borrowed_self, nil_ty, LifetimeBounds, Literal, Path, Self};
use syntax::parse::token;
use syntax::parse::token::InternedString;
use syntax::ptr::P;

#[deriving(Show,PartialEq)]
pub enum AttrType {
    StringType,
    IntType,
    UintType,
}

pub trait Model {
    fn __get_type(attr: &str, _ignored: Option<Self>) -> Option<AttrType>;
    fn __get_string(&self, attr: &str) -> &str;
    fn __get_int(&self, attr: &str) -> int;
    fn __get_uint(&self, attr: &str) -> uint;
}

fn path_to_attr_type(path: &syntax::ast::Path) -> Option<AttrType> {
    let path_str : Vec<String> = path.segments.iter().map(
        |&PathSegment { identifier: Ident { name: ref n, .. }, .. }| n.as_str().to_string()
    ).collect();

    match path_str.connect("::").as_slice() {
        "std::string::String" | "String" => Some(StringType),
        "int" | "i8" | "i16" | "i32" => Some(IntType),
        "uint" | "u8" | "u16" | "u32" => Some(UintType),
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

    let field_type = match field.node.ty.node {
        TyPath(ref path, _, _) => path_to_attr_type(path),
        ref ty => {
            ecx.span_warn(field.node.ty.span, format!("Unsupported type expression {} for automatic model instantiation!", ty).as_slice());
            None
        }
    };

    (field_name, field_type)
}

fn model_template(ecx: &mut ExtCtxt, span: Span, meta_item: &MetaItem, item: &Item, push: |P<Item>|) {
    match meta_item.node {
        MetaWord(_) => {
            let pat_attr_type = Path::new(vec!("rtmpl", "AttrType"));
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
                let mut fields : Vec<syntax::ast::Arm> = struct_def.fields.iter().filter_map(|field| {
                    let info = get_field_info(cx, field);

                    match info {
                        // Known type, named field
                        (Some(ident), Some(ty)) => {
                            let ident_istring = token::get_ident(ident);
                            // The pattern of the match arm, should be the name of the
                            // field
                            let pattern = vec!(cx.pat_lit(span, cx.expr_str(span, ident_istring)));

                            let field_type_str = format!("{}", ty);
                            // The global path to a value of the AttrType enum
                            let fielt_type_path = cx.path_global(span, vec!(
                                    cx.ident_of("rtmpl"),
                                    cx.ident_of(field_type_str.as_slice())
                                    ));
                            // The expression of the match arm
                            let expression = cx.expr_some(span, cx.expr_path(fielt_type_path));

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

            fn get_str(cx: &mut ExtCtxt, span: Span, substr: &Substructure) -> P<Expr> {
                cx.expr_lit(span, LitStr(InternedString::new("Test"), CookedStr))
            }

            fn get_int(cx: &mut ExtCtxt, span: Span, substr: &Substructure) -> P<Expr> {
                cx.expr_lit(span, LitInt(42, UnsuffixedIntLit(Plus)))
            }

            fn get_uint(cx: &mut ExtCtxt, span: Span, substr: &Substructure) -> P<Expr> {
                cx.expr_lit(span, LitInt(43, UnsuffixedIntLit(Plus)))
            }

            macro_rules! md (
                ($name:expr, $expl_self:expr, $args:expr, $ret:expr, $f:ident) => { {
                    //let inline = cx.meta_word(span, InternedString::new("inline"));
                    let attrs = Vec::new();//vec!(cx.attribute(span, inline));
                    MethodDef {
                        name: $name,
                        generics: LifetimeBounds::empty(),
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
                None,
                vec!(
                    borrowed( box Literal( Path::new(vec!("str")) )),
                    Literal( Path::new_(vec!("std", "option", "Option"), None, vec!(box Self), true) ),
                ),
                lit_option_attr_type,
                get_type
            );

            macro_rules! md_get (
                ($name:expr, $expl_self:expr, $ret:expr, $f:ident) => {
                    md!(
                        $name,
                        $expl_self,
                        vec!(borrowed( box Literal( Path::new(vec!("str")) ))),
                        $ret,
                        $f
                    )
                }
                );

            let trait_def = TraitDef {
                span: span,
                attributes: Vec::new(),
                path: Path::new(vec!("rtmpl", "Model")),
                additional_bounds: Vec::new(),
                generics: LifetimeBounds::empty(),
                methods: vec!(
                    md_get_type,
                    md_get!("__get_string", borrowed_explicit_self(), borrowed( box Literal(Path::new(vec!("str"))) ), get_str),
                    md_get!("__get_int", borrowed_explicit_self(), Literal(Path::new(vec!("int"))), get_int),
                    md_get!("__get_uint", borrowed_explicit_self(), Literal(Path::new(vec!("uint"))), get_uint)
                    )
            };
            trait_def.expand(ecx, meta_item, item, push)
        }
        _ => {
            ecx.span_err(meta_item.span, "unsupported trait list in `model`");
        }
        /*
        MetaNameValue(_, ref l) => {
            ecx.span_err(l.span, "unexpected value in `model`");
        }
        MetaList(_, ref titems) if titems.len() == 0 => {
            ecx.span_warn(meta_item.span, "empty trait list in `model`");
        }
        MetaList(_, ref titems) => {
            for titem in titems.iter().rev() {
                match titem.node {
                    MetaNameValue(ref tname, _) |
                        MetaList(ref tname, _) |
                        MetaWord(ref tname) => {
                            macro_rules! expand(($func:path) => ($func(ecx, titem.span,
                                                                       &**titem, item,
                                                                       |i| push(i))));
                            match tname.get() {
                                //"Clone" => expand!(clone::expand_deriving_clone),
                                //"Hash" => expand!(hash::expand_deriving_hash),
                                //"Encodable" => expand!(encodable::expand_deriving_encodable),
                                //"Decodable" => expand!(decodable::expand_deriving_decodable),
                                //"PartialEq" => expand!(eq::expand_deriving_eq),
                                //"Eq" => expand!(totaleq::expand_deriving_totaleq),
                                //"PartialOrd" => expand!(ord::expand_deriving_ord),
                                //"Ord" => expand!(totalord::expand_deriving_totalord),
                                //"Rand" => expand!(rand::expand_deriving_rand),
                                //"Show" => expand!(show::expand_deriving_show),
                                //"Zero" => expand!(zero::expand_deriving_zero),
                                //"Default" => expand!(default::expand_deriving_default),
                                //"FromPrimitive" => expand!(primitive::expand_deriving_from_primitive),
                                //"Send" => expand!(bounds::expand_deriving_bound),
                                //"Sync" => expand!(bounds::expand_deriving_bound),
                                //"Copy" => expand!(bounds::expand_deriving_bound),
                                ref tname => {
                                    ecx.span_err(titem.span,
                                                format!("unknown `model` trait: `{}`",
                                                        *tname).as_slice());
                                }
                            };
                        }
                }
            }
        }
        */
    };
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(token::intern("model"), Decorator(box model_template));
}

