#![feature(box_syntax)]
#![feature(plugin_registrar)]

extern crate syntax;
extern crate rustc;

pub use model::{Model, EmptyModel};
pub use template::{Template, StringTemplate};

pub mod attr;
pub mod plugin;
pub mod model;
pub mod template;

