#![feature(plugin_registrar)]
#![feature(macro_rules)]

extern crate syntax;
extern crate rustc;

pub use model::{Model, EmptyModel};

pub mod plugin;
pub mod model;

