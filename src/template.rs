use model::{AttrType, Model, StringType, IntType, UintType};

pub trait Template<M: Model> {
    fn render(model: &M) -> String;
}

enum Expr {
    ExprField { name: String, ty: AttrType },
}

enum Item {
    ItemText { text: String },
    ItemExpr { expr: Expr },
}

pub struct StringTemplate<M: Model> {
    ast: Vec<Item>,
}

impl <M: Model> StringTemplate<M> {
    pub fn from_str(template: &str) -> StringTemplate<M> {
        enum State { Text, Expr };

        let mut work = template;
        let mut state = Text;
        let mut ast = Vec::new();

        loop {
            state = match state {
                Text => {
                    let v : Vec<&str> = work.splitn(1, '{').collect();

                    work = match v.as_slice() {
                        [text] => {
                            ast.push(ItemText { text: text.to_string() });
                            break;
                        }
                        [text, pos_expr] => {
                            ast.push(ItemText { text: text.to_string() });
                            pos_expr
                        }
                        _ => unreachable!("Because split called with n == 1.")
                    };

                    Expr
                }
                Expr => {
                    let v : Vec<&str> = work.splitn(1, '}').collect();

                    work = match v.as_slice() {
                        [_] => {
                            panic!("No closing delimiter for \"{\"!");
                        }
                        [expr, text] => {
                            let ty = Model::__get_type(expr, None::<M>);
                            assert!(ty.is_some());

                            ast.push(ItemExpr { expr: ExprField{ name: expr.to_string(), ty: ty.unwrap() } } );
                            text
                        }
                        _ => unreachable!("Because split called with n == 1.")
                    };

                    Text
                }
            };
        }

        StringTemplate { ast: ast }
    }

    pub fn render(&self, model: &M) -> String {
        let mut res = String::new();

        for ref item in self.ast.iter() {
            match *item {
                &ItemText { ref text } => res.push_str(text.as_slice()),
                &ItemExpr { expr: ExprField { ref name, ty } } => match ty {
                    StringType => res.push_str(model.__get_string(name.as_slice()).as_slice()),
                    IntType => res.push_str(model.__get_int(name.as_slice()).to_string().as_slice()),
                    UintType => res.push_str(model.__get_uint(name.as_slice()).to_string().as_slice()),
                }
            }
        }
        res
    }
}

