use attr_type::AttrType;
use model::Model;

pub trait Template<M: Model> {
    fn render(model: &M) -> String;
}

enum Expr {
    Field { name: String, ty: AttrType },
}

enum Item {
    Text { text: String },
    Expr { expr: Expr },
}

pub struct StringTemplate<M: Model> {
    ast: Vec<Item>,
}

impl <M: Model> StringTemplate<M> {
    pub fn from_str(template: &str) -> StringTemplate<M> {
        enum State { Text, Expr };

        let mut work = template;
        let mut state = State::Text;
        let mut ast = Vec::new();

        loop {
            state = match state {
                State::Text => {
                    let v : Vec<&str> = work.splitn(1, '{').collect();

                    work = match v.as_slice() {
                        [text] => {
                            ast.push(Item::Text { text: text.to_string() });
                            break;
                        }
                        [text, pos_expr] => {
                            ast.push(Item::Text { text: text.to_string() });
                            pos_expr
                        }
                        _ => unreachable!("Because split called with n == 1.")
                    };

                    State::Expr
                }
                State::Expr => {
                    let v : Vec<&str> = work.splitn(1, '}').collect();

                    work = match v.as_slice() {
                        [_] => {
                            panic!("No closing delimiter for \"{\"!");
                        }
                        [expr, text] => {
                            let ty = Model::__get_type(expr, None::<M>);
                            assert!(ty.is_some());

                            ast.push(Item::Expr { expr: Expr::Field{ name: expr.to_string(), ty: ty.unwrap() } } );
                            text
                        }
                        _ => unreachable!("Because split called with n == 1.")
                    };

                    State::Text
                }
            };
        }

        StringTemplate { ast: ast }
    }

    pub fn render(&self, model: &M) -> String {
        let mut res = String::new();

        for ref item in self.ast.iter() {
            match *item {
                &Item::Text { ref text } => res.push_str(text.as_slice()),
                &Item::Expr { expr: Expr::Field { ref name, ref ty } } => match ty {
                    &AttrType::String => res.push_str(model.__get_string(name.as_slice()).unwrap()),
                    &AttrType::Int => res.push_str(model.__get_int(name.as_slice()).unwrap().to_string().as_slice()),
                    &AttrType::Uint => res.push_str(model.__get_uint(name.as_slice()).unwrap().to_string().as_slice()),
                    &AttrType::Sequence(_) => (),
                }
            }
        }
        res
    }
}

