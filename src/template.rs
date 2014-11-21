use model::Model;

pub trait Template<M: Model> {
    fn render(model: &M) -> String;
}

pub struct StringTemplate<M: Model> {
    res: String,
}

impl <M: Model> StringTemplate<M> {
    pub fn from_str(template: &str) -> StringTemplate<M> {
        StringTemplate { res: String::from_str(template) }
    }

    pub fn render(&self, model: &M) -> &str {
        self.res.as_slice()
    }
}

