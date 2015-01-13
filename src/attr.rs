use std::collections::DList;
use std::iter::IteratorExt;

fn error(ty_expected: &str, ty_used: &str) -> ! {
    panic!("Unable to use type \"{}\" as type \"{}\"!", ty_expected, ty_used);
}

pub trait Attr<'a> {
    fn get_type(&self) -> &'static str;

    fn get_string(&self) -> &'a str { error("String", self.get_type()) }
    fn get_int(&self) -> i64 { error("String", self.get_type()) }
    fn get_uint(&self) -> u64 { error("String", self.get_type()) }
    fn iter(&self) -> SeqIter<'a> { error("String", self.get_type()) }
}

pub trait ToAttr {
    fn to_attr(&self) -> Box<Attr>;
}

struct StringAttr<'a> {
    data: &'a str,
}

impl<'a> Attr<'a> for StringAttr<'a> {
    fn get_type(&self) -> &'static str { "String" }

    fn get_string(&self) -> &'a str {
        self.data
    }
}

impl<'b> ToAttr for &'b str {
    fn to_attr(&self) -> Box<Attr> {
        return box StringAttr { data: *self } as Box<Attr>
    }
}

impl ToAttr for String {
    fn to_attr(&self) -> Box<Attr> {
        return box StringAttr { data: self.as_slice() } as Box<Attr>
    }
}

struct IntAttr<'a> {
    data: i64,
}

impl<'a> Attr<'a> for IntAttr<'a> {
    fn get_type(&self) -> &'static str { "Int" }

    fn get_int(&self) -> i64 {
        self.data
    }
}

macro_rules! to_attr ( ($struct_:ident, $t1:ty, $t2:ty) => {
    impl ToAttr for $t1 {
        fn to_attr(&self) -> Box<Attr> {
            return box $struct_ { data: *self as $t2 } as Box<Attr>
        }
    }
} );

to_attr!(IntAttr, i8,  i64);
to_attr!(IntAttr, i16, i64);
to_attr!(IntAttr, i32, i64);
to_attr!(IntAttr, i64, i64);
to_attr!(IntAttr, isize, i64);

struct UintAttr<'a> {
    data: u64,
}

impl<'a> Attr<'a> for UintAttr<'a> {
    fn get_type(&self) -> &'static str { "Uint" }

    fn get_uint(&self) -> u64 {
        self.data
    }
}

to_attr!(UintAttr, u8,  u64);
to_attr!(UintAttr, u16, u64);
to_attr!(UintAttr, u32, u64);
to_attr!(UintAttr, u64, u64);
to_attr!(UintAttr, usize, u64);

pub struct SeqAttr<'a, A: ToAttr, I: Iterator<Item = &'a A> + Clone> {
    pub data: Box<I>,
}

pub struct SeqIter<'a> {
    iter: Box<Iterator<Item = Box<Attr<'a> + 'a>> + 'a>,
}

impl<'a> Iterator for SeqIter<'a> {
    type Item = Box<Attr<'a> + 'a>;
    fn next(&mut self) -> Option<Box<Attr<'a> + 'a>> {
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl <'a, A: ToAttr + 'a, I: Iterator<Item = &'a A> + Clone + 'a> Attr<'a> for SeqAttr<'a, A, I> {
    fn get_type(&self) -> &'static str { "Sequence" }

    fn iter(&self) -> SeqIter<'a> {
        SeqIter { iter: box self.data.clone().map(|a: &'a A| a.to_attr()) }
    }
}

impl <A: ToAttr> ToAttr for Vec<A> {
    fn to_attr(&self) -> Box<Attr> {
        return box SeqAttr { data: box self.iter() } as Box<Attr>
    }
}

impl <A: ToAttr> ToAttr for DList<A> {
    fn to_attr(&self) -> Box<Attr> {
        return box SeqAttr { data: box self.iter() } as Box<Attr>
    }
}

