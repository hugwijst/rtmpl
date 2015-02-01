use std::collections::DList;
use std::fmt::{Debug, Error, Formatter};
use std::iter::IteratorExt;

use attr_type::{AsAttrType, AttrType};

pub struct SeqIter<'a> {
    attr_type: AttrType,
    iter: Box<Iterator<Item = Box<Attr<'a>>> + 'a>,
}

impl<'a> Debug for SeqIter<'a> {
    fn fmt(&self, fmtr: &mut Formatter) -> Result<(), Error> {
        try!(fmtr.write_str("SeqIter { attr_type: "));
        try!(self.attr_type.fmt(fmtr));
        try!(fmtr.write_str("}"));

        Ok( () )
    }
}

impl<'a> Iterator for SeqIter<'a> {
    type Item = Box<Attr<'a>>;
    fn next(&mut self) -> Option<Box<Attr<'a>>> {
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

#[derive(Debug)]
pub enum Attr<'a> {
    String(&'a str),
    Int(i64),
    Uint(u64),
    Sequence(SeqIter<'a>),
    //Map,
    //Set,
    //Model,
}

impl<'a> Attr<'a> {
    fn get_type(&self) -> AttrType {
        match *self {
            Attr::String(_) => AttrType::String,
            Attr::Int(_) => AttrType::Int,
            Attr::Uint(_) => AttrType::Uint,
            Attr::Sequence(ref iter) => AttrType::Sequence(box iter.attr_type.clone()),
        }
    }
}

pub trait ToAttr {
    fn to_attr(&self) -> Box<Attr>;
}

impl<'_> ToAttr for &'_ str {
    fn to_attr<'a>(&'a self) -> Box<Attr<'a>> {
        return box Attr::String(*self)
    }
}

impl ToAttr for String {
    fn to_attr(&self) -> Box<Attr> {
        return box Attr::String(self.as_slice())
    }
}

macro_rules! to_attr ( ($attr: ident, $t1:ty, $t2:ty) => {
    impl ToAttr for $t1 {
        fn to_attr(&self) -> Box<Attr> {
            return box Attr::$attr(*self as $t2)
        }
    }
} );

to_attr!(Int, i8,  i64);
to_attr!(Int, i16, i64);
to_attr!(Int, i32, i64);
to_attr!(Int, i64, i64);
to_attr!(Int, isize, i64);

to_attr!(Uint, u8,  u64);
to_attr!(Uint, u16, u64);
to_attr!(Uint, u32, u64);
to_attr!(Uint, u64, u64);
to_attr!(Uint, usize, u64);

impl <A> ToAttr for Vec<A> where A: ToAttr, A: AsAttrType {
    fn to_attr<'a>(&'a self) -> Box<Attr<'a>> {
        box Attr::Sequence(SeqIter {
            attr_type: <A as AsAttrType>::as_attr_type(),
            iter: box self.iter().map(|a: &'a A| a.to_attr()),
        })
    }
}

impl <A: ToAttr> ToAttr for DList<A> where A: ToAttr, A: AsAttrType {
    fn to_attr<'a>(&'a self) -> Box<Attr<'a>> {
        box Attr::Sequence(SeqIter {
            attr_type: <A as AsAttrType>::as_attr_type(),
            iter: box self.iter().map(|a: &'a A| a.to_attr()),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test_attr ( ($to_attr: expr, $attr: pat) => {
        if let $attr = *$to_attr {
        } else {
            panic!( concat!("Attributes are not equal: \"", stringify!($to_attr), ".to_attr()\", \"", stringify!($attr), "\".") )
        };
    } );

    #[test]
    fn to_attr_str() {
        test_attr!("foo".to_attr(), Attr::String("foo"));
        test_attr!("".to_attr(), Attr::String(""));
    }

    #[test]
    fn to_attr_string() {
        test_attr!("foo".to_string().to_attr(), Attr::String("foo"));
        test_attr!("".to_string().to_attr(), Attr::String(""));
    }

    #[test]
    fn to_attr_ints() {
        test_attr!(15is.to_attr(), Attr::Int(15i64));
        test_attr!(15i8.to_attr(), Attr::Int(15i64));
        test_attr!(15i16.to_attr(), Attr::Int(15i64));
        test_attr!(15i32.to_attr(), Attr::Int(15i64));
        test_attr!(15i64.to_attr(), Attr::Int(15i64));
    }

    #[test]
    fn to_attr_uints() {
        test_attr!(42us.to_attr(), Attr::Uint(42u64));
        test_attr!(42u8.to_attr(), Attr::Uint(42u64));
        test_attr!(42u16.to_attr(), Attr::Uint(42u64));
        test_attr!(42u32.to_attr(), Attr::Uint(42u64));
        test_attr!(42u64.to_attr(), Attr::Uint(42u64));
    }

    #[test]
    fn to_attr_vec() {
        match *vec!(2is, 4is).to_attr() {
            Attr::Sequence(iter) => {
                let vec: Vec<_> = iter.collect();

                test_attr!(vec[0], Attr::Int(2i64));
                test_attr!(vec[1], Attr::Int(4i64));
            }
            _ => panic!("Expected sequence"),
        }

        match *vec!("foo", "bar").to_attr() {
            Attr::Sequence(iter) => {
                let vec: Vec<_> = iter.collect();

                test_attr!(vec[0], Attr::String("foo"));
                test_attr!(vec[1], Attr::String("bar"));
            }
            _ => panic!("Expected sequence"),
        }

        match *vec!(vec!("foo", "bar"), vec!("oof")).to_attr() {
            Attr::Sequence(mut iter) => {
                match *iter.next().unwrap() {
                    Attr::Sequence(iter_) => {
                        let vec_: Vec<_> = iter_.collect();
                        assert_eq!(vec_.len(), 2);
                        test_attr!(vec_[0], Attr::String("foo"));
                        test_attr!(vec_[1], Attr::String("bar"));
                    }
                    _ => panic!("Expected Sequence"),
                };

                match *iter.next().unwrap() {
                    Attr::Sequence(iter_) => {
                        let vec_: Vec<_> = iter_.collect();
                        assert_eq!(vec_.len(), 1);
                        test_attr!(vec_[0], Attr::String("oof"));
                    }
                    _ => panic!("Expected Sequence"),
                };

                assert!(iter.next().is_none());
            }
            _ => panic!("Expected sequence"),
        }
    }
}
