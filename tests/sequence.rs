#![feature(box_syntax)]
#![feature(plugin)]

#![feature(collections)]

#[plugin]
extern crate rtmpl;

use rtmpl::Model;
use rtmpl::attr::Attr;
use rtmpl::attr_type::AttrType;

#[model]
struct BasicTypeSequenceModel {
    strs: Vec<&'static str>,
    strings: Vec<String>,
    usizes:  Vec<usize>,
    uints8:  Vec<u8>,
    uints16: Vec<u16>,
    uints32: Vec<u32>,
    uints64: Vec<u64>,
    isizes: Vec<isize>,
    ints8:  Vec<i8>,
    ints16: Vec<i16>,
    ints32: Vec<i32>,
    ints64: Vec<i64>,
}

macro_rules! test_attr ( ($to_attr: expr, $attr: pat) => {
    if let $attr = *$to_attr {
    } else {
        panic!( concat!("Attributes are not equal: \"", stringify!($to_attr), ".to_attr()\", \"", stringify!($attr), "\".") )
    };
} );

macro_rules! test_attr_seq ( ($seq: expr, [ $($attr: pat),* ]) => {
    match $seq {
        Attr::Sequence(mut iter) => {
            $(
                test_attr!(iter.next().unwrap(), $attr);
            )*
            assert!(iter.next().is_none());
        }
        _ => panic!("Expected sequence"),
    }
} );


#[test]
fn simple_sequence_model() {
    let model = BasicTypeSequenceModel {
        strs: vec!["str 1", "str 2"],
        strings: vec!["string 1".to_string(), "string 2".to_string()],
        usizes:  vec![42,  0, std::usize::MIN, std::usize::MAX],
        uints8:  vec![8,  0, std::u8::MIN, std::u8::MAX],
        uints16: vec![16, 0, std::u16::MIN, std::u16::MAX],
        uints32: vec![32, 0, std::u32::MIN, std::u32::MAX],
        uints64: vec![64, 0, std::u64::MIN, std::u64::MAX],
        isizes: vec![-42,  0, std::isize::MIN, std::isize::MAX],
        ints8:  vec![-8,  0, std::i8::MIN, std::i8::MAX],
        ints16: vec![-16, 0, std::i16::MIN, std::i16::MAX],
        ints32: vec![-32, 0, std::i32::MIN, std::i32::MAX],
        ints64: vec![-64, 0, std::i64::MIN, std::i64::MAX],
    };

    assert!(<BasicTypeSequenceModel as Model>::__get_type("strs").unwrap() == AttrType::Sequence(box AttrType::String));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("strings").unwrap() == AttrType::Sequence(box AttrType::String));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("usizes").unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("uints8").unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("uints16").unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("uints32").unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("uints64").unwrap() == AttrType::Sequence(box AttrType::Uint));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("isizes").unwrap() == AttrType::Sequence(box AttrType::Int));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("ints8").unwrap() == AttrType::Sequence(box AttrType::Int));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("ints16").unwrap() == AttrType::Sequence(box AttrType::Int));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("ints32").unwrap() == AttrType::Sequence(box AttrType::Int));
    assert!(<BasicTypeSequenceModel as Model>::__get_type("ints64").unwrap() == AttrType::Sequence(box AttrType::Int));


    const US_MAX: u64 = ::std::usize::MAX as u64;
    const U8_MAX: u64 = ::std::u8::MAX as u64;
    const U16_MAX: u64 = ::std::u16::MAX as u64;
    const U32_MAX: u64 = ::std::u32::MAX as u64;
    const U64_MAX: u64 = ::std::u64::MAX as u64;
    const US_MIN: u64 = ::std::usize::MIN as u64;
    const U8_MIN: u64 = ::std::u8::MIN as u64;
    const U16_MIN: u64 = ::std::u16::MIN as u64;
    const U32_MIN: u64 = ::std::u32::MIN as u64;
    const U64_MIN: u64 = ::std::u64::MIN as u64;

    const IS_MAX: i64 = ::std::isize::MAX as i64;
    const I8_MAX: i64 = ::std::i8::MAX as i64;
    const I16_MAX: i64 = ::std::i16::MAX as i64;
    const I32_MAX: i64 = ::std::i32::MAX as i64;
    const I64_MAX: i64 = ::std::i64::MAX as i64;
    const IS_MIN: i64 = ::std::isize::MIN as i64;
    const I8_MIN: i64 = ::std::i8::MIN as i64;
    const I16_MIN: i64 = ::std::i16::MIN as i64;
    const I32_MIN: i64 = ::std::i32::MIN as i64;
    const I64_MIN: i64 = ::std::i64::MIN as i64;


    test_attr_seq!(*model.__get_attr("strs").unwrap(), [Attr::String("str 1"), Attr::String("str 2")]);
    test_attr_seq!(*model.__get_attr("strings").unwrap(), [Attr::String("string 1"), Attr::String("string 2")]);

    test_attr_seq!(*model.__get_attr("usizes").unwrap(),
            [Attr::Uint(42), Attr::Uint(0), Attr::Uint(US_MIN), Attr::Uint(US_MAX)]);
    test_attr_seq!(*model.__get_attr("uints8").unwrap(),
            [Attr::Uint(8), Attr::Uint(0), Attr::Uint(U8_MIN), Attr::Uint(U8_MAX)]);
    test_attr_seq!(*model.__get_attr("uints16").unwrap(),
            [Attr::Uint(16), Attr::Uint(0), Attr::Uint(U16_MIN), Attr::Uint(U16_MAX)]);
    test_attr_seq!(*model.__get_attr("uints32").unwrap(),
            [Attr::Uint(32), Attr::Uint(0), Attr::Uint(U32_MIN), Attr::Uint(U32_MAX)]);
    test_attr_seq!(*model.__get_attr("uints64").unwrap(),
            [Attr::Uint(64), Attr::Uint(0), Attr::Uint(U64_MIN), Attr::Uint(U64_MAX)]);

    test_attr_seq!(*model.__get_attr("isizes").unwrap(),
            [Attr::Int(-42), Attr::Int(0), Attr::Int(IS_MIN), Attr::Int(IS_MAX)]);
    test_attr_seq!(*model.__get_attr("ints8").unwrap(),
            [Attr::Int(-8), Attr::Int(0), Attr::Int(I8_MIN), Attr::Int(I8_MAX)]);
    test_attr_seq!(*model.__get_attr("ints16").unwrap(),
            [Attr::Int(-16), Attr::Int(0), Attr::Int(I16_MIN), Attr::Int(I16_MAX)]);
    test_attr_seq!(*model.__get_attr("ints32").unwrap(),
            [Attr::Int(-32), Attr::Int(0), Attr::Int(I32_MIN), Attr::Int(I32_MAX)]);
    test_attr_seq!(*model.__get_attr("ints64").unwrap(),
            [Attr::Int(-64), Attr::Int(0), Attr::Int(I64_MIN), Attr::Int(I64_MAX)]);
}

#[model]
struct SequenceOfSequenceModel {
    usizes: Vec<Vec<u8>>,
}

#[test]
fn sequence_of_sequence_model() {
    let model = SequenceOfSequenceModel {
        usizes: vec![
            vec![1, 2, 3],
            vec![2, 3],
            vec![],
            vec![4],
        ],
    };

    assert!(<SequenceOfSequenceModel as Model>::__get_type("usizes").unwrap() == AttrType::Sequence(box AttrType::Sequence(box AttrType::Uint)));

    match *model.__get_attr("usizes").unwrap() {
        Attr::Sequence(mut iter) => {
            test_attr_seq!(*iter.next().unwrap(), [Attr::Uint(1), Attr::Uint(2), Attr::Uint(3)]);
            test_attr_seq!(*iter.next().unwrap(), [Attr::Uint(2), Attr::Uint(3)]);
            test_attr_seq!(*iter.next().unwrap(), []);
            test_attr_seq!(*iter.next().unwrap(), [Attr::Uint(4)]);

            assert!(iter.next().is_none());
        }
        _ => panic!("Expected sequence"),
    }
}

#[model]
struct UnusualSequenceModel {
    d_list: std::collections::DList<u16>,
}

#[test]
fn unusual_sequence_model() {
    let model = UnusualSequenceModel {
        d_list: vec![1, 2, 3, 4].iter().map(|&v| v).collect(),
    };

    assert!(<UnusualSequenceModel as Model>::__get_type("d_list").unwrap() == AttrType::Sequence(box AttrType::Uint));

    test_attr_seq!(*model.__get_attr("d_list").unwrap(), [Attr::Uint(1), Attr::Uint(2), Attr::Uint(3), Attr::Uint(4)]);
}
