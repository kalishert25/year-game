use chrono::Datelike;
use counter::Counter;
use itertools::{Itertools, MultiProduct};
use lazy_static::lazy_static;
use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    iter,
    rc::Rc,
    sync::RwLock,
};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

const UNARY_OP_GROUP_LIMIT: u8 = 2; // sqrt(((x)!)!!) = 3
const ABS_NUM_SIZE_LIMIT: i32 = 100; // maximum value that numbers in calculations can reach
const MAX_BASE: i32 = 12; // maximum base that will be calculated
const MAX_EXPONENT: i32 = 7; //maximum exponent that will be calculated
const MAX_FACTORIAL: i32 = 5; //maximum x in x! and (2x)!! that will be calculated

lazy_static! {
    static ref YEAR_DIGITS: Vec<char> = get_user_input_digits();
    static ref YEAR_DIGITS_COUNTER: Counter<u8> = YEAR_DIGITS
        .iter()
        .map(|x| x.to_digit(10).unwrap() as u8)
        .collect();
    // static ref INDEX_MAP: IndexMap = {
    //     let (a, b) = make_index_maps();
    //     IndexMap {
    //         digits_to_index: a,
    //         index_to_digits: b,
    //     }
    // };
}
fn main() {
    let mut equations = ExpressionMap::new();
    // first insert basic digits
    for digit in YEAR_DIGITS.iter() {
        if let Ok(expression_data) = Expression::from_single_num(digit.to_digit(10).unwrap()) {
            equations.insert(expression_data);
        }
    }
    // then insert the permutations of digits
    for k in 2..=YEAR_DIGITS.len() {
        for n in YEAR_DIGITS
            .iter()
            .permutations(k)
            .map(|x| String::from_iter(x).parse::<u32>().unwrap())
        {
            if let Ok(expression_data) = Expression::from_single_num(n) {
                equations.insert(expression_data);
            }
        }
    }

    dbg!(equations);

}

fn get_user_input_digits() -> Vec<char> {
    let year = chrono::Utc::now().year();
    let mut user_input = String::new();
    println!("Lets play the yeargame. Tell me a year and I'll make mathmatical expressions from its digits.");
    println!(
        "Enter a new year if you want to change it, otherwise it will default to {}.",
        year
    );
    std::io::stdin().read_line(&mut user_input).unwrap();
    let user_year: Vec<char> = user_input
        .chars()
        .filter(|x| char::is_numeric(*x))
        .collect();
    return if user_year.len() > 0 {
        user_year.into_iter().collect()
    } else {
        year.to_string().chars().collect()
    };
}

#[derive(Debug, EnumIter, Clone, PartialEq, Eq, Hash, Copy)]
enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
}
impl BinaryOperation {
    fn eval(&self, a: i32, b: i32) -> Result<i32, EvaluationError> {
        match *self {
            BinaryOperation::Add if (a + b).abs() < ABS_NUM_SIZE_LIMIT => Ok(a - b),
            BinaryOperation::Subtract if (a - b).abs() < ABS_NUM_SIZE_LIMIT => Ok(a - b),
            BinaryOperation::Multiply if (a * b).abs() < ABS_NUM_SIZE_LIMIT => Ok(a * b),
            BinaryOperation::Divide if b == 0 => Err(EvaluationError::ZeroDivision),
            BinaryOperation::Divide if a % b != 0 => Err(EvaluationError::Fractional),
            BinaryOperation::Divide => Ok(a / b),
            BinaryOperation::Power if a == 0 && b == 0 => Ok(1),
            BinaryOperation::Power if a < 0 => Err(EvaluationError::Fractional),
            BinaryOperation::Power
                if a.abs() < MAX_BASE
                    && b < MAX_EXPONENT
                    && a.pow(b as u32).abs() < ABS_NUM_SIZE_LIMIT =>
            {
                Ok(a.pow(b as u32))
            }
            _ => Err(EvaluationError::Overflow),
        }
    }
}

#[derive(Debug, EnumIter, Clone, PartialEq, Eq, Hash, Copy)]
enum UnaryOperation {
    Negate,    // -x
    Factorial, // x!
    Sqrt,
}
impl UnaryOperation {
    fn eval(&self, x: i32) -> Result<i32, EvaluationError> {
        match *self {
            UnaryOperation::Negate => Ok(-x),
            UnaryOperation::Factorial if x < 0 => Err(EvaluationError::NegativeFactorial),
            UnaryOperation::Factorial if x == 0 => Ok(1),
            UnaryOperation::Factorial if x < MAX_FACTORIAL => Ok((1..=x).product()),
            UnaryOperation::Sqrt => {
                if (x as f32).sqrt() % 1. < f32::EPSILON {
                    Ok((x as f32).sqrt() as i32)
                } else {
                    Err(EvaluationError::IrationalSqrt)
                }
            }
            _ => Err(EvaluationError::Overflow),
        }
    }
}


pub trait ProductRepeat: Iterator + Clone
where
    Self::Item: Clone,
{
    fn product_repeat(self, repeat: usize) -> MultiProduct<Self> {
        std::iter::repeat(self)
            .take(repeat)
            .multi_cartesian_product()
    }
}

impl<T: Iterator + Clone> ProductRepeat for T where T::Item: Clone {}

#[derive(Debug)]
enum EvaluationError {
    Overflow,
    Fractional,
    ZeroDivision,
    IncompatibleWithYear,
    NegativePower,
    NegativeFactorial,
    IrationalSqrt,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct ExpressionIndex(Counter<u8>);

impl Hash for ExpressionIndex {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hasher();
    }
}

#[derive(Debug, Clone, Copy)]
enum Expression<'a> {
    Simple(u32),
    UnaryComposite {
        a: &'a Expression<'a>,
        unary_op: UnaryOperation,
    },
    BinaryComposite {
        a: &'a Expression<'a>,
        binary_op: BinaryOperation,
        b: &'a Expression<'a>,
    },
}

struct ExpressionData<'a> {
    included_digits: ExpressionIndex,
    value: i32,
    data: Expression<'a>,
}
impl Expression<'_> {
    fn from_single_num(n: u32) -> Result<ExpressionData<'static>, EvaluationError> {
        let mut included_digits: Counter<u8> = Counter::new();
        if n == 0 {
            included_digits[&0] += 1
        } else {
            let mut m = n.clone();

            while m > 0 {
                included_digits[&((m % 10) as u8)] += 1;
                m /= 10;
            }
        }

        if !included_digits.is_subset(&YEAR_DIGITS_COUNTER) {
            return Err(EvaluationError::IncompatibleWithYear);
        }
        if n >= ABS_NUM_SIZE_LIMIT as u32 {
            return Err(EvaluationError::Overflow);
        }
        Ok(ExpressionData {
            included_digits: ExpressionIndex(included_digits),
            value: n as i32,
            data: Expression::Simple(n),
        })
    }
    fn unary_compose<'a>(
        a: &'a ExpressionData<'a>,
        unary_op: UnaryOperation,
    ) -> Result<ExpressionData<'a>, EvaluationError> {
        Ok(ExpressionData {
            included_digits: a.included_digits.clone(),
            value: unary_op.eval(a.value)?,
            data: Expression::UnaryComposite {
                a: &a.data,
                unary_op,
            },
        })
    }
    fn binary_compose<'a>(
        a: &'a ExpressionData,
        b: &'a ExpressionData,
        binary_op: BinaryOperation,
    ) -> Result<ExpressionData<'a>, EvaluationError> {
        let included_digits = a.included_digits.0.clone() | b.included_digits.0.clone();
        if !included_digits.is_subset(&YEAR_DIGITS_COUNTER) {
            return Err(EvaluationError::IncompatibleWithYear);
        }
        Ok(ExpressionData {
            included_digits: ExpressionIndex(included_digits),
            value: binary_op.eval(a.value, b.value)?,
            data: Expression::BinaryComposite {
                a: &a.data,
                binary_op,
                b: &b.data,
            },
        })
    }
}

#[derive(Debug)]
struct ExpressionMap<'a>(HashMap<ExpressionIndex, HashMap<i32, Expression<'a>>>);
impl ExpressionMap<'_> {
    fn new() -> ExpressionMap<'static> {
        let mut h = HashMap::new();
        let included: Vec<u8> = YEAR_DIGITS
        .iter()
        .map(|x| x.to_digit(10).unwrap() as u8)
        .collect();
        for a in included
        .to_owned()
        .into_iter()
        .powerset()
        .skip(1) {
            h.insert(ExpressionIndex(a.into_iter().collect::<Counter<_>>()), HashMap::new());
        }

        ExpressionMap(h)
    }
    fn insert(&mut self, expression_data: ExpressionData) {
        self.0.get_mut(&expression_data.included_digits).unwrap().insert(expression_data.value, expression_data.data);
    }
}

//2 12 [1] [2] [12]
//[1] [2] [3] [12] [13] [23] [123]  v   

// fn make_index_maps() -> (
//     HashMap<ExpressionIndex, usize>,
//     HashMap<usize, ExpressionIndex>,
// ) {
//     let included: Vec<u8> = YEAR_DIGITS
//         .iter()
//         .map(|x| x.to_digit(10).unwrap() as u8)
//         .collect();
//     let mut digits_to_index = HashMap::new();
//     let mut index_to_digits = HashMap::new();
//     for (a, b) in included
//         .to_owned()
//         .into_iter()
//         .powerset()
//         .skip(1)
//         .enumerate()
//     {
//         let inc = ExpressionIndex(b.into_iter().collect::<Counter<_>>());
//         digits_to_index.insert(inc.clone(), a);
//         index_to_digits.insert(a, inc);
//     }
//     (digits_to_index, index_to_digits)
// }

// struct IndexMap {
//     digits_to_index: HashMap<ExpressionIndex, usize>,
//     index_to_digits: HashMap<usize, ExpressionIndex>,
// }
// impl IndexMap {
//     fn len(&self) -> usize {
//         self.digits_to_index.len()
//     }
//     fn get_digits(&self, index: usize) -> Option<&ExpressionIndex> {
//         self.index_to_digits.get(&index)
//     }
//     fn get_index(&self, digits: ExpressionIndex) -> Option<&usize> {
//         self.digits_to_index.get(&digits)
//     }
// }




        // let mut h = HashMap::new();
        // let included: Vec<u8> = YEAR_DIGITS
        // .iter()
        // .map(|x| x.to_digit(10).unwrap() as u8)
        // .collect();
        // for a in included
        // .to_owned()
        // .into_iter()
        // .powerset()
        // .skip(1) {
        //     h.insert(ExpressionIndex(a.into_iter().collect::<Counter<_>>()), HashMap::new());
        // }

        // EquationMap(h)


    // let partitions = get_ordered_partitions(&year);
    // let length = partitions.len();
    // for (i, numbers) in partitions.into_iter().enumerate() {
    //     println!("n={:?}, ({}/{})", numbers, i, length);
    //     let n = numbers.len();
    //     for binary_ops in BinaryOperation::iter().permutations(n - 1) {
    //         for parenthesis_arangement in UnaryOperation::iter().product_repeat(n * (n + 1) / 2) {
    //             let (a, b) = solve(numbers, binary_ops, parenthesis_arangement);
    //             count += 1;
    //         }
    //     }
    // }
    // dbg!(count);



//     fn get_ordered_partitions(year_digits: &[char]) -> HashSet<Vec<String>> {
//     // first generate all partitions of the set
//     let mut partitions = HashSet::new();
//     let n = year_digits.len();
//     let mut codeword = vec![1; n];

//     'outer: loop {
//         if let Some(x) = ordered_partitions_from_codeword(&codeword, year_digits) {
//             partitions.extend(x)
//         }

//         let mut start_index = n - 1;
//         while start_index >= 0 {
//             let max_value = match codeword[0..start_index].iter().max() {
//                 Some(x) => x,
//                 None => break 'outer,
//             };
//             if codeword[start_index] > *max_value || max_value > &n || codeword[start_index] >= n {
//                 codeword[start_index] = 1;
//                 start_index -= 1;
//             } else {
//                 codeword[start_index] += 1;
//                 break;
//             }
//         }
//     }
//     return partitions;

//     fn ordered_partitions_from_codeword(
//         codeword: &[usize],
//         year_digits: &[char],
//     ) -> Option<HashSet<Vec<String>>> {
//         // take a codeword, EX: [1, 1, 2] => ab|c => {ab|c, c|ab}
//         // returns None if any part has a leading zero
//         let length = *codeword.iter().max().unwrap();
//         let mut base_partition = vec![String::new(); length];
//         for (index, value) in codeword.iter().enumerate() {
//             base_partition[*value - 1] += &year_digits[index].to_string();
//             if base_partition[*value - 1].starts_with("0") && base_partition[*value - 1].len() > 1 {
//                 return None; // if any of the numbers have leading zeros, dont add the partition
//             }
//         }
//         let x = base_partition
//             .into_iter()
//             .permutations(length)
//             .collect::<HashSet<_>>();
//         Some(x)
//     }
// }

