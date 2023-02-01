use chrono::Datelike;
use counter::Counter;
use itertools::Itertools;
use lazy_static::lazy_static;
use queues::*;
use std::fs;
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
// Performace settings: higher numbers take longer but may increase yeild.
const UNARY_OP_GROUP_LIMIT: u8 = 5; // sqrt(((x)!)!!) = 3
const ABS_NUM_SIZE_LIMIT: i32 = 1000; // maximum value that numbers in calculations can reach in intermediate expressions
const MAX_BASE: i32 = 12; // maximum base that will be calculated
const MAX_EXPONENT: i32 = 10; //maximum exponent that will be calculated
const MAX_FACTORIAL: i32 = 10; //maximum x in x!  that will be calculated
const MAX_DOUBLE_FACTORIAL: i32 = 10; //maximum x!! that will be calculated

//IMPORTANT ====================================================
const ANSWERS_MINIMUM: i32 = 0; //minimum final answer
const ANSWERS_MAXIUM: i32 = 100; //maximum final answer
                                 // =============================================================

lazy_static! {
    static ref YEAR_DIGITS: Vec<char> = get_user_input_digits();
    static ref YEAR_DIGITS_COUNTER: Counter<u8> = YEAR_DIGITS
        .iter()
        .map(|x| x.to_digit(10).unwrap() as u8)
        .collect();
}
fn main() {
    let mut expressions = vec![];
    let x = add_expressions();
    for (value, ex) in x.get_answers().into_iter() {
        if *value >= ANSWERS_MINIMUM && *value <= ANSWERS_MAXIUM {
            expressions.push((*value, format!("{:?} = {}\n", value, collapse(&ex.0, &x),)));
        }
    }
    expressions.sort();
    fs::write("answer.txt", expressions.iter().map(|x| &x.1).join("")).unwrap();
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
    Radical,
}
impl BinaryOperation {
    fn eval(&self, a: i32, b: i32) -> Result<i32, EvaluationError> {
        match *self {
            BinaryOperation::Add if (a + b).abs() <= ABS_NUM_SIZE_LIMIT => Ok(a + b),
            BinaryOperation::Subtract if (a - b).abs() <= ABS_NUM_SIZE_LIMIT => Ok(a - b),
            BinaryOperation::Multiply if (a * b).abs() <= ABS_NUM_SIZE_LIMIT => Ok(a * b),
            BinaryOperation::Divide if b == 0 => Err(EvaluationError::ZeroDivision),
            BinaryOperation::Divide if a % b != 0 => Err(EvaluationError::Fractional),
            BinaryOperation::Divide => Ok(a / b),
            BinaryOperation::Power if a == 0 && b == 0 => Ok(1),
            BinaryOperation::Power if b < 0 => Err(EvaluationError::Fractional),
            BinaryOperation::Power
                if a.abs() < MAX_BASE
                    && b.abs() < MAX_EXPONENT
                    && a.pow(b as u32).abs() <= ABS_NUM_SIZE_LIMIT =>
            {
                Ok(a.pow(b as u32))
            }
            BinaryOperation::Radical if f64::powf(a as f64, 1.0 / b as f64) % 1. < f64::EPSILON => {
                Ok(f64::powf(a as f64, 1.0 / b as f64) as i32)
            }

            _ => Err(EvaluationError::Overflow),
        }
    }
    fn get_as_string(&self, a: (&str, Expression), b: (&str, Expression)) -> String {
        let token_a = if matches!(
            a.1,
            Expression::Simple(_) | Expression::UnaryComposite { .. }
        ) {
            a.0.to_owned()
        } else {
            format!("({})", a.0)
        };
        let token_b = if matches!(
            b.1,
            Expression::Simple(_) | Expression::UnaryComposite { .. }
        ) {
            b.0.to_owned()
        } else {
            format!("({})", b.0)
        };
        match *self {
            BinaryOperation::Add => format!("{token_a} + {token_b}"),
            BinaryOperation::Subtract => format!("{token_a} - {token_b}"),
            BinaryOperation::Multiply => format!("{token_a} * {token_b}"),
            BinaryOperation::Divide => format!("{token_a}/{token_b}"),
            BinaryOperation::Power => format!("{token_a}^{token_b}"),
            BinaryOperation::Radical => format!("{token_b}√{token_a}"),
        }
    }
}

fn double_factorial(n: i32) -> i32 {
    return ((n + 1) % 2 + 1..=n).step_by(2).product();
}
#[derive(Debug, EnumIter, Clone, PartialEq, Eq, Hash, Copy)]
enum UnaryOperation {
    Negate,    // -x
    Factorial, // x!
    DoubleFactorial,
    Sqrt,
}
impl UnaryOperation {
    fn eval(&self, x: i32) -> Result<i32, EvaluationError> {
        match *self {
            UnaryOperation::Negate => Ok(-x),
            UnaryOperation::Factorial | UnaryOperation::DoubleFactorial if x < 0 => {
                Err(EvaluationError::NegativeFactorial)
            }
            UnaryOperation::Factorial | UnaryOperation::DoubleFactorial if x == 0 => Ok(1),
            UnaryOperation::Factorial
                if x < MAX_FACTORIAL && (1..=x).product::<i32>() <= ABS_NUM_SIZE_LIMIT =>
            {
                Ok((1..=x).product())
            }
            UnaryOperation::DoubleFactorial
                if x < MAX_DOUBLE_FACTORIAL && double_factorial(x) <= ABS_NUM_SIZE_LIMIT =>
            {
                Ok(double_factorial(x))
            }
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
    fn get_as_string(&self, a: (&str, Expression)) -> String {
        let token = if matches!(a.1, Expression::Simple(_)) {
            a.0.to_owned()
        } else {
            format!("({})", a.0)
        };
        match *self {
            UnaryOperation::Negate => format!("-{token}"),
            UnaryOperation::Factorial => format!("{token}!"),
            UnaryOperation::DoubleFactorial => format!("{token}!!"),
            UnaryOperation::Sqrt => format!("√{token}"),
        }
    }
}

#[derive(Debug)]
enum EvaluationError {
    Overflow,
    Fractional,
    ZeroDivision,
    IncompatibleWithYear,
    NegativeFactorial,
    IrationalSqrt,
    UnaryOperationOverflow,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct IncludedDigits(Counter<u8>);

impl Hash for IncludedDigits {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        self.0.hasher();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ExpressionIndex {
    included_digits: IncludedDigits,
    value: i32,
}

impl ExpressionIndex {
    fn new(included_digits: IncludedDigits, value: i32) -> Self {
        Self {
            included_digits,
            value,
        }
    }
}
#[derive(Debug, Clone)]
enum Expression {
    Simple(u32),
    UnaryComposite {
        a: ExpressionIndex,
        unary_op: UnaryOperation,
        iterative_ops: u8,
    },
    BinaryComposite {
        a: ExpressionIndex,
        binary_op: BinaryOperation,
        b: ExpressionIndex,
    },
}

#[derive(Clone, Debug)]
struct ExpressionData {
    included_digits: IncludedDigits,
    value: i32,
    data: Expression,
    nesting_level: u8,
}
impl Expression {
    fn from_single_num<'a>(n: u32) -> Result<ExpressionData, EvaluationError> {
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
        if n > ABS_NUM_SIZE_LIMIT as u32 {
            return Err(EvaluationError::Overflow);
        }
        Ok(ExpressionData {
            included_digits: IncludedDigits(included_digits),
            value: n as i32,
            data: Expression::Simple(n),
            nesting_level: 0,
        })
    }
    fn unary_compose<'a>(
        a: &'a ExpressionData,
        unary_op: UnaryOperation,
    ) -> Result<ExpressionData, EvaluationError> {
        Ok(ExpressionData {
            included_digits: a.included_digits.clone(),
            value: unary_op.eval(a.value)?,
            data: Expression::UnaryComposite {
                a: ExpressionIndex::new(a.included_digits.clone(), a.value),
                unary_op,
                iterative_ops: if let Expression::UnaryComposite { iterative_ops, .. } = &a.data {
                    if iterative_ops + 1 < UNARY_OP_GROUP_LIMIT {
                        iterative_ops + 1
                    } else {
                        Err(EvaluationError::UnaryOperationOverflow)?
                    }
                } else {
                    1
                },
            },
            nesting_level: a.nesting_level + 1,
        })
    }
    fn binary_compose<'a>(
        a: &'a ExpressionData,
        binary_op: BinaryOperation,
        b: &'a ExpressionData,
    ) -> Result<ExpressionData, EvaluationError> {
        let mut included_digits = a.included_digits.0.clone();
        included_digits.extend(b.included_digits.0.clone().iter());
        if !included_digits.is_subset(&YEAR_DIGITS_COUNTER) {
            //println!("Incompatible {:?}", included_digits);
            return Err(EvaluationError::IncompatibleWithYear);
        }
        Ok(ExpressionData {
            included_digits: IncludedDigits(included_digits),
            value: binary_op.eval(a.value, b.value)?,
            data: Expression::BinaryComposite {
                a: ExpressionIndex::new(a.included_digits.clone(), a.value),
                binary_op,
                b: ExpressionIndex::new(b.included_digits.clone(), b.value),
            },
            nesting_level: a.nesting_level + b.nesting_level, //a.nesting_level.max(b.nesting_level),
        })
    }
}

#[derive(Debug)]
struct ExpressionTable(HashMap<IncludedDigits, HashMap<i32, (Expression, u8)>>);
impl ExpressionTable {
    fn get(&self, index: ExpressionIndex) -> ExpressionData {
        let x = self
            .0
            .get(&index.included_digits)
            .unwrap()
            .get(&index.value)
            .unwrap();
        ExpressionData {
            included_digits: index.included_digits.clone(),
            value: index.value,
            data: x.0.clone(),
            nesting_level: x.1,
        }
    }
    fn get_answers(&self) -> &HashMap<i32, (Expression, u8)> {
        self.0
            .get(&IncludedDigits(YEAR_DIGITS_COUNTER.to_owned()))
            .unwrap()
    }
    fn get_expressions_that_pair(
        &self,
        included: &IncludedDigits,
    ) -> Vec<(&IncludedDigits, &HashMap<i32, (Expression, u8)>)> {
        self.0
            .iter()
            .filter_map(|(inc, exprs)| {
                (inc.0.clone() | included.0.clone())
                    .is_subset(&YEAR_DIGITS_COUNTER)
                    .then(|| (inc, exprs))
            })
            .collect_vec()
    }
    fn insert(&mut self, expression_data: ExpressionData) {
        let x = self.0.get_mut(&expression_data.included_digits).unwrap();
        match x.get(&expression_data.value) {
            Some((_, curr_nesting_level))
                if *curr_nesting_level <= expression_data.nesting_level => {}
            _ => {
                x.insert(
                    expression_data.value,
                    (expression_data.data, expression_data.nesting_level),
                );
            }
        }
    }
    fn new() -> ExpressionTable {
        let mut h = HashMap::new();
        let included: Vec<u8> = YEAR_DIGITS
            .iter()
            .map(|x| x.to_digit(10).unwrap() as u8)
            .collect();
        for a in included.to_owned().into_iter().powerset().skip(1) {
            h.insert(
                IncludedDigits(a.into_iter().collect::<Counter<_>>()),
                HashMap::new(),
            );
        }

        ExpressionTable(h)
    }
}

fn add_expressions() -> ExpressionTable {
    let mut table = ExpressionTable::new();
    let mut queue: Queue<ExpressionIndex> = queue![];
    let mut seen_expressions: HashSet<ExpressionIndex> = HashSet::new();
    fn add_to_queue(
        queue: &mut Queue<ExpressionIndex>,
        seen_expressions: &mut HashSet<ExpressionIndex>,
        expression_data: &ExpressionData,
    ) {
        let val = ExpressionIndex {
            included_digits: expression_data.included_digits.clone(),
            value: expression_data.value.clone(),
        };
        if !seen_expressions.contains(&val) {
            queue.add(val.clone()).unwrap();
            seen_expressions.insert(val);
        }
    }
    fn remove_from_queue(queue: &mut Queue<ExpressionIndex>) -> ExpressionIndex {
        queue.remove().unwrap()
    }
    // first insert basic digits
    for digit in YEAR_DIGITS.iter() {
        if let Ok(expression_data) = Expression::from_single_num(digit.to_digit(10).unwrap()) {
            add_to_queue(&mut queue, &mut seen_expressions, &expression_data);
            table.insert(expression_data);
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
                add_to_queue(&mut queue, &mut seen_expressions, &expression_data);
                table.insert(expression_data);
            }
        }
    }

    //now algorithm!
    while queue.size() > 0 {
        println!("The queue has {} items", queue.size());
        let curr = table.get(remove_from_queue(&mut queue));
        for unary_op in UnaryOperation::iter() {
            if let Ok(expression_data) = Expression::unary_compose(&curr, unary_op) {
                add_to_queue(&mut queue, &mut seen_expressions, &expression_data);
                table.insert(expression_data);
            }
        }
        let mut added = vec![];
        //now look up in the table for all other expressions that could validly combine with current
        let possible_pairs = table.get_expressions_that_pair(&curr.included_digits);
        for (included_digits, row) in possible_pairs {
            for (value, (data, nesting_level)) in row {
                for binary_op in BinaryOperation::iter() {
                    if let Ok(expression_data) = Expression::binary_compose(
                        &curr,
                        binary_op,
                        &ExpressionData {
                            included_digits: included_digits.clone(),
                            value: *value,
                            data: data.clone(),
                            nesting_level: *nesting_level,
                        },
                    ) {
                        add_to_queue(&mut queue, &mut seen_expressions, &expression_data);
                        added.push(expression_data);
                    };
                }
            }
        }
        for expression_data in added {
            table.insert(expression_data);
        }
    }
    table
}

fn collapse(expression: &Expression, table: &ExpressionTable) -> String {
    match expression {
        Expression::Simple(x) => x.to_string(),
        Expression::UnaryComposite { a, unary_op, .. } => {
            let data = table.get(a.clone()).data;
            unary_op.get_as_string((&collapse(&data, table), data))
        }
        Expression::BinaryComposite { a, binary_op, b } => {
            let a_data = table.get(a.clone()).data;
            let b_data = table.get(b.clone()).data;
            binary_op.get_as_string(
                ((&collapse(&a_data, table)), a_data),
                (&collapse(&b_data, table), b_data),
            )
        }
    }
}
