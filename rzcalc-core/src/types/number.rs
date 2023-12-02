#[derive(Clone, Copy)]
pub enum Number {
    Real(f64),
    Int(i64),
}
impl Default for Number {
    fn default() -> Self {
        Self::Int(0)
    }
}
impl Number {
    #[inline]
    pub const fn real(self) -> f64 {
        match self {
            Number::Real(n) => n,
            Number::Int(n) => n as f64,
        }
    }
    #[inline]
    pub const fn int(self) -> i64 {
        match self {
            Number::Real(n) => n as i64,
            Number::Int(n) => n,
        }
    }

    pub const fn type_name(&self) -> &'static str {
        match self {
            Number::Real(_) => "real number",
            Number::Int(_) => "interger number",
        }
    }
}
impl Number {
    pub fn safe_div(self, other: Self) -> Self {
        match (self, other) {
            (Number::Int(x), Number::Int(y)) => match x.checked_div_euclid(y) {
                Some(n) => Self::Int(n),
                None => Self::Real(f64::INFINITY),
            },
            (x, y) => x / y,
        }
    }
}

impl FromStr for Number {
    type Err = ParseNumberError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.chars().any(|x| x == '.' || x == 'e') {
            Ok(Number::Real(s.parse::<f64>().map_err(|err| {
                ParseNumberError {
                    toparse: s.to_owned(),
                    cause: err.to_string(),
                }
            })?))
        } else {
            match s.get(..2) {
                Some("0x") => return Number::from_str_radix(&s[2..], 16),
                Some("0o") => return Number::from_str_radix(&s[2..], 8),
                Some("0b") => return Number::from_str_radix(&s[2..], 2),
                _ => {}
            }
            Ok(Number::Int(s.parse::<i64>().map_err(|err| {
                ParseNumberError {
                    toparse: s.to_owned(),
                    cause: err.to_string(),
                }
            })?))
        }
    }
}

macro_rules! apply_ops_assign {
    ($($traits:ident :: $method:ident ($op:tt)),* $(,)?) => {
        $(impl $traits for Number {
            #[inline]
            fn $method(&mut self, rhs: Self) {
                match (self, rhs) {
                    (Self::Real(ref mut x), Self::Real(y)) => *x $op y,
                    (Self::Real(ref mut x), Self::Int(y)) => *x $op y as f64,
                    (Self::Int(ref mut x), Self::Real(y)) => *x $op y as i64,
                    (Self::Int(ref mut x), Self::Int(y)) => *x $op y,
                }
            }
        })*
    }
}

macro_rules! apply_ops {
    ($($traits:ident :: $method:ident ($op:tt)),* $(,)?) => {
        $(impl $traits for Number {
            type Output = Self;
            #[inline]
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::Real(x), Self::Real(y)) => Self::Real(x $op y),
                    (Self::Real(x), Self::Int(y)) => Self::Real(x $op y as f64),
                    (Self::Int(x), Self::Real(y)) => Self::Real(x as f64 $op y),
                    (Self::Int(x), Self::Int(y)) => Self::Int(x $op y),
                }
            }
        })*
    }
}
macro_rules! apply_bit_ops {
    ($($traits:ident :: $method:ident ($op:tt)),* $(,)?) => {
        $(impl $traits for Number {
            type Output = Self;
            #[inline]
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::Real(x), Self::Real(y)) => Self::Int((x as i64) $op (y as i64)),
                    (Self::Real(x), Self::Int(y)) => Self::Int((x as i64) $op y),
                    (Self::Int(x), Self::Real(y)) => Self::Int(x $op (y as i64)),
                    (Self::Int(x), Self::Int(y)) => Self::Int(x $op y),
                }
            }
        })*
    }
}
macro_rules! apply_bit_ops_assign {
    ($($traits:ident :: $method:ident ($op:tt)),* $(,)?) => {
        $(impl $traits for Number {
            #[inline]
            fn $method(&mut self, rhs: Self) {
                match (*self, rhs) {
                    (Number::Real(x), Number::Real(y)) => *self = Self::Int((x as i64) & (y as i64)),
                    (Number::Real(x), Number::Int(y)) => *self = Self::Int((x as i64) & y),
                    (Number::Int(x), Number::Real(y)) => *self = Self::Int(x & (y as i64)),
                    (Number::Int(x), Number::Int(y)) => *self = Self::Int(x & y),
                }
            }
        })*
    }
}

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    ops::{
        Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div,
        DivAssign, Mul, MulAssign, Neg, Not, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub,
        SubAssign,
    },
    str::FromStr,
};

apply_ops!(Add::add(+), Sub::sub(-), Mul::mul(*), Div::div(/), Rem::rem(%));
apply_ops_assign!(AddAssign::add_assign(+=), SubAssign::sub_assign(-=), MulAssign::mul_assign(*=), DivAssign::div_assign(/=), RemAssign::rem_assign(%=));

apply_bit_ops!(BitAnd::bitand(&), BitOr::bitor(|), BitXor::bitxor(^), Shr::shr(>>), Shl::shl(<<));
apply_bit_ops_assign!(BitAndAssign::bitand_assign(&), BitOrAssign::bitor_assign(|), BitXorAssign::bitxor_assign(^), ShrAssign::shr_assign(>>), ShlAssign::shl_assign(<<));

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Real(x), Number::Real(y)) => x.eq(y),
            (Number::Real(x), Number::Int(y)) => x.eq(&(*y as f64)),
            (Number::Int(x), Number::Real(y)) => (*x as f64).eq(y),
            (Number::Int(x), Number::Int(y)) => x.eq(y),
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Number::Real(x), Number::Real(y)) => x.partial_cmp(y),
            (Number::Real(x), Number::Int(y)) => x.partial_cmp(&(*y as f64)),
            (Number::Int(x), Number::Real(y)) => (*x as f64).partial_cmp(y),
            (Number::Int(x), Number::Int(y)) => x.partial_cmp(y),
        }
    }
}

impl Hash for Number {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.int().hash(state)
    }
}

impl Neg for Number {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self::Output {
        match self {
            Number::Real(n) => Self::Real(-n),
            Number::Int(n) => Self::Int(-n),
        }
    }
}
impl Not for Number {
    type Output = Self;
    #[inline]
    fn not(self) -> Self::Output {
        match self {
            Number::Real(x) => Self::Int(Not::not(x as i64)),
            Number::Int(x) => Self::Int(Not::not(x)),
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Real(n) => write!(f, "{n}"),
            Number::Int(n) => write!(f, "{n}"),
        }
    }
}
impl Debug for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Real(n) => f.debug_tuple("Real").field(n).finish(),
            Self::Int(n) => f.debug_tuple("Int").field(n).finish(),
        }
    }
}

#[allow(unused)]
impl Number {
    pub const fn zero() -> Self {
        Self::Int(0)
    }
    pub fn is_zero(&self) -> bool {
        match self {
            Number::Real(x) if *x == 0.0 => true,
            Number::Int(0) => true,
            _ => false,
        }
    }
    pub const fn one() -> Self {
        Self::Int(1)
    }

    fn from_str_radix(str: &str, radix: u32) -> Result<Self, ParseNumberError> {
        match i64::from_str_radix(str, radix) {
            Ok(ok) => Ok(Self::Int(ok)),
            Err(err) => Err(ParseNumberError {
                toparse: str.to_owned(),
                cause: err.to_string(),
            }),
        }
    }

    pub const fn nan() -> Self {
        Self::Real(f64::NAN)
    }
    pub const fn infinity() -> Self {
        Self::Real(f64::INFINITY)
    }
    pub const fn neg_infinity() -> Self {
        Self::Real(f64::NEG_INFINITY)
    }
    pub const fn neg_zero() -> Self {
        Self::Real(-0.0)
    }
    pub const fn min_value() -> Self {
        Self::Real(f64::MIN)
    }
    pub const fn min_positive_value() -> Self {
        Self::Real(f64::MIN_POSITIVE)
    }
    pub const fn max_value() -> Self {
        Self::Real(f64::MAX)
    }
    pub fn is_nan(self) -> bool {
        self.real().is_nan()
    }
    pub fn is_infinite(self) -> bool {
        self.real().is_infinite()
    }
    pub fn is_finite(self) -> bool {
        self.real().is_finite()
    }
    pub fn is_normal(self) -> bool {
        self.real().is_normal()
    }
    pub fn classify(self) -> std::num::FpCategory {
        self.real().classify()
    }
    pub fn floor(self) -> Self {
        Self::Real(self.real().floor())
    }
    pub fn ceil(self) -> Self {
        Self::Real(self.real().ceil())
    }
    pub fn round(self) -> Self {
        Self::Real(self.real().round())
    }
    pub fn trunc(self) -> Self {
        Self::Real(self.real().trunc())
    }
    pub fn fract(self) -> Self {
        Self::Real(self.real().fract())
    }
    pub fn abs(self) -> Self {
        Self::Real(self.real().abs())
    }
    pub fn signum(self) -> Self {
        Self::Real(self.real().signum())
    }
    pub fn is_sign_positive(self) -> bool {
        self.real().is_sign_positive()
    }
    pub fn is_sign_negative(self) -> bool {
        self.real().is_sign_negative()
    }
    pub fn mul_add(self, a: Self, b: Self) -> Self {
        Self::Real(f64::mul_add(self.real(), a.real(), b.real()))
    }
    pub fn recip(self) -> Self {
        Self::Real(self.real().recip())
    }
    pub fn powi(self, n: i32) -> Self {
        Self::Real(self.real().powi(n))
    }
    pub fn powf(self, n: Self) -> Self {
        Self::Real(self.real().powf(n.real()))
    }
    pub fn sqrt(self) -> Self {
        Self::Real(self.real().sqrt())
    }
    pub fn exp(self) -> Self {
        Self::Real(self.real().exp())
    }
    pub fn exp2(self) -> Self {
        Self::Real(self.real().exp2())
    }
    pub fn ln(self) -> Self {
        Self::Real(self.real().ln())
    }
    pub fn log(self, base: Self) -> Self {
        Self::Real(self.real().log(base.real()))
    }
    pub fn log2(self) -> Self {
        Self::Real(self.real().log2())
    }
    pub fn log10(self) -> Self {
        Self::Real(self.real().log10())
    }
    pub fn max(self, other: Self) -> Self {
        Self::Real(self.real().max(other.real()))
    }
    pub fn min(self, other: Self) -> Self {
        Self::Real(self.real().min(other.real()))
    }
    pub fn abs_sub(self, other: Self) -> Self {
        #[allow(deprecated)]
        Self::Real(self.real().abs_sub(other.real()))
    }
    pub fn cbrt(self) -> Self {
        Self::Real(self.real().cbrt())
    }
    pub fn hypot(self, other: Self) -> Self {
        Self::Real(self.real().hypot(other.real()))
    }
    pub fn sin(self) -> Self {
        Self::Real(self.real().sin())
    }
    pub fn cos(self) -> Self {
        Self::Real(self.real().cos())
    }
    pub fn tan(self) -> Self {
        Self::Real(self.real().tan())
    }
    pub fn asin(self) -> Self {
        Self::Real(self.real().asin())
    }
    pub fn acos(self) -> Self {
        Self::Real(self.real().acos())
    }
    pub fn atan(self) -> Self {
        Self::Real(self.real().atan())
    }
    pub fn atan2(self, other: Self) -> Self {
        Self::Real(self.real().atan2(other.real()))
    }
    pub fn sin_cos(self) -> (Self, Self) {
        let (s, c) = self.real().sin_cos();
        (Self::Real(s), Self::Real(c))
    }
    pub fn exp_m1(self) -> Self {
        Self::Real(self.real().exp_m1())
    }
    pub fn ln_1p(self) -> Self {
        Self::Real(self.real().ln_1p())
    }
    pub fn sinh(self) -> Self {
        Self::Real(self.real().sinh())
    }
    pub fn cosh(self) -> Self {
        Self::Real(self.real().cosh())
    }
    pub fn tanh(self) -> Self {
        Self::Real(self.real().tanh())
    }
    pub fn asinh(self) -> Self {
        Self::Real(self.real().asinh())
    }
    pub fn acosh(self) -> Self {
        Self::Real(self.real().acosh())
    }
    pub fn atanh(self) -> Self {
        Self::Real(self.real().atanh())
    }
}

macro_rules! impl_from_num {
    ($($tp:ty),*) => {$(
        impl From<$tp> for Number {
            #[inline]
            fn from(value: $tp) -> Self {
                Number::Int(value as i64)
            }
        }
    )*};
}
impl_from_num!(i8, u8, i16, u16, i32, u32, i64, u64, isize, usize);

impl From<f64> for Number {
    #[inline]
    fn from(value: f64) -> Self {
        Number::Real(value)
    }
}
impl From<f32> for Number {
    #[inline]
    fn from(value: f32) -> Self {
        Number::Real(value as f64)
    }
}

#[derive(Debug, Clone)]
pub struct ParseNumberError {
    toparse: String,
    cause: String,
}
impl Display for ParseNumberError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "failed to parse number from string: '{}' - (Reason: {})",
            self.toparse, self.cause
        )
    }
}
