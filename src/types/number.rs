#[derive(Debug, Clone, Copy)]
pub enum Number {
    Real(f64),
    Int(i64),
}
impl Number {
    #[inline]
    pub fn real(self) -> f64 {
        match self {
            Number::Real(n) => n,
            Number::Int(n) => n.to_f64().unwrap_or_default(),
        }
    }
    #[inline]
    pub fn int(self) -> i64 {
        match self {
            Number::Real(n) => n.to_i64().unwrap_or_default(),
            Number::Int(n) => n,
        }
    }
}

impl FromStr for Number {
    type Err = ParseNumberError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use num_traits::Num;
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
    fmt::Display,
    ops::{
        Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div,
        DivAssign, Mul, MulAssign, Neg, Not, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub,
        SubAssign,
    },
    str::FromStr,
};

use num_traits::ToPrimitive;

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
            Number::Real(n) => write!(f, "{n} (Real)"),
            Number::Int(n) => write!(f, "{n} (Int)"),
        }
    }
}

impl num_traits::Zero for Number {
    #[inline]
    fn zero() -> Self {
        Self::Int(0)
    }
    #[inline]
    fn is_zero(&self) -> bool {
        matches!(self, Number::Real(x) if x.is_zero()) || matches!(self, Number::Int(0))
    }
}
impl num_traits::One for Number {
    #[inline]
    fn one() -> Self {
        Self::Int(1)
    }
}
impl num_traits::Num for Number {
    type FromStrRadixErr = ParseNumberError;
    fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        match i64::from_str_radix(str, radix) {
            Ok(ok) => Ok(Self::Int(ok)),
            Err(err) => Err(ParseNumberError {
                toparse: str.to_owned(),
                cause: err.to_string(),
            }),
        }
    }
}
impl num_traits::ToPrimitive for Number {
    fn to_i64(&self) -> Option<i64> {
        Some(self.int())
    }
    fn to_u64(&self) -> Option<u64> {
        self.int().to_u64()
    }
    fn to_f64(&self) -> Option<f64> {
        Some(self.real())
    }
}
impl num_traits::NumCast for Number {
    fn from<T: num_traits::ToPrimitive>(n: T) -> Option<Self> {
        n.to_i64().map(Self::Int)
    }
}

impl num_traits::Float for Number {
    fn nan() -> Self {
        Self::Real(f64::nan())
    }
    fn infinity() -> Self {
        Self::Real(f64::infinity())
    }
    fn neg_infinity() -> Self {
        Self::Real(f64::neg_infinity())
    }
    fn neg_zero() -> Self {
        Self::Real(f64::neg_zero())
    }
    fn min_value() -> Self {
        Self::Real(f64::min_value())
    }
    fn min_positive_value() -> Self {
        Self::Real(f64::min_positive_value())
    }
    fn max_value() -> Self {
        Self::Real(f64::max_value())
    }
    fn is_nan(self) -> bool {
        self.real().is_nan()
    }
    fn is_infinite(self) -> bool {
        self.real().is_infinite()
    }
    fn is_finite(self) -> bool {
        self.real().is_finite()
    }
    fn is_normal(self) -> bool {
        self.real().is_normal()
    }
    fn classify(self) -> std::num::FpCategory {
        self.real().classify()
    }
    fn floor(self) -> Self {
        Self::Real(self.real().floor())
    }
    fn ceil(self) -> Self {
        Self::Real(self.real().ceil())
    }
    fn round(self) -> Self {
        Self::Real(self.real().round())
    }
    fn trunc(self) -> Self {
        Self::Real(self.real().trunc())
    }
    fn fract(self) -> Self {
        Self::Real(self.real().fract())
    }
    fn abs(self) -> Self {
        Self::Real(self.real().abs())
    }
    fn signum(self) -> Self {
        Self::Real(self.real().signum())
    }
    fn is_sign_positive(self) -> bool {
        self.real().is_sign_positive()
    }
    fn is_sign_negative(self) -> bool {
        self.real().is_sign_negative()
    }
    fn mul_add(self, a: Self, b: Self) -> Self {
        Self::Real(f64::mul_add(self.real(), a.real(), b.real()))
    }
    fn recip(self) -> Self {
        Self::Real(self.real().recip())
    }
    fn powi(self, n: i32) -> Self {
        Self::Real(self.real().powi(n))
    }
    fn powf(self, n: Self) -> Self {
        Self::Real(self.real().powf(n.real()))
    }
    fn sqrt(self) -> Self {
        Self::Real(self.real().sqrt())
    }
    fn exp(self) -> Self {
        Self::Real(self.real().exp())
    }
    fn exp2(self) -> Self {
        Self::Real(self.real().exp2())
    }
    fn ln(self) -> Self {
        Self::Real(self.real().ln())
    }
    fn log(self, base: Self) -> Self {
        Self::Real(self.real().log(base.real()))
    }
    fn log2(self) -> Self {
        Self::Real(self.real().log2())
    }
    fn log10(self) -> Self {
        Self::Real(self.real().log10())
    }
    fn max(self, other: Self) -> Self {
        Self::Real(self.real().max(other.real()))
    }
    fn min(self, other: Self) -> Self {
        Self::Real(self.real().min(other.real()))
    }
    fn abs_sub(self, _other: Self) -> Self {
        unimplemented!()
    }
    fn cbrt(self) -> Self {
        Self::Real(self.real().cbrt())
    }
    fn hypot(self, other: Self) -> Self {
        Self::Real(self.real().hypot(other.real()))
    }
    fn sin(self) -> Self {
        Self::Real(self.real().sin())
    }
    fn cos(self) -> Self {
        Self::Real(self.real().cos())
    }
    fn tan(self) -> Self {
        Self::Real(self.real().tan())
    }
    fn asin(self) -> Self {
        Self::Real(self.real().asin())
    }
    fn acos(self) -> Self {
        Self::Real(self.real().acos())
    }
    fn atan(self) -> Self {
        Self::Real(self.real().atan())
    }
    fn atan2(self, other: Self) -> Self {
        Self::Real(self.real().atan2(other.real()))
    }
    fn sin_cos(self) -> (Self, Self) {
        let (s, c) = self.real().sin_cos();
        (Self::Real(s), Self::Real(c))
    }
    fn exp_m1(self) -> Self {
        Self::Real(self.real().exp_m1())
    }
    fn ln_1p(self) -> Self {
        Self::Real(self.real().ln_1p())
    }
    fn sinh(self) -> Self {
        Self::Real(self.real().sinh())
    }
    fn cosh(self) -> Self {
        Self::Real(self.real().cosh())
    }
    fn tanh(self) -> Self {
        Self::Real(self.real().tanh())
    }
    fn asinh(self) -> Self {
        Self::Real(self.real().asinh())
    }
    fn acosh(self) -> Self {
        Self::Real(self.real().acosh())
    }
    fn atanh(self) -> Self {
        Self::Real(self.real().atanh())
    }

    fn integer_decode(self) -> (u64, i16, i8) {
        unimplemented!()
    }
}

impl From<f64> for Number {
    #[inline]
    fn from(value: f64) -> Self {
        Number::Real(value)
    }
}
impl From<i64> for Number {
    #[inline]
    fn from(value: i64) -> Self {
        Number::Int(value)
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
