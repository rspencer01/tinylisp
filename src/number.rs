use std::str::FromStr;

use crate::ratio::Ratio;

#[derive(Copy, Clone)]
pub enum Number {
    Ratio(Ratio),
    Approximation(f32),
}

impl Number {
    pub fn is_zero(&self) -> bool {
        match self {
            Number::Ratio(r) => r.is_zero(),
            Number::Approximation(x) => *x == 0.0,
        }
    }

    pub fn inverse(&self) -> Number {
        match self {
            Number::Ratio(r) => Number::Ratio(r.inverse()),
            Number::Approximation(x) => Number::Approximation(1.0 / *x),
        }
    }

    fn approximation(&self) -> f32 {
        match self {
            Number::Ratio(r) => r.approximation(),
            Number::Approximation(x) => *x,
        }
    }
}

impl std::ops::Add for Number {
    type Output = Number;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Ratio(r1), Number::Ratio(r2)) => Number::Ratio(r1 + r2),
            (n1, n2) => Number::Approximation(n1.approximation() + n2.approximation()),
        }
    }
}

impl std::ops::Mul for Number {
    type Output = Number;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Ratio(r1), Number::Ratio(r2)) => Number::Ratio(r1 * r2),
            (n1, n2) => Number::Approximation(n1.approximation() * n2.approximation()),
        }
    }
}

impl std::ops::Neg for Number {
    type Output = Number;
    fn neg(self) -> Self::Output {
        match self {
            Number::Ratio(r) => Number::Ratio(-r),
            Number::Approximation(x) => Number::Approximation(-x),
        }
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Ratio(r) => write!(f, "{}", r),
            Number::Approximation(x) => write!(f, "{}", x),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Ratio(r1), Number::Ratio(r2)) => r1.eq(r2),
            (n1, n2) => n1.approximation().eq(&n2.approximation()),
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Number::Ratio(r1), Number::Ratio(r2)) => r1.partial_cmp(r2),
            (n1, n2) => n1.approximation().partial_cmp(&n2.approximation()),
        }
    }
}

impl FromStr for Number {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(r) = Ratio::from_str(s) {
            Ok(Number::Ratio(r))
        } else if let Ok(x) = f32::from_str(s) {
            Ok(Number::Approximation(x))
        } else {
            Err(())
        }
    }
}
impl From<i32> for Number {
    fn from(r: i32) -> Self {
        Number::Ratio(r.into())
    }
}
impl From<f32> for Number {
    fn from(x: f32) -> Self {
        Number::Approximation(x)
    }
}

pub const ONE: Number = Number::Ratio(crate::ratio::ONE);
pub const ZERO: Number = Number::Ratio(crate::ratio::ZERO);
