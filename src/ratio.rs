use std::ops::{Add, Mul, Neg};
use std::str::FromStr;

#[derive(Copy, Clone, Eq, Debug)]
pub struct Ratio {
    num: i64,
    den: u64,
}

impl Ratio {
    fn normalise(&mut self) -> Self {
        let g = gcd(self.den, self.num.unsigned_abs());
        self.num /= g as i64;
        self.den /= g;
        *self
    }
    pub fn is_zero(&self) -> bool {
        self.num == 0
    }
    pub fn approximation(&self) -> f64 {
        self.num as f64 / self.den as f64
    }
    pub fn inverse(&self) -> Ratio {
        Ratio {
            num: if self.num < 0 {
                -(self.den as i64)
            } else {
                self.den as i64
            },
            den: self.num.unsigned_abs(),
        }
    }
    pub fn floor(&self) -> Ratio {
        if self.num >= 0 || self.num % self.den as i64 == 0 {
            Ratio::from(self.num / self.den as i64)
        } else {
            Ratio::from(self.num / self.den as i64 - 1)
        }
    }
}

impl PartialEq for Ratio {
    fn eq(&self, other: &Self) -> bool {
        self.num * (other.den as i64) == other.num * (self.den as i64)
    }
}

impl PartialOrd for Ratio {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        i64::partial_cmp(
            &(self.num * (other.den as i64)),
            &(other.num * (self.den as i64)),
        )
    }
}

impl Add for Ratio {
    type Output = Ratio;

    fn add(self, rhs: Ratio) -> Ratio {
        Ratio {
            num: self.num * rhs.den as i64 + rhs.num * self.den as i64,
            den: self.den * rhs.den,
        }
        .normalise()
    }
}

impl Mul for Ratio {
    type Output = Ratio;

    fn mul(self, rhs: Ratio) -> Ratio {
        Ratio {
            num: self.num * rhs.num,
            den: self.den * rhs.den,
        }
        .normalise()
    }
}

impl Neg for Ratio {
    type Output = Ratio;

    fn neg(self) -> Ratio {
        Ratio {
            num: -self.num,
            den: self.den,
        }
        .normalise()
    }
}

impl FromStr for Ratio {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.split('/');
        let num = it.next().unwrap().parse()?;
        let den = it.next().map(|x| x.parse()).unwrap_or(Ok(1))?;
        Ok(Ratio { num, den }.normalise())
    }
}

impl From<i64> for Ratio {
    fn from(x: i64) -> Self {
        Ratio { num: x, den: 1 }.normalise()
    }
}

impl std::fmt::Display for Ratio {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.den == 1 {
            write!(f, "{}", self.num)
        } else {
            write!(f, "{}/{}", self.num, self.den)
        }
    }
}

fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        a
    } else if a < b {
        gcd(b, a)
    } else {
        gcd(b, a % b)
    }
}

pub const ONE: Ratio = Ratio { num: 1, den: 1 };
pub const ZERO: Ratio = Ratio { num: 0, den: 1 };
