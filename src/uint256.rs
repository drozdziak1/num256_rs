use failure::Error;
use num::{
    traits::{CheckedAdd, CheckedDiv, CheckedMul, CheckedRem, CheckedSub},
    BigUint, Bounded, Num, Zero,
};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use std::{
    fmt,
    ops::{Add, AddAssign, Deref, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Sub, SubAssign},
};

/// A 256-bit newtype wrapper for `num::BigUint`
#[derive(Clone, PartialEq, PartialOrd, FromPrimitive, ToPrimitive, Num, Zero, One)]
pub struct Uint256(pub BigUint);

impl Uint256 {
    pub fn from_bytes_le(slice: &[u8]) -> Result<Uint256, Error> {
        match &BigUint::from_bytes_le(slice) {
            value if value < &Self::max_value().0 => Ok(Uint256(value.clone())),
            overflowing => bail!(
                "Value of {} overflows in the 256-bit space!",
                overflowing.to_str_radix(10)
            ),
        }
    }
    pub fn from_bytes_be(slice: &[u8]) -> Result<Uint256, Error> {
        match BigUint::from_bytes_be(slice) {
            ref value if value < &Self::max_value().0 => Ok(Uint256(value.clone())),
            overflowing => bail!(
                "Value of {} overflows in the 256-bit space!",
                overflowing.to_str_radix(10)
            ),
        }
    }
    pub fn from_str_radix(s: &str, radix: u32) -> Result<Uint256, Error> {
        BigUint::from_str_radix(s, radix)
            .map(Uint256)
            .map_err(|e| format_err!("{}", std::error::Error::description(&e)))
    }
}

impl Bounded for Uint256 {
    fn min_value() -> Self {
        Self::zero()
    }
    fn max_value() -> Self {
        // BigUint's constructor takes a big-endian vec of base 2^32 digits.
        Uint256(BigUint::new(vec![std::u32::MAX; 8]))
    }
}

impl fmt::Display for Uint256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

impl fmt::Debug for Uint256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Uint256({})", self.to_string())
    }
}

impl fmt::LowerHex for Uint256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "0x")?;
        }
        write!(f, "{}", self.0.to_str_radix(16))
    }
}

impl fmt::UpperHex for Uint256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "0x")?;
        }
        write!(f, "{}", self.0.to_str_radix(16).to_uppercase())
    }
}

impl Serialize for Uint256 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = "0x".to_owned();
        s.push_str(&format!("{:x}", self.0));
        serializer.serialize_str(&s)
    }
}

impl<'de> Deserialize<'de> for Uint256 {
    fn deserialize<D>(deserializer: D) -> Result<Uint256, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let s = if s.starts_with("0x") { &s[2..] } else { &s };

        BigUint::from_str_radix(&s, 16)
            .map(Uint256)
            .map_err(serde::de::Error::custom)
    }
}

impl Deref for Uint256 {
    type Target = BigUint;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> Add<T> for Uint256
where
    T: Into<Uint256>,
{
    type Output = Uint256;
    fn add(self, v: T) -> Uint256 {
        let num: Uint256 = v.into();
        if self.0.bits() + num.bits() > 256 {
            panic!("overflow");
        }
        Uint256(self.0 + num.0)
    }
}

impl<T> AddAssign<T> for Uint256
where
    T: Into<Uint256>,
{
    fn add_assign(&mut self, v: T) {
        self.0 = self.0.clone() + v.into().0;
        if self.0.bits() > 256 {
            panic!("overflow");
        }
    }
}

impl CheckedAdd for Uint256 {
    fn checked_add(&self, v: &Uint256) -> Option<Uint256> {
        // drop down to wrapped bigint to stop from panicing in fn above
        if self.0.bits() + v.0.bits() > 256 {
            return None;
        }
        Some(Uint256(self.0.clone() + v.0.clone()))
    }
}

impl<T> Sub<T> for Uint256
where
    T: Into<Uint256>,
{
    type Output = Uint256;
    fn sub(self, v: T) -> Uint256 {
        let num = self.0 - v.into().0;
        if num.bits() > 256 {
            panic!("overflow");
        }
        Uint256(num)
    }
}

impl<T> SubAssign<T> for Uint256
where
    T: Into<Uint256>,
{
    fn sub_assign(&mut self, v: T) {
        self.0 = self.0.clone() - v.into().0;
        if self.0.bits() > 256 {
            panic!("overflow");
        }
    }
}

impl CheckedSub for Uint256 {
    fn checked_sub(&self, v: &Uint256) -> Option<Uint256> {
        self.0.checked_sub(&v.0).map(Uint256)
    }
}

impl<T> Mul<T> for Uint256
where
    T: Into<Uint256>,
{
    type Output = Uint256;
    fn mul(self, v: T) -> Uint256 {
        let num = self.0 * v.into().0;
        if num.bits() > 256 {
            panic!("overflow");
        }
        Uint256(num)
    }
}

impl<T> MulAssign<T> for Uint256
where
    T: Into<Uint256>,
{
    fn mul_assign(&mut self, v: T) {
        self.0 = self.0.clone() * v.into().0;
        if self.0.bits() > 256 {
            panic!("overflow");
        }
    }
}

impl CheckedMul for Uint256 {
    fn checked_mul(&self, v: &Uint256) -> Option<Uint256> {
        // drop down to wrapped bigint to stop from panicing in fn above
        let num = self.0.clone() * v.0.clone();
        if num.bits() > 256 {
            return None;
        }
        Some(Uint256(num))
    }
}

impl<T> DivAssign<T> for Uint256
where
    T: Into<Uint256>,
{
    fn div_assign(&mut self, v: T) {
        self.0 = self.0.clone() / v.into().0;
        if self.0.bits() > 256 {
            panic!("overflow");
        }
    }
}

impl<T> Div<T> for Uint256
where
    T: Into<Uint256>,
{
    type Output = Uint256;
    fn div(self, v: T) -> Uint256 {
        let num = self.0 / v.into().0;
        if num.bits() > 256 {
            panic!("overflow");
        }
        Uint256(num)
    }
}

impl CheckedDiv for Uint256 {
    fn checked_div(&self, v: &Uint256) -> Option<Uint256> {
        if *v == Uint256::zero() {
            return None;
        }
        // drop down to wrapped bigint to stop from panicking in fn above
        let num = self.0.clone() / v.0.clone();
        Some(Uint256(num))
    }
}

impl<T> Rem<T> for Uint256
where
    T: Into<Uint256>,
{
    type Output = Uint256;
    fn rem(self, v: T) -> Uint256 {
        let num = self.0 % v.into().0;
        if num.bits() > 256 {
            panic!("overflow");
        }
        Uint256(num)
    }
}

impl<T> RemAssign<T> for Uint256
where
    T: Into<Uint256>,
{
    fn rem_assign(&mut self, v: T) {
        self.0 = self.0.clone() % v.into().0;
        if self.0.bits() > 256 {
            panic!("overflow");
        }
    }
}

impl CheckedRem for Uint256 {
    fn checked_rem(&self, v: &Uint256) -> Option<Uint256> {
        let num = self.0.clone() % v.0.clone();
        Some(Uint256(num))
    }
}
