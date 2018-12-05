extern crate num256;
#[macro_use]
extern crate lazy_static;
extern crate num;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate serde;

use num::{
    pow::pow,
    traits::{Bounded, CheckedAdd, CheckedMul, CheckedSub, FromPrimitive, ToPrimitive},
    BigInt,
};
use num256::{Int256, Uint256};
use std::ops::{Add, Div, Sub};

lazy_static! {
    static ref UINT256_MAX: Uint256 = Uint256::from_bytes_le(&[255u8; 32]).unwrap();
    static ref INT256_MAX: Int256 = Int256(pow(BigInt::from(2), 255) - BigInt::from(1));
    static ref INT256_MIN: Int256 = Int256(pow(BigInt::from(-2), 255) + BigInt::from(1));
    static ref INT256_MAX_AS_UINT256: Uint256 = {
        let mut biggest_int_le = [255u8; 32];
        biggest_int_le[31] = 127;
        Uint256::from_bytes_le(&biggest_int_le).unwrap()
    };
}

#[derive(Serialize, Deserialize, Debug)]
pub struct MyStruct {
    uint: Uint256,
    int: Int256,
}

#[test]
fn serialize() {
    let struc = MyStruct {
        uint: Uint256::min_value(),
        int: Int256::min_value(),
    };

    let expected = "{\"uint\":\"0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\",\"int\":\"-57896044618658097711785492504343953926634992332820282019728792003956564819967\"}";

    let j = serde_json::to_string(&struc).unwrap();

    assert_eq!(expected, j);
    let m: MyStruct = serde_json::from_str(expected).unwrap();

    assert_eq!(UINT256_MAX.clone(), m.uint);
    assert_eq!(INT256_MIN.clone(), m.int);
}

#[test]
fn test_from_uint() {
    let (a, b, c, d, e) = (
        Uint256::from_u8(8u8).unwrap(),
        Uint256::from_u16(8u16).unwrap(),
        Uint256::from_u32(8u32).unwrap(),
        Uint256::from_u64(8u64).unwrap(),
        Uint256::from_usize(8usize).unwrap(),
    );

    assert_eq!(a, b);
    assert_eq!(b, c);
    assert_eq!(c, d);
    assert_eq!(d, e);
}

#[test]
fn test_from_int() {
    let (a, b, c, d, e) = (
        Int256::from_i8(-8i8).unwrap(),
        Int256::from_i16(-8i16).unwrap(),
        Int256::from_i32(-8i32).unwrap(),
        Int256::from_i64(-8i64).unwrap(),
        Int256::from_isize(-8isize).unwrap(),
    );

    assert_eq!(a, b);
    assert_eq!(b, c);
    assert_eq!(c, d);
    assert_eq!(d, e);
}

#[test]
#[should_panic]
fn test_uint_add_panic() {
    let _val = UINT256_MAX.clone() + Uint256::from_u32(1u32).unwrap();
}

#[test]
fn test_uint_add_no_panic() {
    let _val = UINT256_MAX.clone() + Uint256::from_u32(0u32).unwrap();
}

#[test]
#[should_panic]
fn test_uint_from_add_panic() {
    let _val = UINT256_MAX.clone().add(Uint256::from_u32(1).unwrap());
}

#[test]
fn test_uint_from_add_no_panic() {
    let _val = UINT256_MAX.clone().add(Uint256::from_u32(0).unwrap());
}

#[test]
#[should_panic]
fn test_uint_sub_panic() {
    let _val = Uint256::from_u32(1u32)
        .unwrap()
        .sub(Uint256::from_u32(2u32).unwrap());
}

#[test]
fn test_uint_sub_no_panic() {
    assert_eq!(
        Uint256::from_u32(1u32)
            .unwrap()
            .sub(Uint256::from_u32(1u32).unwrap()),
        Uint256::from_u32(0u32).unwrap()
    );
}

#[test]
#[should_panic]
fn test_uint_from_sub_panic() {
    let _val = Uint256::from_u32(1u32)
        .unwrap()
        .sub(Uint256::from_u32(2).unwrap());
}

#[test]
fn test_uint_from_sub_no_panic() {
    assert_eq!(
        Uint256::from_u32(1u32)
            .unwrap()
            .sub(Uint256::from_u32(1).unwrap()),
        Uint256::from_u32(0u32).unwrap()
    );
}

#[test]
#[should_panic]
fn test_uint_mul_panic() {
    let _val: Uint256 = UINT256_MAX.clone() * Uint256::from_u32(2).unwrap();
}

#[test]
fn test_uint_mul_no_panic() {
    assert_eq!(
        Uint256::from_u32(3).unwrap() * Uint256::from_u32(2).unwrap(),
        Uint256::from_u32(6).unwrap()
    );
}

#[test]
#[should_panic]
fn test_uint_from_mul_panic() {
    let _val = UINT256_MAX.clone() * Uint256::from_u32(2).unwrap();
}

#[test]
fn test_uint_from_mul_no_panic() {
    assert_eq!(
        Uint256::from_u32(3).unwrap() * Uint256::from_u32(2).unwrap(),
        Uint256::from_u32(6).unwrap()
    );
}

#[test]
#[should_panic]
fn test_uint_div_panic() {
    let _val = UINT256_MAX.clone() / Uint256::from_u32(0).unwrap();
}

#[test]
fn test_uint_div_no_panic() {
    assert_eq!(
        Uint256::from_u32(6).unwrap() / Uint256::from_u32(2).unwrap(),
        Uint256::from_u32(3).unwrap()
    );
}

#[test]
fn test_uint_from_div_assign_no_panic() {
    assert_eq!(
        Uint256::from_u32(6)
            .unwrap()
            .div(Uint256::from_u32(2).unwrap()),
        Uint256::from_u32(3).unwrap()
    );
}

#[test]
#[should_panic]
fn test_uint_from_div_panic() {
    let _val = UINT256_MAX.clone().div(Uint256::from_u32(0).unwrap());
}

#[test]
fn test_uint_from_div_no_panic() {
    assert_eq!(
        Uint256::from_u32(6)
            .unwrap()
            .div(Uint256::from_u32(2).unwrap()),
        Uint256::from_u32(3).unwrap()
    );
}

#[test]
fn test_uint256() {
    assert!(
        UINT256_MAX
            .checked_add(&Uint256::from_u32(1u32).unwrap())
            .is_none(),
        "should return None adding 1 to biggest"
    );

    assert!(
        UINT256_MAX
            .checked_add(&Uint256::from_u32(0u32).unwrap())
            .is_some(),
        "should return Some adding 0 to biggest"
    );

    assert!(
        &Uint256::from_u32(1u32)
            .unwrap()
            .checked_sub(&Uint256::from_u32(2u32).unwrap())
            .is_none(),
        "should return None if RHS is larger than LHS"
    );

    assert!(
        &Uint256::from_u32(1u32)
            .unwrap()
            .checked_sub(&Uint256::from_u32(1u32).unwrap())
            .is_some(),
        "should return Some if RHS is not larger than LHS"
    );

    let num = &Uint256::from_u32(1u32)
        .unwrap()
        .checked_sub(&Uint256::from_u32(1u32).unwrap())
        .unwrap()
        .to_u32()
        .unwrap();
    assert_eq!(*num, 0, "1 - 1 should = 0");

    let num2 = &Uint256::from_u32(346u32)
        .unwrap()
        .checked_sub(&Uint256::from_u32(23u32).unwrap())
        .unwrap()
        .to_u32()
        .unwrap();

    assert_eq!(*num2, 323, "346 - 23 should = 323");
}

#[test]
#[should_panic]
fn test_int_add_panic() {
    let _val = INT256_MAX.clone() + Int256::from_u32(1).unwrap();
}

#[test]
fn test_int_add_no_panic() {
    let _val = INT256_MAX.clone() + Int256::from_u32(0).unwrap();
}

#[test]
#[should_panic]
fn test_int_sub_panic() {
    let _val = INT256_MIN.clone() - Int256::from_u32(1).unwrap();
}

#[test]
fn test_int_sub_no_panic() {
    assert_eq!(
        Int256::from_u32(1).unwrap() - Int256::from_u32(1).unwrap(),
        Int256::from_u32(0).unwrap()
    );
}

#[test]
#[should_panic]
fn test_int_mul_panic() {
    let _val = INT256_MIN.clone() * Int256::from_u32(2).unwrap();
}

#[test]
fn test_int_mul_no_panic() {
    assert_eq!(
        Int256::from_u32(3).unwrap() * Int256::from_u32(2).unwrap(),
        Int256::from_u32(6).unwrap()
    );
}

#[test]
#[should_panic]
fn test_int_div_panic() {
    let _val = INT256_MIN.clone() / Int256::from_u32(0).unwrap();
}

#[test]
fn test_int_div_no_panic() {
    assert_eq!(
        Int256::from_u32(6).unwrap() / Int256::from_u32(2).unwrap(),
        Int256::from_u32(3).unwrap()
    );
}

#[test]
#[should_panic]
fn test_int_from_add_panic() {
    let _val = INT256_MAX.clone() + 1;
}

#[test]
fn test_int_from_add_no_panic() {
    let _val = INT256_MAX.clone() + 0;
}

#[test]
#[should_panic]
fn test_int_from_sub_panic() {
    let _val = INT256_MIN.clone() - 1;
}

#[test]
fn test_int_from_sub_no_panic() {
    assert_eq!(
        Int256::from_u32(1).unwrap() - 1,
        Int256::from_u32(0).unwrap()
    );
}

#[test]
#[should_panic]
fn test_int_from_mul_panic() {
    let _val = INT256_MIN.clone() * 2;
}

#[test]
fn test_int_from_mul_no_panic() {
    assert_eq!(
        Int256::from_u32(3).unwrap() * 2,
        Int256::from_u32(6).unwrap()
    );
}

#[test]
#[should_panic]
fn test_int_from_div_panic() {
    let _val = INT256_MIN.clone() / 0;
}

#[test]
fn test_int_from_div_no_panic() {
    assert_eq!(
        Int256::from_u32(6).unwrap() / 2,
        Int256::from_u32(3).unwrap()
    );
}

#[test]
#[should_panic]
fn test_uint_to_int_panic() {
    Int256::from(
        INT256_MAX_AS_UINT256
            .clone()
            .add(Uint256::from_u32(1u32).unwrap()),
    );
}

#[test]
fn test_int256() {
    assert_eq!(
        Int256::from(
            INT256_MAX_AS_UINT256
                .clone()
                .add(Uint256::from_u32(0u32).unwrap())
        ),
        INT256_MAX.clone()
    );

    assert!(
        INT256_MAX
            .checked_add(&Int256::from_u32(1).unwrap())
            .is_none(),
        "should return None adding 1 to biggest"
    );
    assert!(
        INT256_MAX
            .checked_add(&Int256::from_u32(0).unwrap())
            .is_some(),
        "should return Some adding 0 to biggest"
    );

    assert!(
        INT256_MIN
            .checked_sub(&Int256::from_u32(1).unwrap())
            .is_none(),
        "should return None subtracting 1 from smallest"
    );
    assert!(
        INT256_MIN
            .checked_sub(&Int256::from_u32(0).unwrap())
            .is_some(),
        "should return Some subtracting 0 from smallest"
    );

    assert!(
        INT256_MIN
            .checked_mul(&Int256::from_u32(2).unwrap())
            .is_none()
    );
    assert!(
        INT256_MIN
            .checked_mul(&Int256::from_u32(1).unwrap())
            .is_some()
    );

    let num = &Int256::from_u32(345)
        .unwrap()
        .checked_sub(&Int256::from_u32(44).unwrap())
        .unwrap()
        .to_u32()
        .unwrap();

    assert_eq!(*num, 301, "345 - 44 should = 301");
}

#[test]
fn test_increment_2_to_the_power_of_255() {
    // This one was failing with ethereum_types::U256
    let mut value: Uint256 = "0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        .parse()
        .unwrap();
    assert_eq!(value.bits(), 255);
    value += 1;
    assert_eq!(value.bits(), 256);
}

#[test]
#[should_panic]
fn test_increment_2_to_the_power_of_256() {
    let mut value: Uint256 = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        .parse()
        .unwrap();
    assert_eq!(value.bits(), 256);
    value += 1;
    assert_eq!(value.bits(), 256);
}

#[test]
fn test_increment_2_to_the_power_of_256_checked() {
    //2**256-1
    let value: Uint256 = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        .parse()
        .unwrap();
    assert_eq!(value.bits(), 256);
    assert!(
        value
            .checked_add(&Uint256::from_u32(1u32).unwrap())
            .is_none()
    );
}

#[test]
fn test_uint_underflow() {
    let value: Uint256 = 0.into();
    let res = value.checked_sub(&Uint256::from_u32(1u32).unwrap());
    assert!(res.is_none());
}

#[test]
#[should_panic]
fn test_uint_underflow_assign() {
    let mut value: Uint256 = 0.into();
    value -= 1;
}
