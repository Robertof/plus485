pub fn decode_bcd (number: u32, num_digits: u8) -> u32 {
    let mut output: u32 = 0;
    for digit in (0..num_digits).rev() {
        output = output * 10 + ((number >> digit * 4) & 0xF);
    }
    output
}

pub fn decode_exponent (exponent: u16) -> i16 {
    if is_negative (exponent) {
        // perform 2's complement
        -((!(exponent - 1) & 0xFFFF) as i16)
    } else {
        exponent as i16
    }
}

#[inline]
// MSB set to 1
pub fn is_negative (number: u16) -> bool { number >> 15 == 1 }
#[inline]
pub fn signum (number: u16) -> i8 { if is_negative(number) { -1 } else { 1 } }

#[test]
fn test_bcd_decoding() {
    assert_eq!(decode_bcd (0x0221,     3), 221);
    assert_eq!(decode_bcd (0x0708,     3), 708);
    assert_eq!(decode_bcd (0x8082,     3), 82);
    assert_eq!(decode_bcd (0x00748202, 8), 748202);
}

#[test]
fn test_exponent_decoding() {
    assert_eq!(decode_exponent (0), 0);
    assert_eq!(decode_exponent (0xFFFF), -1);
    assert_eq!(decode_exponent (0xFFFE), -2);
}

#[test]
fn test_signum() {
    assert!(is_negative (0x8000));
    assert_eq!(signum (0x8000), -1);
    assert_eq!(signum (0x1), 1);
}
