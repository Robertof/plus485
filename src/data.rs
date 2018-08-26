//! Data objects such as `Register`s, `Line`s etc.

use std::str::FromStr;
use std::borrow::Borrow;
use std::{fmt, result};
use constants::*;
use clap;

/// A data format with which data is encoded inside registers.
#[derive(Debug, PartialEq)]
pub enum Format {
    /// A 16-bit floating-point decimal using BCD encoding for the mantissa.
    FloatingPoint16,
    /// A 32-bit decimal using BCD encoding for both the integer and fractional part.
    Bcd32
}

impl Format {
    /// The number of registers required to represent this value.
    /// 
    /// - For `FloatingPoint16`, 2 registers are required
    /// - For `Bcd32`, 3 registers are required.
    pub fn quantity (&self) -> u8 {
        match *self {
            Format::FloatingPoint16 => 2,
            Format::Bcd32 => 3
        }
    }

    /// Given a slice of unsigned 16-bit integers (representing the contents of the registers),
    /// returns the corresponding decoded value as a 64-bit floating-point number.
    /// 
    /// **NOTE**: `input` must be at least as large as [`quantity()`], otherwise `NaN` is returned.
    /// 
    /// [`quantity()`]: ./enum.Format.html#method.quantity
    pub fn decode_value (&self, input: &[u16]) -> f64 {
        use num_utils::*;
        match *self {
            Format::FloatingPoint16 => {
                if let [mantissa, exponent] = *input {
                    // mantissa is simply a 3-digit BCD-encoded decimal, exponent instead is
                    // in binary form (with 2's complement applied if negative)
                    (
                        // signum (MSB), from i8 to i16
                        signum (mantissa) as i16 *
                        // decoded mantissa (3 digits - safe cast from u32 to i16)
                        decode_bcd (mantissa as u32, 3) as i16
                    ) as f64 * 10_f64.powi ( // 10^exponent
                        // decoded exponent (safe cast from i16 to i32)
                        decode_exponent (exponent) as i32
                    )
                } else {
                    ::std::f64::NAN
                }
            },
            Format::Bcd32 => {
                // Bcd32 is made up by 8 BCD digits (encoded as two u16 values) and a fractional
                // part using 4 BCD digits (a single u16).
                if let [value1, value2, decimal_part] = *input {
                    let combined_value: u32 = ((value1 as u32) << 16) | value2 as u32;
                    decode_bcd (combined_value, 8) as f64 +
                        decode_bcd (decimal_part as u32, 4) as f64 * 0.0001
                } else {
                    ::std::f64::NAN
                }
            }
        }
    }
}

/// Enum which represents possible lines associated to a register.
/// 
/// When formatted, yields `L[num]` for variants which are not [`Line::None`], or an empty string
/// otherwise.
/// 
/// [`Line::None`]: ./enum.Line.html#variant.None
#[derive(Debug, PartialEq, PartialOrd, Eq, Copy, Clone)]
pub enum Line {
    /// No line - this refers to the line-agnostic register variants.
    None,
    /// L1
    One,
    /// L2
    Two,
    /// L3
    Three
}

impl Line {
    /// Returns `true` if the represented line isn't [`Line::None`].
    /// 
    /// [`Line::None`]: ./enum.Line.html#variant.None
    pub fn has_line (&self) -> bool { *self != Line::None }

    /// Returns the integer representation of this line, or `None` if the represented line is
    /// [`Line::None`].
    /// 
    /// [`Line::None`]: ./enum.Line.html#variant.None
    pub fn to_u8 (&self) -> Option<u8> {
        match *self {
            Line::One   => Some(1),
            Line::Two   => Some(2),
            Line::Three => Some(3),
            Line::None  => None
        }
    }
}

impl From<Option<u8>> for Line {
    fn from (number: Option<u8>) -> Line {
        match number {
            Some(1) => Line::One,
            Some(2) => Line::Two,
            Some(3) => Line::Three,
            _       => Line::None
        }
    }
}

impl fmt::Display for Line {
    fn fmt (&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(line_no) = self.to_u8() {
            write!(f, "L{}", line_no)
        } else {
            Ok(()) // don't write anything if we are none
        }
    }
}

/// Represents an unit.
pub type Unit = &'static str;
/// Represents a complete register value, composed by the value itself (`f64`) and its unit.
pub type RegisterValue = (f64, Unit);

/// A struct which represents a register without an associated line.
/// 
/// It is possible to obtain a `Register` from a `String` using the `from_str` trait:
/// ```
/// # use plus485::data::Register;
/// use std::str::FromStr;
/// assert_eq!(Register::from_str ("power_factor"), Ok(Register::PowerFactor));
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Register {
    Voltage,
    Current,
    Power,
    PowerFactor,
    ActiveEnergy,
    ReactiveEnergy
}

impl FromStr for Register {
    type Err = &'static str;
    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        Ok (match s {
            "voltage"         => Register::Voltage,
            "current"         => Register::Current,
            "power"           => Register::Power,
            "power_factor"    => Register::PowerFactor,
            "energy"          => Register::ActiveEnergy,
            "reactive_energy" => Register::ReactiveEnergy,
            _                 => return Err("Invalid register passed to Register::from_str")
        })
    }
}

impl Register {
    /// Given a [`Line`] variant, returns the corresponding register number.
    /// 
    /// [`Line`]: ./enum.Line.html
    pub fn number_for_line<T: Borrow<Line>>(&self, line: T) -> u8 {
        let line = line.borrow();
        let base_no = match *self {
            Register::Voltage if line.has_line() => REGISTER_VOLTAGE_L1,
            Register::Voltage                    => REGISTER_VOLTAGE,
            Register::Current if line.has_line() => REGISTER_CURRENT_L1,
            Register::Current                    => REGISTER_CURRENT,
            Register::Power   if line.has_line() => REGISTER_POWER_L1,
            Register::Power                      => REGISTER_POWER,
            // These other registers do not have line-specific equivalents, and as such are
            // returned immediately.
            Register::PowerFactor    => return REGISTER_POWER_FACTOR,
            Register::ActiveEnergy   => return REGISTER_ACTIVE_ENERGY,
            Register::ReactiveEnergy => return REGISTER_REACTIVE_ENERGY
        };
        // Use line_no to determine the correct register number to use.
        if let Some(line_no) = line.to_u8() {
            // There are two registers for each line (mantissa and exponent), we account for that
            // by multiplying `line_no - 1` by 2.
            return base_no + 2 * (line_no - 1)
        }
        base_no
    }

    /// Whether this register has a line-specific variant or not.
    /// 
    /// Only [`PowerFactor`], [`ActiveEnergy`] and [`ReactiveEnergy`] do not have a line-specific
    /// variant.
    /// 
    /// [`PowerFactor`]: ./enum.Register.html#variant.PowerFactor
    /// [`ActiveEnergy`]: ./enum.Register.html#variant.ActiveEnergy
    /// [`ReactiveEnergy`]: ./enum.Register.html#variant.ReactiveEnergy
    pub fn has_line_variant (&self) -> bool {
        match *self {
            Register::PowerFactor | Register::ActiveEnergy | Register::ReactiveEnergy => false,
            _ => true
        }
    }

    /// How data in this register is formatted.
    /// 
    /// # Example
    /// ```
    /// # use plus485::data::{Register, Format};
    /// assert_eq!(Register::Voltage.format(), Format::FloatingPoint16);
    /// assert_eq!(Register::ActiveEnergy.format(), Format::Bcd32);
    /// ```
    pub fn format (&self) -> Format {
        match *self {
            Register::Voltage | Register::Current | Register::Power | Register::PowerFactor
                => Format::FloatingPoint16,
            Register::ActiveEnergy | Register::ReactiveEnergy
                => Format::Bcd32
        }
    }

    /// The unit of this register. Returns an empty string if not applicable.
    /// 
    /// **NOTE:** values are always returned in non-SI prefixed units (e.g. `Wh` instead of `kWh`)
    /// 
    /// # Example
    /// ```
    /// # use plus485::data::Register;
    /// assert_eq!(Register::Voltage.unit(), "V")
    /// ```
    pub fn unit (&self) -> Unit {
        match *self {
            Register::Voltage => "V",
            Register::Current => "A",
            Register::Power => "W",
            Register::PowerFactor => "",
            Register::ActiveEnergy => "Wh",
            Register::ReactiveEnergy => "varh"
        }
    }

    /// Given a slice of unsigned 16-bit integers (representing the contents of the registers),
    /// returns the corresponding decoded value.
    /// 
    /// See also [`Format::decode_value()`].
    /// 
    /// [`Format::decode_value()`]: ./enum.Format.html#method.decode_value
    pub fn decode_value (&self, input: &[u16]) -> RegisterValue {
        let value = self.format().decode_value (&input);
        // Active energy and reactive energy are in kWh/kvarh, fix that.
        (
            match *self {
                Register::ActiveEnergy | Register::ReactiveEnergy => value * 1000_f64,
                _ => value
            },
            self.unit()
        )
    }
}

impl fmt::Display for Register {
    fn fmt (&self, f: &mut fmt::Formatter) -> fmt::Result {
        let is_alternate = f.alternate();
        write!(f, "{}", match *self {
            Register::Voltage => "V",
            Register::Current => "I",
            Register::Power   => "P",
            Register::PowerFactor => "PF",
            Register::ActiveEnergy => if is_alternate { "E" } else { "Active Energy" },
            Register::ReactiveEnergy => if is_alternate { "RE" } else { "Reactive Energy" }
        })
    }
}

/// A struct which represents a register along with an associated line.
/// See also [`Register`], [`Line`].
/// 
/// [`Register`]: ./enum.Register.html
/// [`Line`]: ./enum.Line.html
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct QualifiedRegister(pub Register, pub Line);

impl QualifiedRegister {
    /// Returns the corresponding `Register` object.
    pub fn register (&self) -> &Register { &self.0 }
    /// Returns the corresponding `Line` object.
    pub fn line (&self) -> &Line { &self.1 }
    /// Returns the number corresponding to the combination of this register and line.
    pub fn number (&self) -> u8 { self.register().number_for_line (self.line()) }
    /// The number of registers required to represent this value.
    pub fn quantity (&self) -> u8 { self.register().format().quantity() }
    /// Given a slice of unsigned 16-bit integers (representing the contents of the registers),
    /// returns the corresponding decoded value.
    pub fn decode_value (&self, input: &[u16]) -> RegisterValue {
        self.register().decode_value (input)
    }
}

impl fmt::Display for QualifiedRegister {
    fn fmt (&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#}{}", self.register(), self.line())
        } else {
            write!(f, "{}{}", self.register(), self.line())
        }
    }
}

/// Represents the different kinds of output formats.
#[derive(Debug)]
pub enum OutputFormat {
    /// Standard output format. One line per reading, includes register name, line number and
    /// unit.
    /// ```text
    /// $ plus485 -l 1 -VPIE /dev/ttyUSB0
    /// VL1 = 237 V
    /// PL1 = 82.2 W
    /// IL1 = 0.404 A
    /// Active Energy = 2997.3 Wh
    /// ```
    Normal,
    /// Compact output format. Space-delimited readings, does not include register name, line
    /// number and unit.
    /// ```text
    /// $ plus485 -l 1,2,3 -VI --compact /dev/ttyUSB0
    /// 237 62.5 62.4 0.432 0 0
    /// ```
    Compact,
    /// IEC 62056 or MeterN compatible format. One line per reading, includes device address,
    /// compact register name, line number and unit.
    /// ```text
    /// $ plus485 -l 1,2,3 -VI --metern /dev/ttyUSB0
    /// 1_VL1(237*V)
    /// 1_VL2(62.6*V)
    /// 1_VL3(62.5*V)
    /// 1_IL1(0.433*A)
    /// 1_IL2(0*A)
    /// 1_IL3(0*A)
    /// ```
    Iec62056(String)
}

impl OutputFormat {
    /// Returns an `OutputFormat` variant given the user-specified command line arguments.
    pub fn from_args (args: &clap::ArgMatches) -> OutputFormat {
        if args.is_present ("compact") {
            OutputFormat::Compact
        } else if args.is_present ("metern") {
            OutputFormat::Iec62056 (args.value_of ("address").unwrap().to_owned())
        } else {
            OutputFormat::Normal
        }
    }

    /// Displays a reading with the specified output format.
    pub fn display (&self, value: RegisterValue, register: &QualifiedRegister) {
        match *self {
            OutputFormat::Normal => println!("{name} = {value} {unit}", name = register,
                value = value.0, unit = value.1),
            OutputFormat::Compact => print!("{} ", value.0),
            OutputFormat::Iec62056 (ref address) => {
                // Prepend "*" to unit if it is actually non-empty.
                let unit = format!("{}{}", if value.1.is_empty() { "" } else { "*" }, value.1);
                // 1_VL1(220*V)
                println!("{addr}_{reg_short:#}({value}{unit})",
                    addr = address, reg_short = register, value = value.0, unit = unit)
            }
        }
    }

    /// Ensures a newline is printed whenever necessary (e.g. when using `OutputFormat::Compact`)
    pub fn ensure_newline (&self) {
        if let OutputFormat::Compact = *self {
            println!("")
        }
    }
}
