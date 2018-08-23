#[macro_use]
extern crate clap;
extern crate futures;
extern crate tokio_core;
extern crate tokio_modbus;
extern crate tokio_serial;

use std::fs;
use std::borrow::Borrow;

#[derive(Debug)]
enum Format {
    FloatingPoint16,
    Bcd32
}

impl Format {
    fn get_quantity (&self) -> u8 {
        match *self {
            Format::FloatingPoint16 => 2,
            Format::Bcd32 => 3
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Copy, Clone)]
enum Line {
    None,
    One,
    Two,
    Three
}

impl Line {
    fn has_number (&self) -> bool { *self != Line::None }

    fn to_u8 (&self) -> Option<u8> {
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

#[derive(Debug)]
enum Register {
    Voltage,
    Current,
    Power,
    PowerFactor,
    ActiveEnergy,
    ReactiveEnergy
}

impl Register {
    fn get_number_for_line<T: Borrow<Line>>(&self, line: T) -> u8 {
        let line = line.borrow();
        let base_no = match *self {
            // V: 0, VL1: 28, VL2: 30, VL3: 32
            Register::Voltage        => if line.has_number() { 28 } else { 0 },
            // I: 2, IL1: 34, IL2: 36, IL3: 38
            Register::Current        => if line.has_number() { 34 } else { 2 },
            // P: 4, PL1: 40, PL2: 42, PL3: 44
            Register::Power          => if line.has_number() { 40 } else { 4 },
            // These other registers do not have line-specific equivalents, and as such are
            // returned immediately.
            Register::PowerFactor    => return 10,
            Register::ActiveEnergy   => return 20,
            Register::ReactiveEnergy => return 23
        };
        // Use line_no to determine the correct register number to use.
        if let Some(line_no) = line.to_u8() {
            return base_no + 2 * (line_no - 1)
        }
        base_no
    }

    fn get_number (&self) -> u8 { self.get_number_for_line (Line::None) }

    fn get_format (&self) -> Format {
        match *self {
            Register::Voltage | Register::Current | Register::Power | Register::PowerFactor
                => Format::FloatingPoint16,
            Register::ActiveEnergy | Register::ReactiveEnergy
                => Format::Bcd32
        }
    }
}

struct QualifiedRegister(Register, Line);

impl QualifiedRegister {
    fn get_number (&self) -> u8 { self.0.get_number_for_line (&self.1) }
}

fn main() {
    let args = clap_app!(plus485 =>
        (@setting DeriveDisplayOrder)
        (version: crate_version!())
        (about: "Query information through modbus from Electrex PLUS-485 meters")
        (author: "Roberto Frenna [https://roberto.frenna.pro]")
        (@arg address: -a --address +takes_value default_value("1") {
            // Ensures that the argument is an u8.
            |s: String| s.parse::<u8>()
                .map (|_| ())
                .map_err (|_| "must be numeric, >= 0 and <= 255".to_string())
        } "Address of the meter (must be numeric)")
        (@arg serial_port: * {
            // Ensures that the argument is a valid serial device.
            |s: String| fs::metadata (s)
                .map (|_| ())
                .map_err (|_| "must be a serial device".to_string())
        } "Serial port/device")
        
        (@group format =>
            (@arg compact: -c --compact
                "Outputs data in the compact format (space-delimited, same order as arguments)")
            (@arg metern: --metern alias[iec]
                "Outputs data in IEC 62056 (metern-compatible) format (--iec)")
        )

        (@arg line: -l --line +takes_value +multiple +require_delimiter value_delimiter(",")
            possible_value("1") possible_value("2") possible_value("3") possible_value("none")
            "Line number.\nCan be specified multiple times (multiple values can be delimited \
             with commas) and is relative to the last reading")

        (@group readings =>
            (@attributes +multiple +required)
            (@arg voltage: -V --voltage "Returns line voltage in volts (V)")
            (@arg current: -I --current "Returns current in amperes (A)")
            (@arg power: -P --power "Returns instantaneous power in watts (W)")
            (@arg powerFactor: --("power-factor") "Returns the power factor")
            (@arg energy: -E --energy "Returns energy in watt-hours (Wh)")
            (@arg reactiveEnergy: -R --("reactive-energy") "Returns reactive energy in VAr")
        )
    ).get_matches();

    let to_fetch = Vec::<QualifiedRegister>::new();
    // Line numbers can be specified multiple times in-between reading flags. For example:
    // plus485 -l 1 -V -l none -I -l 1,2,3 -PER /dev/ttyUSB0
    // This means:
    // - fetch the voltage from line 1
    // - fetch the current from no particular line
    // - fetch power, energy and reactive energy from all of the three lines.
    println!("{:#?}", args.indices_of("line").unwrap().collect::<Vec<_>>());

    println!("{:#?}", args.values_of("line").unwrap().collect::<Vec<_>>());
}
