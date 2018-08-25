#[macro_use]
extern crate clap;
#[macro_use]
extern crate error_chain;
extern crate futures;
extern crate tokio_core;
extern crate tokio_modbus;
extern crate tokio_serial;
extern crate tokio_timer;

use std::{fs, process, io, thread, fmt};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::str::FromStr;
use std::time::Duration;
use error_chain::ChainedError;
use futures::{Stream, Future};
use futures::future::Loop;
use tokio_timer::Timeout;
use tokio_serial::{Serial, SerialPortSettings};
use tokio_modbus::ModbusClient;

mod errors {
    error_chain! {
        foreign_links {
            Io(::std::io::Error);
        }
    }
}

mod num_utils {
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
}

use errors::*;

// Constants
// NOTE: those are only the base addresses of the registers. According to the required quantity,
// as many values as needed are fetched. See `Format` and `Register::format`.
const REGISTER_VOLTAGE:    u8 = 0;
const REGISTER_VOLTAGE_L1: u8 = 28;
const REGISTER_CURRENT:    u8 = 2;
const REGISTER_CURRENT_L1: u8 = 34;
const REGISTER_POWER:      u8 = 4;
const REGISTER_POWER_L1:   u8 = 40;
const REGISTER_POWER_FACTOR:    u8 = 10;
const REGISTER_ACTIVE_ENERGY:   u8 = 20;
const REGISTER_REACTIVE_ENERGY: u8 = 23;

#[derive(Debug)]
enum Format {
    FloatingPoint16,
    Bcd32
}

impl Format {
    fn quantity (&self) -> u8 {
        match *self {
            Format::FloatingPoint16 => 2,
            Format::Bcd32 => 3
        }
    }

    fn decode_value (&self, input: &[u16]) -> f64 {
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
                    std::f64::NAN
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
                    std::f64::NAN
                }
            }
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
    fn has_line (&self) -> bool { *self != Line::None }

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

impl fmt::Display for Line {
    fn fmt (&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(line_no) = self.to_u8() {
            write!(f, "L{}", line_no)
        } else {
            Ok(()) // don't write anything if we are none
        }
    }
}

type Unit = &'static str;
type RegisterValue = (f64, Unit);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Register {
    Voltage,
    Current,
    Power,
    PowerFactor,
    ActiveEnergy,
    ReactiveEnergy
}

impl FromStr for Register {
    type Err = &'static str;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
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
    fn number_for_line<T: Borrow<Line>>(&self, line: T) -> u8 {
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

    fn has_line_variant (&self) -> bool {
        match *self {
            Register::PowerFactor | Register::ActiveEnergy | Register::ReactiveEnergy => false,
            _ => true
        }
    }

    fn format (&self) -> Format {
        match *self {
            Register::Voltage | Register::Current | Register::Power | Register::PowerFactor
                => Format::FloatingPoint16,
            Register::ActiveEnergy | Register::ReactiveEnergy
                => Format::Bcd32
        }
    }

    fn unit (&self) -> Unit {
        match *self {
            Register::Voltage => "V",
            Register::Current => "A",
            Register::Power => "W",
            Register::PowerFactor => "",
            Register::ActiveEnergy => "Wh",
            Register::ReactiveEnergy => "varh"
        }
    }

    fn decode_value (&self, input: &[u16]) -> RegisterValue {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct QualifiedRegister(Register, Line);

impl QualifiedRegister {
    fn register (&self) -> &Register { &self.0 }
    fn line (&self) -> &Line { &self.1 }
    fn number (&self) -> u8 { self.register().number_for_line (self.line()) }
    fn quantity (&self) -> u8 { self.register().format().quantity() }
    fn decode_value (&self, input: &[u16]) -> RegisterValue {
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

#[derive(Debug)]
enum OutputFormat {
    Normal,
    Compact,
    Iec62056(String)
}

impl OutputFormat {
    fn from_args (args: &clap::ArgMatches) -> OutputFormat {
        if args.is_present ("compact") {
            OutputFormat::Compact
        } else if args.is_present ("metern") {
            OutputFormat::Iec62056 (args.value_of ("address").unwrap().to_owned())
        } else {
            OutputFormat::Normal
        }
    }

    fn display (&self, value: RegisterValue, register: &QualifiedRegister) {
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

    // Ensures a newline is printed when using OutputFormat::Compact.
    fn finish (&self) {
        if let OutputFormat::Compact = *self {
            println!("")
        }
    }
}

const DOCUMENTATION: &'static str = r#"ABOUT LINE NUMBERS:
    Once set using the command line parameter `-l`, `--line`, line numbers
    persist to the arguments specified after the parameter itself, until
    `-l none` is encountered. Here are some examples:
        plus485 -l 1 --voltage -l 2,3 --current -R -l none --power ...
          Reads `voltage` from line 1, `current` from line 1, 2 and 3.
          `reactive-energy` has no line-specific equivalent, and as such it is
          only read one time. Finally, `power` is also read from the
          non-line-specific register.
        plus485 -l 1 -V -l none -I -l 1,2,3 -PER ...
          Reads `voltage` from line 1, `current` from the non-line-specific
          register, `power` from line 1, 2, 3 and finally `energy` and
          `reactive-energy` have no line-specific equivalent, and as such are
          only read one time.
"#;

fn main() {
    let args = clap_app!(plus485 =>
        (@setting DeriveDisplayOrder)
        (version: crate_version!())
        (about: "Query information through modbus from Electrex PLUS-485 meters")
        (author: "Roberto Frenna [https://roberto.frenna.pro]")
        (after_help: DOCUMENTATION)
        (@arg address: -a --address +takes_value default_value("1") {
            // Ensures that the argument is an u8.
            |s: String| s.parse::<u8>()
                .map (|_| ())
                .map_err (|_| "must be numeric, >= 0 and <= 255".to_string())
        } "Address of the meter (must be numeric)")
        (@arg serial_port: * {
            // Ensures that the argument is a valid serial device.
            |s: String| {
                #[cfg(windows)]
                let is_win32_port = s.starts_with ("COM");
                #[cfg(not(windows))]
                let is_win32_port = false;
                if is_win32_port {
                    Ok(())
                } else {
                    fs::metadata (s)
                        .map (|_| ())
                        .map_err (|_| "must be a serial device".to_string())
                }
            }
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
             with commas), propagates to values specified after it until 'none' is encountered")

        (@group readings =>
            (@attributes +multiple +required)
            (@arg voltage: -V --voltage "Returns line voltage in volts (V)")
            (@arg current: -I --current "Returns current in amperes (A)")
            (@arg power: -P --power "Returns instantaneous power in watts (W)")
            (@arg power_factor: --("power-factor") "Returns the power factor")
            (@arg energy: -E --energy "Returns energy in watt-hours (Wh)")
            (@arg reactive_energy: -R --("reactive-energy") "Returns reactive energy (varh)")
        )
    ).get_matches();

    // This will be the final list of registers to read.
    let mut to_fetch = Vec::<QualifiedRegister>::new();

    // Lines can be specified multiple times in-between reading flags. For example:
    // plus485 -l 1 -V -l none -I -l 1,2,3 -PER /dev/ttyUSB0
    // This means:
    // - fetch the voltage from line 1
    // - fetch the current from no particular line
    // - fetch power, energy and reactive energy from all of the three lines.
    
    // This tree map is only defined when we actually got line parameters, and maps the indices
    // of the found `line` parameters to the specified argument.
    // We're using a tree map because we need to be able to determine indices in a specific range,
    // the one between the last "none" line and the index of a register (as passed by the user).
    // E.g. (1, "one"), (2, "two"), ...
    let line_map: Option<BTreeMap<usize, &str>> = args
        .indices_of ("line")
        // Zip indices with argument values. We can safely `unwrap()` values_of if we know that
        // `indices_of` is not empty.
        .map (|indices| indices.zip (args.values_of ("line").unwrap()).collect());
    // This vector uses the same principle of `line_map`. It contains mappings (tuples) composed
    // of register argument indexes and register names (as passed by the user).
    // It is a vector because we do not need to index by key.
    let mut register_map: Vec<(usize, &str)> =
        ["voltage", "current", "power", "power_factor", "energy", "reactive_energy"]
        .into_iter()
        // Filter out non-requested registers from this list and create the mappings, as specified
        // before.
        .filter_map (|reg_name| args.index_of (reg_name).map (|index| (index, *reg_name)))
        .collect();
    // Here comes the trick: sort the entire vector by the argument index. This ensures that
    // `to_fetch` will be populated in order, as we want results to follow the order of the passed
    // arguments.
    register_map.sort_unstable_by (|a, b| a.0.cmp (&b.0));
    // This holds the index of the last line argument which was "none". Consider the following
    // arguments:
    // -l 1 --voltage -l 2,3 --current -l none --power
    // The first line argument specifies that we're interested in reading L1. The next argument
    // specifies that we want to read the voltage from L1.
    // Then, L2 and L3 are also specified, along with the `current` argument, which means that
    // we want to read the current from all of the three lines.
    // The next argument tells that we're discarding the previous `line` values, and that we want
    // to read the generic (averaged) power value. This is why this variable is needed - we store
    // the last time we found a `none` value to avoid re-using previous `line` values which are not
    // valid anymore.
    let mut last_none_index = 0;
    for (register_index, register_name) in register_map {
        let register = Register::from_str (register_name)
            .expect ("register from register_name not OK");
        // If this register does not allow line variations, just use Line::None.
        if !register.has_line_variant() {
            to_fetch.push (QualifiedRegister (register, Line::None));
        } else if let Some(line_map) = line_map.as_ref() {
            // Keep a temporary vec of QualifiedRegisters to allow clearing them if we find
            // that `-l none` was specified.
            let mut requested_registers = Vec::<QualifiedRegister>::new();
            // Iterate from `-l` arguments specified between `last_none_index` and the index of
            // this register argument.
            for (line_index, line) in line_map.range (last_none_index..register_index) {
                // &str -> Option<u8> -> Line
                let line: Line = line.parse::<u8>().ok().into();
                // If we got `-l none`, clear `requested_registers` and update `last_none_index`.
                if let Line::None = line {
                    requested_registers.clear();
                    last_none_index = *line_index + 1;
                }
                // We're done - push QualifiedRegister to `requested_registers`.
                requested_registers.push (QualifiedRegister (register, line))
            }
            // Merge `requested_registers` and `to_fetch`.
            to_fetch.append (&mut requested_registers);
        } else {
            // Easy peasy if we don't have any `-l` argument.
            to_fetch.push (QualifiedRegister (register, Line::None));
        }
    }
    // We're done parsing arguments.
    read_from_meter (&args, to_fetch);    
}

fn read_from_meter(
    args: &clap::ArgMatches,
    mut to_fetch: Vec<QualifiedRegister>
) {
    let mut try_count = 1_u8;
    let processed_items = RefCell::new (0);
    let output_format = OutputFormat::from_args (&args);
    while let Err(error) = read_from_meter_impl (
        &args, &to_fetch[..], &processed_items, &output_format
    ) {
        match error {
            // Catch timeout errors and - if we're within our limits - retry by establishing a
            // brand new modbus instance.
            Error(ErrorKind::Io(ref error), _)
                if error.kind() == io::ErrorKind::TimedOut && try_count <= 3 =>
            {
                to_fetch.drain (0..*processed_items.borrow());
                *processed_items.borrow_mut() = 0;
                try_count += 1;
                // Give enough time to let the serial interface recover.
                thread::sleep (Duration::from_millis (100));
            },
            Error(ErrorKind::Io(_), _) => {
                output_format.finish();
                eprintln!("{}",
                    error.chain_err (|| "can't retrieve data from modbus").display_chain());
                process::exit (1);
            },
            _ => {
                output_format.finish();
                eprintln!("{}", error.display_chain());
                process::exit (1);
            }
        }
    }
    output_format.finish();
}

fn read_from_meter_impl(
    args: &clap::ArgMatches,
    to_fetch: &[QualifiedRegister],
    processed_items: &RefCell<usize>,
    output_format: &OutputFormat
) -> Result<()> {
    let settings = SerialPortSettings {
        baud_rate: tokio_serial::BaudRate::Baud4800,
        stop_bits: tokio_serial::StopBits::Two,
        ..Default::default()
    };
    let mut core = tokio_core::reactor::Core::new().unwrap();
    let handle = core.handle();
    #[allow(unused_mut)] // for win32
    let mut port = Serial::from_path_with_handle (
        args.value_of ("serial_port").unwrap(),
        &settings,
        &handle.new_tokio_handle()
    ).chain_err (|| "unable to open requested serial port")?;
    #[cfg(unix)]
    port.set_exclusive (false).chain_err (|| "unable to set port exclusivity")?;
    let mut client: Option<tokio_modbus::Client> = None;
    let task = tokio_modbus::Client::connect_rtu (
        port,
        args.value_of ("address").unwrap().parse().unwrap(), // safe
        &handle
    ).and_then (|local_client| {
        client = Some(local_client);
        futures::stream::iter_ok::<_, io::Error>(to_fetch).for_each (|register| {
            futures::future::loop_fn ((register, 1), |(register, tries)| {
                Timeout::new (
                    client.as_ref().unwrap().read_input_registers (
                        register.number() as u16,
                        register.quantity() as u16
                    ),
                    Duration::from_millis (200)
                )
                .map_err (|e|
                    if e.is_elapsed() {
                        io::ErrorKind::TimedOut.into()
                    } else if e.is_inner() {
                        e.into_inner().unwrap()
                    } else {
                        io::Error::new (io::ErrorKind::Other, e)
                    }
                )
                .then (move |val| {
                    match val {
                        Err(ref error) if error.kind() == io::ErrorKind::TimedOut => {
                            if tries >= 2 {
                                Err(io::ErrorKind::TimedOut.into())
                            } else {
                                Ok(Loop::Continue((register, tries + 1)))
                            }
                        },
                        Err(error) => Err(error),
                        Ok(ref result) => {
                            output_format.display (register.decode_value (result), register);
                            Ok(Loop::Break(()))
                        }
                    }
                })
            }).and_then (|_| {
                *processed_items.borrow_mut() += 1;
                Ok(())
            })
        })
    });
    core.run (task)?;
    Ok(())
}
