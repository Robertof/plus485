#[macro_use]
extern crate clap;
extern crate error_chain;
extern crate futures;
extern crate tokio_core;
extern crate tokio_modbus;
extern crate tokio_serial;
extern crate tokio_timer;
extern crate plus485;

use std::{fs, process, io, thread};
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
use plus485::errors::*;
use plus485::data::{QualifiedRegister, Register, OutputFormat, Line};

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
    // Serial communication is unstable and sometimes dies/timeouts with no reasons.
    // As such, there are two retry mechanisms implemented:
    // - a basic timeout-then-retry implemented in read_from_meter_impl (up to 2 tries)
    // - a mechanism which actually closes and reopens the serial interface after 100ms and
    //   tries to read the data again (up to 3 tries), implemented here.
    let mut try_count = 1_u8;
    // This RefCell keeps track of the already processed items - it allows to remove them from
    // `to_fetch` before retrying.
    let processed_items = RefCell::new (0);
    let output_format = OutputFormat::from_args (&args);
    // Keep calling `read_from_meter_impl` until we do not receive an error.
    while let Err(error) = read_from_meter_impl (
        &args, &to_fetch[..], &processed_items, &output_format
    ) {
        match error {
            // Catch timeout errors and - if we're within our limits - retry by establishing a
            // brand new modbus instance.
            Error(ErrorKind::Io(ref error), _)
                if error.kind() == io::ErrorKind::TimedOut && try_count <= 3 =>
            {
                // Remove already processed items from `to_fetch`
                to_fetch.drain (0..*processed_items.borrow());
                *processed_items.borrow_mut() = 0;
                try_count += 1;
                // Give enough time to let the serial interface recover.
                thread::sleep (Duration::from_millis (100));
            },
            // TODO: we may want to catch and retry on "broken pipe" errors too. They seem to
            // happen quite frequently.
            Error(ErrorKind::Io(_), _) => {
                output_format.ensure_newline();
                eprintln!("{}",
                    error.chain_err (|| "can't retrieve data from modbus").display_chain());
                process::exit (1);
            },
            _ => {
                output_format.ensure_newline();
                eprintln!("{}", error.display_chain());
                process::exit (1);
            }
        }
    }
    output_format.ensure_newline();
}

fn read_from_meter_impl(
    args: &clap::ArgMatches,
    to_fetch: &[QualifiedRegister],
    processed_items: &RefCell<usize>,
    output_format: &OutputFormat
) -> Result<()> {
    // Configure the serial port we're going to use.
    let settings = SerialPortSettings {
        baud_rate: tokio_serial::BaudRate::Baud4800,
        stop_bits: tokio_serial::StopBits::Two,
        ..Default::default()
    };
    // Create a tokio reactor.
    let mut core = tokio_core::reactor::Core::new().chain_err (|| "can't create tokio reactor")?;
    let handle = core.handle();
    #[allow(unused_mut)] // for win32
    let mut port = Serial::from_path_with_handle (
        args.value_of ("serial_port").unwrap(), // this is safe - it's a mandatory argument
        &settings,
        &handle.new_tokio_handle()
    ).chain_err (|| "unable to open requested serial port")?;
    #[cfg(unix)]
    port.set_exclusive (false).chain_err (|| "unable to set port exclusivity")?;
    let mut client: Option<tokio_modbus::Client> = None;
    // Create the future.
    let task = tokio_modbus::Client::connect_rtu (
        port,
        args.value_of ("address").unwrap().parse().unwrap(), // safe
        &handle
    ).and_then (|local_client| {
        // The following is an unfortunate, required hack due to the various nesting closures used
        // to handle retries and so on:
        // - directly using `client` is not possible as it would either cause a move on an FnMut
        //   closure (which is forbidden), or a borrow with an insufficient lifetime
        // - using a RefCell would only be useful outside of this closure's scope, but we can't do
        //   it as the client is only defined at this step.
        // Option<T> - initially None - allows us to define it here and then use client.as_ref()
        // to obtain a valid borrow (without any kind of moving) inside the nested closures.
        client = Some(local_client);
        // We "convert" `to_fetch` to a stream of Future<Item = (), Error = io::Error> values.
        // `for_each` executes each promises and most importantly only executes the next one when
        // the previous is completed. Concurrent access does not work.
        futures::stream::iter_ok::<_, io::Error>(to_fetch).for_each (|register| {
            // To handle retries, loop until either we succeed or we reach the maximum number of
            // tries. We pass `register` as an argument to avoid the moving issues yet again.
            futures::future::loop_fn ((register, 1), |(register, tries)| {
                // Try to read the requested register (and quantity) with a maximum timeout of
                // 200ms.
                Timeout::new (
                    client.as_ref().unwrap().read_input_registers (
                        register.number() as u16,
                        register.quantity() as u16
                    ),
                    Duration::from_millis (200)
                )
                // Since this must return an io::Error, convert errors returned by tokio_timer
                // to an appopriate type.
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
                        // If we timed out and we are allowed to, return Loop::Continue and retry.
                        Err(ref error) if error.kind() == io::ErrorKind::TimedOut => {
                            if tries >= 2 {
                                // Otherwise, just propagate the `timed out` error.
                                Err(io::ErrorKind::TimedOut.into())
                            } else {
                                Ok(Loop::Continue((register, tries + 1)))
                            }
                        },
                        Err(error) => Err(error),
                        Ok(ref result) => {
                            // If we succeeded, display the read data using the requested
                            // `output_format` and stop the loop.
                            output_format.display (register.decode_value (result), register);
                            Ok(Loop::Break(()))
                        }
                    }
                })
            }).and_then (|_| {
                // If and only if we successfully retrieved a value, increase the total number
                // of processed items.
                *processed_items.borrow_mut() += 1;
                Ok(())
            })
        })
    });
    // Run the task.
    core.run (task)?;
    Ok(())
}
