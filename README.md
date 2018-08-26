# plus485
## A Rust tool to query information from Electrex PLUS energy meters

`plus485` allows to query information from modbus-enabled `PLUS-485` energy meters. It uses
[`tokio`](https://tokio.rs/) to provide fast, asynchronous I/O and in most cases has better
performance than native C code written using `libmodbus`.

The protocol documentation is available [here](http://www.alectryon.nl/meters/PLUS_modbus.pdf).

## Getting started

To get started, clone this repository or obtain one of the precompiled binaries. Then, run:

```sh
cargo build --release
./target/release/plus485 --help
```

Depending on the speed of your system, the compilation may take a while. For reference, it took
about 2 hours to compile on my Raspberry Pi.

## Usage

```usage
plus485 0.1.0
Roberto Frenna [https://roberto.frenna.pro]
Query information through modbus from Electrex PLUS-485 meters

USAGE:
    plus485 [FLAGS] [OPTIONS] <serial_port> <--voltage|--current|--power|
                                             --power-factor|--energy|--reactive-energy>

FLAGS:
    -c, --compact            Outputs data in the compact format (space-delimited,
                             same order as arguments)
        --metern             Outputs data in IEC 62056 (metern-compatible) format (--iec)
    -V, --voltage            Returns line voltage in volts (V)
    -I, --current            Returns current in amperes (A)
    -P, --power              Returns instantaneous power in watts (W)
        --power-factor       Returns the power factor
    -E, --energy             Returns energy in watt-hours (Wh)
    -R, --reactive-energy    Returns reactive energy (varh)
    -h, --help               Prints help information
        --version            Prints version information

OPTIONS:
    -a, --address <address>    Address of the meter (must be numeric) [default: 1]
    -l, --line <line>...       Line number.
                               Can be specified multiple times (multiple values can be delimited
                               with commas), propagates to values specified after it until 'none'
                               is encountered [possible values: 1, 2, 3, none]

ARGS:
    <serial_port>    Serial port/device

ABOUT LINE NUMBERS:
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
```

### Examples

```
# obtain voltage, current and power from Line 1, then obtain energy
$ plus485 -l 1 -VIPE /dev/ttyUSB0
VL1 = 238 V
IL1 = 0.423 A
PL1 = 86.5 W
Active Energy = 3036.9 Wh

# obtain voltage and current from Line 1, 2 and 3. Use the IEC 62056 format and query the device
# at modbus address 2
$ plus485 -l 1,2,3 -VI --metern -a 2 /dev/ttyUSB0
2_VL1(239*V)
2_VL2(60.2*V)
2_VL3(60.1*V)
2_IL1(0*A)
2_IL2(0*A)
2_IL3(0*A)
```

## License

This software is released under the simplified BSD license. See [LICENSE](LICENSE).
