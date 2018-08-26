// NOTE: those are only the base addresses of the registers. According to the required quantity,
// as many values as needed are fetched. See `Format` and `Register::format`.
//! Register numbers and other constants.
//! # Note about register numbers
//! The constants in this module only point to the first register of a particular category.
//! 
//! The others are automatically determined according to the quantity of the specific register
//! and - if applicable - the line.
//! For example, `REGISTER_VOLTAGE_L1` is `28` and its `quantity` is `2`, as such the relevant
//! register numbers are `28` and `29`. The register which contains `L2` immediately follows, and
//! has id `30`. The general formula is `base_no + 2 * (line_no - 1)`.
//! 
//! For `base_no` = `28` and `line_no` = `2`, `register_no` = `28 + 2 * 1` = `30`.

/// Line-agnostic register which contains the input voltage.
pub const REGISTER_VOLTAGE:    u8 = 0;
/// Register which contains the input voltage for L1.
pub const REGISTER_VOLTAGE_L1: u8 = 28;
/// Line-agnostic register which contains the current.
pub const REGISTER_CURRENT:    u8 = 2;
/// Register which contains the current for L1.
pub const REGISTER_CURRENT_L1: u8 = 34;
/// Line-agnostic register which contains the power.
pub const REGISTER_POWER:      u8 = 4;
/// Register which contains the power for L1.
pub const REGISTER_POWER_L1:   u8 = 40;
/// Line-agnostic register which contains the power factor.
pub const REGISTER_POWER_FACTOR:    u8 = 10;
/// Line-agnostic register which contains the active energy.
pub const REGISTER_ACTIVE_ENERGY:   u8 = 20;
/// Line-agnostic register which contains the reactive energy.
pub const REGISTER_REACTIVE_ENERGY: u8 = 23;
