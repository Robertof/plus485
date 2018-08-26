//! A full-fledged tool to read data from Electres Plus-485 energy meters.

#[macro_use]
extern crate error_chain;
extern crate clap;

pub mod errors;
pub mod constants;
pub mod data;
mod num_utils;
