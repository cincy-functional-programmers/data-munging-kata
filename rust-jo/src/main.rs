//! Mung Some Data
#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![cfg_attr(feature="clippy", deny(clippy, clippy_pedantic))]
#![deny(missing_docs)]
extern crate clap;
#[macro_use]
extern crate lazy_static;
extern crate regex;

mod error;

use clap::{App, Arg};
use error::MungError;
use regex::{Captures, Regex};
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::path::PathBuf;

const WEATHER_DATA: &'static str = "../data/weather.dat";
const FOOTBALL_DATA: &'static str = "../data/football.dat";

lazy_static! {
    static ref WEATHER: Regex = Regex::new(r"^\s*(\d+)\s+(\d+)[\s\*]+(\d+)").unwrap();
    static ref FOOTBALL: Regex =
        Regex::new(r"\s*\d+\. (\w+)\s+(\d+\s+){4}(\d+)  -  (\d+)").unwrap();
}

type MungResult<T> = Result<T, MungError>;

enum Mode {
    Weather,
    Football,
}

struct Munger {
    filepath: PathBuf,
    mode: Mode,
}

impl Munger {
    fn new(mode: Mode) -> Munger {
        let filepath = match mode {
            Mode::Weather => PathBuf::from(WEATHER_DATA),
            Mode::Football => PathBuf::from(FOOTBALL_DATA),
        };

        Munger {
            filepath: filepath,
            mode: mode,
        }
    }

    #[cfg_attr(feature="clippy", allow(cast_sign_loss))]
    fn add_tuple<'a>(&self,
                     tuples: &mut Vec<(&'a str, usize)>,
                     caps: Captures<'a>,
                     max: usize,
                     min: usize) {
        if let (Some(col), Some(min_str), Some(max_str)) = (caps.at(1),
                                                            caps.at(max),
                                                            caps.at(min)) {
            if let (Ok(max), Ok(min)) = (max_str.parse::<isize>(), min_str.parse::<isize>()) {
                tuples.push((col, (max - min).abs() as usize));
            }
        }
    }

    fn mung(&self) -> MungResult<String> {
        let f = try!(File::open(&self.filepath));
        let reader = BufReader::new(f);

        let matches = reader.lines()
            .map(|line| match line {
                Ok(ln) => ln,
                Err(_) => "".to_owned(),
            })
            .filter(|line| {
                match self.mode {
                    Mode::Weather => WEATHER.is_match(line),
                    Mode::Football => FOOTBALL.is_match(line),
                }
            })
            .collect::<Vec<String>>();

        let mut tuples = Vec::new();

        for line in &matches {
            match self.mode {
                Mode::Weather => {
                    if let Some(caps) = WEATHER.captures(line) {
                        self.add_tuple(&mut tuples, caps, 3, 2);
                    }
                }
                Mode::Football => {
                    if let Some(caps) = FOOTBALL.captures(line) {
                        self.add_tuple(&mut tuples, caps, 4, 3);
                    }
                }
            }
        }

        let result_tuple = tuples.into_iter()
            .fold(("", usize::max_value()),
                  |acc, x| if x.1 < acc.1 { x } else { acc });

        let result = result_tuple.0.to_owned();
        Ok(result)
    }
}

fn main() {
    let matches = App::new("mung")
        .version("1.0")
        .author("Jason Ozias <jason.g.ozias@gmail.com>")
        .about("data munging kata")
        .arg(Arg::with_name("mode")
            .short("m")
            .long("mode")
            .value_name("MODE")
            .possible_values(&["w", "f"])
            .required(true)
            .help("Specity the mode (football or weather)")
            .takes_value(true))
        .get_matches();

    let mode = match matches.value_of("mode") {
        Some("w") => Mode::Weather,
        Some("f") => Mode::Football,
        _ => panic!("You shouldn't be here, EVER!"),
    };

    if let Ok(answer) = Munger::new(mode).mung() {
        writeln!(io::stdout(), "The answer is: {}", answer).expect("Unable to write to stdout!");
    } else {
        writeln!(io::stderr(), "Sadly, there is no answer").expect("Unable to write to stdout!");
    }
}
