use std::error;
use std::fmt;
use std::io;
use std::num;

#[derive(Debug)]
pub enum MungError {
    Io(io::Error),
    ParseIntError(num::ParseIntError),
}

impl fmt::Display for MungError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MungError::Io(ref err) => err.fmt(f),
            MungError::ParseIntError(ref err) => err.fmt(f),
        }
    }
}

impl error::Error for MungError {
    fn description(&self) -> &str {
        match *self {
            MungError::Io(ref err) => err.description(),
            MungError::ParseIntError(ref err) => err.description(),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            MungError::Io(ref err) => Some(err),
            MungError::ParseIntError(ref err) => Some(err),
        }
    }
}

impl From<io::Error> for MungError {
    fn from(err: io::Error) -> MungError {
        MungError::Io(err)
    }
}

impl From<num::ParseIntError> for MungError {
    fn from(err: num::ParseIntError) -> MungError {
        MungError::ParseIntError(err)
    }
}
