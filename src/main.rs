#![allow(
    clippy::needless_lifetimes,
    clippy::redundant_guards,
    clippy::legacy_numeric_constants
)]
use std::fs::File;
use std::io;
use std::path::PathBuf;

use clap::Parser;

mod corner;
mod error;
pub mod evaluator;
mod expressions;
mod intermediate_representation;
mod operators;
pub mod parser;
pub mod points;
pub mod statement;
mod stops;
mod svg;
mod values;

#[derive(Parser)]
struct Args {
    /// Input file, stdin if not present.
    #[clap(short, long)]
    input: Option<PathBuf>,
    /// Output file, stdout if not present.
    #[clap(short, long)]
    output: Option<PathBuf>,
    /// Output format
    #[clap(short, long, default_value = "svg")]
    format: OutputFormat,
    /// Whether to output debugging info, and file to output to.
    #[clap(short, long)]
    debug: Option<Option<PathBuf>>,
}

#[derive(Clone, Copy, Debug, Default, clap::ValueEnum)]
enum OutputFormat {
    #[default]
    Svg,
    Json,
}

fn main() -> Result<(), anyhow::Error> {
    let args = Args::parse();
    let input = if let Some(path) = args.input {
        std::fs::read_to_string(path)?
    } else {
        io::read_to_string(&mut io::stdin())?
    };
    let statements = parser::parse(&input).map_err(|err| anyhow::anyhow!("{err}"))?;
    let mut evaluator = evaluator::Evaluator::new();
    evaluator.evaluate_all(statements)?;
    if let Some(debug_output) = args.debug {
        let mut debug_output: Box<dyn io::Write> = if let Some(path) = debug_output {
            Box::new(File::create(path)?)
        } else {
            Box::new(io::stderr())
        };
        if let Err(e) = evaluator.write_debug(&mut debug_output) {
            eprintln!("{:?}", anyhow::Error::from(e));
        }
    }
    let mut output: Box<dyn io::Write> = if let Some(path) = args.output {
        Box::new(File::create(path)?)
    } else {
        Box::new(io::stdout())
    };
    let document = evaluator.into_document();
    match args.format {
        OutputFormat::Svg => {
            write!(output, r#"<?xml version="1.0" encoding="utf-8" ?>"#)?;
            ::svg::write(&mut output, &document.to_svg()?)?;
        }
        OutputFormat::Json => serde_json::to_writer_pretty(output, &document)?,
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{fs, path::Path};

    use crate::{evaluator::Evaluator, parser::parse};

    fn test_example(input_file: impl AsRef<Path>) -> anyhow::Result<()> {
        let input = fs::read_to_string(input_file)?;
        let statements = parse(&input).unwrap();
        let mut evaluator = Evaluator::new();
        evaluator.evaluate_all(statements)?;
        let _document = evaluator.into_document().to_svg()?;
        Ok(())
    }

    #[test]
    fn test_bart_example() {
        test_example("examples/bart.subway").unwrap();
    }

    #[test]
    fn test_simple_wyes_example() {
        test_example("examples/simple_wyes.subway").unwrap();
    }
}
