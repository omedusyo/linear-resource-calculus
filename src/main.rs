mod interpreter;
mod repl;
mod tokenizer;
mod syntax;
mod identifier;
mod calculi;
mod pattern_branch;

use clap;
use rustyline:: history::FileHistory;
use tokenizer::TokenStream;
use repl::{Repl, CartesianRepl, LinearRepl, CombinedRepl};

// ===Command Line Arguments===
#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
enum CliCommand {
    #[command(subcommand)]
    Repl(CliSubcommandRepl),
}

#[derive(clap::Subcommand, Debug, Clone)]
enum CliSubcommandRepl {
    Cartesian {
        #[arg(short, long)]
        load: Option<String> 
    },
    Linear {
        #[arg(short, long)]
        load: Option<String> 
    },
    Combined {
        #[arg(short, long)]
        load: Option<String> 
    },
}

#[derive(Clone, Copy)]
enum Calculus {
    Cartesian,
    Linear,
    Combined
}

impl CliSubcommandRepl {
    fn to_calculus_mode(&self) -> Calculus {
        match self {
            Self::Cartesian { .. } => Calculus::Cartesian,
            Self::Linear { .. } => Calculus::Linear,
            Self::Combined { .. } => Calculus::Combined,
        }
    }

    fn file_to_load(&self) -> Option<&str> {
        match self {
            Self::Cartesian { load, .. } => load.as_deref(),
            Self::Linear { load, .. } => load.as_deref(),
            Self::Combined { load, .. } => load.as_deref(),
        }
    }
}

// ===Repl===
type IResult<I, O> = Result<(I, O), nom::Err<nom::error::Error<I>>>;
type Editor = rustyline::Editor<(), FileHistory>;

type IResult0<'a, O> = Result<(TokenStream<'a>, O), nom::Err<nom::error::Error<&'a str>>>;

fn repl(cli_subcommand_repl: CliSubcommandRepl) -> rustyline::Result<()> {
    use Calculus::*;
    let mode = cli_subcommand_repl.to_calculus_mode();
    let file = cli_subcommand_repl.file_to_load();
    match mode {
        Cartesian => {
            let mut repl = CartesianRepl::new()?;
            repl.start_loop(file)
        },
        Linear => {
            let mut repl = LinearRepl::new()?;
            repl.start_loop(file)
        },
        Combined => {
            let mut repl = CombinedRepl::new()?;
            repl.start_loop(file)
        },
    }
    Ok(())
}

fn main() -> rustyline::Result<()> {
    let mode = {
        use clap::Parser;
        CliCommand::parse()
    };
    use CliCommand::*;
    match mode {
        Repl(repl_mode) => repl(repl_mode),
    }
}
