mod interpreter;
mod repl;
mod tokenizer;
mod identifier;
mod calculi;

use clap;
use rustyline:: history::FileHistory;
use tokenizer::TokenStream;
use repl::{Repl, CartesianRepl, LinearRepl};

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
}

#[derive(Clone, Copy)]
enum CalculusMode {
    Cartesian,
    Linear,
}

impl CliSubcommandRepl {
    fn to_calculus_mode(&self) -> CalculusMode {
        match self {
            Self::Cartesian { .. } => CalculusMode::Cartesian,
            Self::Linear { .. } => CalculusMode::Linear,
        }
    }
}

// ===Repl===
type IResult<I, O> = Result<(I, O), nom::Err<nom::error::Error<I>>>;
type Editor = rustyline::Editor<(), FileHistory>;

type IResult0<'a, O> = Result<(TokenStream<'a>, O), nom::Err<nom::error::Error<&'a str>>>;

fn repl(cli_subcommand_repl: CliSubcommandRepl) -> rustyline::Result<()> {
    use CalculusMode::*;
    let mode = cli_subcommand_repl.to_calculus_mode();
    match mode {
        Cartesian => {
            let mut repl = CartesianRepl::new()?;
            repl.start_loop()
        },
        Linear => {
            let mut repl = LinearRepl::new()?;
            repl.start_loop()
        }
    }
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
