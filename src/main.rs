mod parser;
mod scanner;
mod keywords;

use crate::keywords::load_keywords;
use crate::parser::parser::Parser;
use crate::scanner::scanner::Scanner;
use crate::scanner::token::TokenType;
use anyhow::Result;
use clap::Parser as ClapParser;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::io::{BufRead, BufReader, Write};

#[derive(ClapParser)]
#[command(name = "gaul")]
#[command(about = "The Gaul programming language")]
struct Cli {
    /// Script file to run (omit for REPL)
    script: Option<String>,

    /// Path to keywords JSON file
    #[arg(short, long)]
    keywords: Option<String>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let keywords = load_keywords(cli.keywords.as_deref())?;

    match cli.script {
        None => run_prompt(&keywords)?,
        Some(path) => run_file(&path, &keywords)?,
    }

    Ok(())
}

fn run_prompt(keywords: &HashMap<String, TokenType>) -> Result<()> {
    let stdin = io::stdin();
    let reader = BufReader::new(stdin.lock());

    print!("> ");
    io::stdout().flush()?;

    for line in reader.lines() {
        run(&line?, keywords)?;
        print!("> ");
        io::stdout().flush()?;
    }

    Ok(())
}

fn run_file(path: &str, keywords: &HashMap<String, TokenType>) -> Result<()> {
    let contents = fs::read_to_string(path)?;
    run(&contents, keywords)?;
    Ok(())
}

fn run(source: &str, keywords: &HashMap<String, TokenType>) -> Result<()> {
    let scanner = Scanner::new(source, keywords);

    match scanner.scan_tokens() {
        Ok(tokens) => {
            tokens.iter().for_each(|token| println!("{:?}", token));

            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(expr) => println!("Parsed: {:?}", expr),
                Err(e) => eprintln!("Parse error: {:?}", e),
            }
        }
        Err(errors) => {
            errors.iter().for_each(|e| eprintln!("{}", e));
        }
    }

    Ok(())
}