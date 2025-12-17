mod keywords;
mod parser;
mod scanner;

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
    let mut reader = BufReader::new(stdin.lock());
    let mut buffer = String::new();

    loop {
        // Change prompt if we are in the middle of a multi-line block
        if buffer.is_empty() {
            print!("> ");
        } else {
            print!("| ");
        }
        io::stdout().flush()?;

        let mut line = String::new();
        let bytes_read = reader.read_line(&mut line)?;

        if bytes_read == 0 {
            break;
        } // Ctrl+D

        buffer.push_str(&line);

        // Check if the current buffer has balanced braces/parens
        if is_complete(&buffer) {
            if !buffer.trim().is_empty() {
                if let Err(e) = run(&buffer, keywords) {
                    eprintln!("Execution error: {}", e);
                }
            }
            buffer.clear();
        }
    }
    Ok(())
}

fn is_complete(code: &str) -> bool {
    let mut indent = 0;
    let mut in_string = false;
    let mut escaped = false;
    let mut iter = code.chars().peekable();

    while let Some(c) = iter.next() {
        if in_string {
            if escaped {
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if c == '"' {
                in_string = false;
            } else if c == '\n' {
                // Unterminated string on this line - but keep going,
                // the scanner will report the error
                in_string = false;
            }
            continue;
        }

        match c {
            '"' => in_string = true,
            '/' => {
                if iter.peek() == Some(&'/') {
                    while let Some(&next) = iter.peek() {
                        if next == '\n' {
                            break;
                        }
                        iter.next();
                    }
                }
            }
            '{' | '(' => indent += 1,
            '}' | ')' => indent -= 1,
            _ => {}
        }
    }

    indent <= 0 && !in_string
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

            let parser = Parser::new(tokens);
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
