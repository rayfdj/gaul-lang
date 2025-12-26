use anyhow::Result;
use clap::Parser as ClapParser;
use gaul_lang::interpreter::Interpreter;
use gaul_lang::interpreter::environment::Environment;
use gaul_lang::interpreter::value::Value;
use gaul_lang::keywords::load_keywords;
use gaul_lang::parser::Parser;
use gaul_lang::resolver::Resolver;
use gaul_lang::scanner::Scanner;
use gaul_lang::scanner::token::TokenType;
use rustyline::DefaultEditor;
use std::collections::HashMap;
use std::fs;

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
    let mut resolver = Resolver::new();
    let mut interpreter = Interpreter::new(Environment::new());

    match cli.script {
        None => run_prompt(&keywords, &mut resolver, &mut interpreter)?,
        Some(path) => run_file(&path, &keywords, &mut resolver, &mut interpreter)?,
    }

    Ok(())
}

fn run_prompt(
    keywords: &HashMap<String, TokenType>,
    resolver: &mut Resolver,
    interpreter: &mut Interpreter,
) -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    let mut buffer = String::new();

    let history_path = dirs::home_dir().map(|p| p.join(".gaul_history"));
    if let Some(ref path) = history_path {
        let _ = rl.load_history(path);
    }

    loop {
        let prompt = if buffer.is_empty() { "> " } else { "| " };

        match rl.readline(prompt) {
            Ok(line) => {
                buffer.push_str(&line);
                buffer.push('\n');

                if is_complete(&buffer) {
                    if !buffer.trim().is_empty() {
                        let _ = rl.add_history_entry(buffer.trim());
                        if let Err(e) = run(&buffer, keywords, resolver, interpreter) {
                            eprintln!("Execution error: {}", e);
                        }
                    }
                    buffer.clear();
                }
            }
            Err(rustyline::error::ReadlineError::Eof) => break,
            Err(rustyline::error::ReadlineError::Interrupted) => {
                buffer.clear();
                println!("^C");
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                break;
            }
        }
    }

    if let Some(ref path) = history_path {
        let _ = rl.save_history(path);
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

fn run_file(
    path: &str,
    keywords: &HashMap<String, TokenType>,
    resolver: &mut Resolver,
    interpreter: &mut Interpreter,
) -> Result<()> {
    let contents = fs::read_to_string(path)?;
    run(&contents, keywords, resolver, interpreter)?;
    Ok(())
}

fn run(
    source: &str,
    keywords: &HashMap<String, TokenType>,
    resolver: &mut Resolver,
    interpreter: &mut Interpreter,
) -> Result<()> {
    let scanner = Scanner::new(source, keywords);

    match scanner.scan_tokens() {
        Ok(tokens) => {
            // tokens.iter().for_each(|token| println!("{:?}", token));

            let parser = Parser::new(tokens);
            match parser.parse() {
                Ok(mut program) => {
                    match resolver.resolve(&mut program) {
                        Ok(()) => {}
                        Err(e) => {
                            eprintln!("Resolver error: {:?}", e);
                            return Ok(());
                        }
                    }

                    // println!("Parsed: {:?}", program);
                    match interpreter.interpret(program) {
                        Ok(Value::Null) => {}
                        Ok(value) => println!("Value: {:?}", value),
                        Err(e) => eprintln!("Runtime error: {:?}", e),
                    }
                }
                Err(e) => eprintln!("Parse error: {:?}", e),
            }
        }
        Err(errors) => {
            errors.iter().for_each(|e| eprintln!("{}", e));
        }
    }

    Ok(())
}
