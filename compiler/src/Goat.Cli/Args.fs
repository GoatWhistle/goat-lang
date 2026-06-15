module Goat.Cli.Args

let version = "1.0.0"

type Command =
    | Run   of file: string
    | Check of file: string
    | Repl
    | Help
    | Version

let parse (argv: string[]) : Command =
    match argv with
    | [| "run";   file |]       -> Run file
    | [| "check"; file |]       -> Check file
    | [| "repl" |] | [||]       -> Repl
    | [| "--version" |] | [| "-V" |] -> Version
    | [| "--help" |] | [| "-h" |]    -> Help
    | _                         -> Help

let usage = """
Goat Language Interpreter

Usage:
  goat run   <file.goat>   Run a Goat program
  goat check <file.goat>   Parse and check syntax only
  goat repl                Start the interactive REPL
  goat                     Start the interactive REPL
  goat --help, -h          Show this help message
  goat --version, -V       Show the interpreter version
"""
