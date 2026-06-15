module Goat.Cli.Program

open Goat.Core.Parser
open Goat.Core.Desugar
open Goat.Core.Eval
open Goat.Core.Stdlib
open Goat.Cli.Args

let runFile (path: string) =
    if not (System.IO.File.Exists path) then
        eprintfn "Error: file not found: %s" path
        1
    else
        let source = System.IO.File.ReadAllText path
        match parseProgram source path with
        | Error msg ->
            eprintfn "Parse error:\n%s" msg
            1
        | Ok prog ->
            let desugared = desugar prog
            match fullEnv () with
            | Error e ->
                eprintfn "Stdlib load error: %s" (formatError e)
                1
            | Ok env ->
                match runProgram env desugared with
                | Error e ->
                    eprintfn "Runtime error: %s" (formatError e)
                    1
                | Ok _ -> 0

let checkFile (path: string) =
    if not (System.IO.File.Exists path) then
        eprintfn "Error: file not found: %s" path
        1
    else
        let source = System.IO.File.ReadAllText path
        match parseProgram source path with
        | Error msg ->
            eprintfn "Parse error:\n%s" msg
            1
        | Ok prog ->
            let _ = desugar prog
            printfn "OK: %s" path
            0

[<EntryPoint>]
let main argv =
    match parse argv with
    | Run file   -> runFile file
    | Check file -> checkFile file
    | Repl ->
        printfn "Goat REPL — type :quit to exit, :help for help"
        let env = ref (match fullEnv () with Ok e -> e | Error _ -> Goat.Core.Builtins.baseEnv)
        let mutable running = true
        while running do
            printf "goat> "
            let line = System.Console.ReadLine() |> Option.ofObj |> Option.defaultValue ""
            if line.Trim() = ":quit" then
                running <- false
            elif line.Trim() = ":help" then
                printfn "  :quit  — exit\n  :help  — this message\n  <expr> — evaluate an expression"
            else
                match Goat.Core.Parser.parseExpr line with
                | Error msg -> eprintfn "Parse error: %s" msg
                | Ok expr ->
                    let ds = Goat.Core.Desugar.dsExpr' expr
                    match Goat.Core.Eval.eval !env ds with
                    | Error e -> eprintfn "Error: %s" (formatError e)
                    | Ok v    -> printfn "%s" (showValue v)
        0
    | Version ->
        printfn "Goat %s" version
        0
    | Help ->
        printfn "%s" usage
        0
