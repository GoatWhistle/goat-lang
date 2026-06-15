module Goat.Repl.Repl

open Goat.Core.Ast
open Goat.Core.Value
open Goat.Core.Environment
open Goat.Core.Parser
open Goat.Core.Desugar
open Goat.Core.Eval
open Goat.Core.Stdlib

let banner = """
  ______  ___   __  ______
 / ___\ \/ / | / // ____/
/ (_ /\  /| |/ // /_
\___/ /_/ |___/ \__/

Goat REPL  — :help for commands
"""

type ReplState = { Env: Env; History: string list }

let printHelp () =
    printfn """
Commands:
  :help          Show this help
  :quit          Exit the REPL
  :load <file>   Load and evaluate a .goat file
  :env           List all defined names
  :reset         Reset environment to stdlib defaults
  <expr>         Evaluate an expression
  let x = ...   Define a top-level binding
  fun f x = ...  Define a function
"""

let tryEvalLine (state: ReplState) (line: string) : ReplState =
    let trimmed = line.Trim()

    let asDecl =
        if trimmed.StartsWith("let ") || trimmed.StartsWith("fun ") || trimmed.StartsWith("type ") then
            match parseProgram trimmed "<repl>" with
            | Ok prog ->
                let ds = desugar prog
                let rec go env = function
                    | [] -> Ok env
                    | d :: rest ->
                        match evalDecl env d with
                        | Error e -> Error e
                        | Ok env' -> go env' rest
                match go state.Env ds.Decls with
                | Ok env' ->

                    ds.Decls |> List.iter (fun d ->
                        match d with
                        | DLet (_, name, _, _, _) -> printfn "val %s = <defined>" name
                        | DType (name, _, _, _)   -> printfn "type %s = <defined>" name
                        | _ -> ())
                    Some { state with Env = env' }
                | Error e ->
                    eprintfn "Error: %s" (formatError e)
                    Some state
            | Error _ -> None
        else None

    match asDecl with
    | Some s -> s
    | None ->

        match parseExpr trimmed with
        | Error msg ->
            eprintfn "Parse error: %s" msg
            state
        | Ok expr ->
            let ds = dsExpr' expr
            match eval state.Env ds with
            | Error e ->
                eprintfn "Error: %s" (formatError e)
                state
            | Ok (VIO action) ->
                match action () with
                | IOOk v    ->
                    match v with
                    | VUnit -> ()
                    | _     -> printfn "%s" (showValue v)
                    state
                | IOError m ->
                    eprintfn "IOError: %s" m
                    state
            | Ok v ->
                printfn "%s" (showValue v)
                state

let run () =
    printfn "%s" banner
    match fullEnv () with
    | Error e ->
        eprintfn "Failed to load stdlib: %s" (formatError e)
    | Ok env ->
        let mutable state = { Env = env; History = [] }
        let mutable running = true
        while running do
            printf "goat> "
            match System.Console.ReadLine() |> Option.ofObj with
            | None ->
                running <- false
                printfn "Bye!"
            | Some line ->
                let trimmed = line.Trim()
                if trimmed = ":quit" || trimmed = ":q" then
                    running <- false
                    printfn "Bye!"
                elif trimmed = ":help" || trimmed = ":h" then
                    printHelp ()
                elif trimmed = ":env" then
                    state.Env |> Map.iter (fun k _ -> printfn "  %s" k)
                elif trimmed = ":reset" then
                    match fullEnv () with
                    | Ok env' ->
                        state <- { state with Env = env' }
                        printfn "Environment reset."
                    | Error e ->
                        eprintfn "Reset failed: %s" (formatError e)
                elif trimmed.StartsWith(":load ") then
                    let path = trimmed.Substring(6).Trim()
                    if not (System.IO.File.Exists path) then
                        eprintfn "File not found: %s" path
                    else
                        let src = System.IO.File.ReadAllText path
                        match parseProgram src path with
                        | Error msg -> eprintfn "Parse error: %s" msg
                        | Ok prog ->
                            let ds = desugar prog
                            let rec go env = function
                                | [] -> Ok env
                                | d :: rest ->
                                    match evalDecl env d with
                                    | Error e -> Error e
                                    | Ok env' -> go env' rest
                            match go state.Env ds.Decls with
                            | Ok env' ->
                                state <- { state with Env = env' }
                                printfn "Loaded: %s" path
                            | Error e ->
                                eprintfn "Error: %s" (formatError e)
                elif trimmed = "" then
                    ()
                else
                    state <- tryEvalLine state line
