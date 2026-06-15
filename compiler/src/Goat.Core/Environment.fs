module Goat.Core.Environment

open Goat.Core.Ast
open Goat.Core.Value

let empty : Env = Map.empty

let extend (name: Ident) (v: Value) (env: Env) : Env =
    Map.add name v env

let extendMany (bindings: (Ident * Value) list) (env: Env) : Env =
    List.fold (fun e (k, v) -> Map.add k v e) env bindings

let lookup (name: Ident) (span: Span option) (env: Env) : EvalResult =
    match Map.tryFind name env with
    | Some v -> Ok v
    | None   -> Error (NameError ($"Unbound variable '{name}'", span))
