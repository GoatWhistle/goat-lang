module Goat.Core.Stdlib

open Goat.Core.Ast
open Goat.Core.Value
open Goat.Core.Environment
open Goat.Core.Builtins
open Goat.Core.Parser
open Goat.Core.Desugar
open Goat.Core.Eval

let stdlibSource = """
-- Option type
type Option = None | Some of x

-- Result type
type Result = Ok of x | Err of e

-- Option combinators
fun mapOption f opt =
  match opt with
  | None   => None
  | Some x => Some (f x)

fun bindOption opt f =
  match opt with
  | None   => None
  | Some x => f x

fun fromMaybe default opt =
  match opt with
  | None   => default
  | Some x => x

fun isNone opt =
  match opt with
  | None => true
  | _    => false

fun isSome opt =
  match opt with
  | Some _ => true
  | _      => false

-- Result combinators
fun mapOk f r =
  match r with
  | Ok x  => Ok (f x)
  | Err e => Err e

fun bindOk r f =
  match r with
  | Ok x  => f x
  | Err e => Err e

-- List functions (building on low-level builtins)
fun rec append xs ys =
  match xs with
  | []      => ys
  | x :: rest => x :: append rest ys

fun rec concat xss =
  match xss with
  | []        => []
  | xs :: rest => append xs (concat rest)

fun concatMap f xs = concat (map f xs)

fun rec zipWith f xs ys =
  match (xs, ys) with
  | ([], _)          => []
  | (_, [])          => []
  | (x :: xt, y :: yt) => f x y :: zipWith f xt yt

fun rec takeWhile pred xs =
  match xs with
  | []      => []
  | x :: rest => if pred x then x :: takeWhile pred rest else []

fun rec dropWhile pred xs =
  match xs with
  | []      => []
  | x :: rest => if pred x then dropWhile pred rest else x :: rest

fun rec any pred xs =
  match xs with
  | []      => false
  | x :: rest => if pred x then true else any pred rest

fun rec all pred xs =
  match xs with
  | []      => true
  | x :: rest => if pred x then all pred rest else false

fun rec elem x xs =
  match xs with
  | []      => false
  | y :: rest => if x == y then true else elem x rest

fun rec last xs =
  match xs with
  | [x]     => x
  | _ :: rest => last rest
  | []      => error "last: empty list"

fun rec init xs =
  match xs with
  | [_]     => []
  | x :: rest => x :: init rest
  | []      => error "init: empty list"

fun rec scan f acc xs =
  match xs with
  | []      => [acc]
  | x :: rest =>
      let acc' = f acc x in
      acc :: scan f acc' rest

fun replicate n x =
  if n <= 0 then []
  else x :: replicate (n - 1) x

fun unzipFst pairs =
  match pairs with
  | []             => []
  | (a, _) :: rest => a :: unzipFst rest

fun unzipSnd pairs =
  match pairs with
  | []             => []
  | (_, b) :: rest => b :: unzipSnd rest

fun unzip pairs = (unzipFst pairs, unzipSnd pairs)

fun rec lazyFilter pred xs =
  match xs with
  | []      => []
  | x :: rest =>
      if pred x then x :: lazy (lazyFilter pred (force rest))
      else lazyFilter pred (force rest)

fun rec iterate f x = x :: lazy (iterate f (f x))

fun rec repeat x = x :: lazy (repeat x)

fun rec natsFrom n = n :: lazy (natsFrom (n + 1))

let nats = natsFrom 0

-- range is already a Range expression in the parser, but also available as fn
fun range lo hi = [lo .. hi]

-- Functional utilities
fun id x = x

fun const x _ = x

fun flip f x y = f y x

fun compose f g x = f (g x)

fun rec fix f = f (lazy (fix f))

-- Boolean helpers
fun bool default' value b =
  if b then value else default'

-- String helpers
fun unwords ws =
  fold (\acc w -> acc ++ " " ++ w) "" ws

fun unlines ls =
  fold (\acc l -> acc ++ l ++ "\n") "" ls

fun splitLines s = lines s
"""

let loadStdlib (env: Env) : Result<Env, GoatError> =
    match parseProgram stdlibSource "<stdlib>" with
    | Error msg -> Error (TypeError ($"Stdlib parse error: {msg}", None))
    | Ok prog   ->
        let desugared = desugar prog
        let rec go env = function
            | [] -> Ok env
            | d :: rest ->
                match evalDecl env d with
                | Error e -> Error e
                | Ok env' -> go env' rest
        go env desugared.Decls

let fullEnv () : Result<Env, GoatError> =
    loadStdlib baseEnv
