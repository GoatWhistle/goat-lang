module Goat.Core.Builtins

open Goat.Core.Ast
open Goat.Core.Value
open Goat.Core.Environment
open Goat.Core.Eval

let typeErr msg = Error (TypeError (msg, None))

let builtin1 name f =
    (name, VBuiltin (name, f))

let builtin2 name f =
    (name, VBuiltin (name, fun a ->
        Ok (VBuiltin (name + "'", fun b -> f a b))))

let builtin3 name f =
    (name, VBuiltin (name, fun a ->
        Ok (VBuiltin (name + "'", fun b ->
            Ok (VBuiltin (name + "''", fun c -> f a b c))))))

let rec toFsList (v: Value) : Result<Value list, GoatError> =
    match v with
    | VNil -> Ok []
    | VCons (h, tRef) ->
        match forceThunk tRef with
        | Error e -> Error e
        | Ok t    ->
            match toFsList t with
            | Error e  -> Error e
            | Ok rest  -> Ok (h :: rest)
    | VThunk tref ->
        match forceThunk tref with
        | Error e -> Error e
        | Ok v'   -> toFsList v'
    | _ -> typeErr $"Expected list, got {typeNameOf v}"

let fromFsList (lst: Value list) : Value =
    List.foldBack (fun v tl -> VCons (v, ThunkRef (ref (Evaluated tl)))) lst VNil

let ioPrint =
    builtin1 "print" (fun v ->
        Ok (VIO (fun () ->
            printf "%s" (showValue v)
            IOOk VUnit)))

let ioPrintln =
    builtin1 "println" (fun v ->
        Ok (VIO (fun () ->
            printfn "%s" (showValue v)
            IOOk VUnit)))

let ioInput =
    ("input", VIO (fun () ->
        let line =
            System.Console.ReadLine()
            |> Option.ofObj
            |> Option.defaultValue ""
        IOOk (VString line)))

let ioReadFile =
    builtin1 "readFile" (fun v ->
        match v with
        | VString path ->
            Ok (VIO (fun () ->
                try IOOk (VString (System.IO.File.ReadAllText path))
                with ex -> IOError ex.Message))
        | _ -> typeErr "readFile expects String")

let ioWriteFile =
    builtin2 "writeFile" (fun path content ->
        match path, content with
        | VString p, VString c ->
            Ok (VIO (fun () ->
                try System.IO.File.WriteAllText(p, c); IOOk VUnit
                with ex -> IOError ex.Message))
        | _ -> typeErr "writeFile expects String String")

let ioAppendFile =
    builtin2 "appendFile" (fun path content ->
        match path, content with
        | VString p, VString c ->
            Ok (VIO (fun () ->
                try System.IO.File.AppendAllText(p, c); IOOk VUnit
                with ex -> IOError ex.Message))
        | _ -> typeErr "appendFile expects String String")

let ioFileExists =
    builtin1 "fileExists" (fun v ->
        match v with
        | VString path ->
            Ok (VIO (fun () ->
                try IOOk (VBool (System.IO.File.Exists path))
                with ex -> IOError ex.Message))
        | _ -> typeErr "fileExists expects String")

let ioGetArgs =
    ("getArgs", VIO (fun () ->
        let args = System.Environment.GetCommandLineArgs() |> Array.toList |> List.map VString
        IOOk (fromFsList args)))

let ioExit =
    builtin1 "exit" (fun v ->
        match v with
        | VInt code ->
            Ok (VIO (fun () -> System.Environment.Exit(int code); IOOk VUnit))
        | _ -> typeErr "exit expects Int")

let strLength =
    builtin1 "strLength" (fun v ->
        match v with
        | VString s -> Ok (VInt (int64 s.Length))
        | _ -> typeErr "strLength expects String")

let strChars =
    builtin1 "chars" (fun v ->
        match v with
        | VString s ->
            let chars = s |> Seq.map (fun c -> VString (string c)) |> Seq.toList
            Ok (fromFsList chars)
        | _ -> typeErr "chars expects String")

let strWords =
    builtin1 "words" (fun v ->
        match v with
        | VString s ->
            Ok (fromFsList (s.Split([|' '; '\t'|], System.StringSplitOptions.RemoveEmptyEntries)
                            |> Array.map VString |> Array.toList))
        | _ -> typeErr "words expects String")

let strLines =
    builtin1 "lines" (fun v ->
        match v with
        | VString s ->
            Ok (fromFsList (s.Split([|'\n'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries)
                            |> Array.map VString |> Array.toList))
        | _ -> typeErr "lines expects String")

let strTrim =
    builtin1 "trim" (fun v ->
        match v with
        | VString s -> Ok (VString (s.Trim()))
        | _ -> typeErr "trim expects String")

let strToUpper =
    builtin1 "toUpper" (fun v ->
        match v with
        | VString s -> Ok (VString (s.ToUpper()))
        | _ -> typeErr "toUpper expects String")

let strToLower =
    builtin1 "toLower" (fun v ->
        match v with
        | VString s -> Ok (VString (s.ToLower()))
        | _ -> typeErr "toLower expects String")

let strContains =
    builtin2 "contains" (fun haystack needle ->
        match haystack, needle with
        | VString h, VString n -> Ok (VBool (h.Contains n))
        | _ -> typeErr "contains expects String String")

let strReplace =
    builtin3 "replace" (fun s old_ new_ ->
        match s, old_, new_ with
        | VString s, VString o, VString n -> Ok (VString (s.Replace(o, n)))
        | _ -> typeErr "replace expects String String String")

let strShow =
    builtin1 "show" (fun v -> Ok (VString (showValue v)))

let errorBuiltin =
    builtin1 "error" (fun v ->
        match v with
        | VString msg -> Error (UserError msg)
        | _ -> Error (UserError (showValue v)))

let strParseInt =
    builtin1 "parseInt" (fun v ->
        match v with
        | VString s ->
            match System.Int64.TryParse s with
            | true, n  -> Ok (VCtor ("Some", [VInt n]))
            | false, _ -> Ok (VCtor ("None", []))
        | _ -> typeErr "parseInt expects String")

let strParseFloat =
    builtin1 "parseFloat" (fun v ->
        match v with
        | VString s ->
            match System.Double.TryParse(s, System.Globalization.CultureInfo.InvariantCulture) with
            | true, f  -> Ok (VCtor ("Some", [VFloat f]))
            | false, _ -> Ok (VCtor ("None", []))
        | _ -> typeErr "parseFloat expects String")

let mathBuiltin1 name (f: float -> float) =
    builtin1 name (fun v ->
        match v with
        | VFloat x -> Ok (VFloat (f x))
        | VInt x   -> Ok (VFloat (f (float x)))
        | _ -> typeErr $"{name} expects a number")

let mathSqrt   = mathBuiltin1 "sqrt"   sqrt
let mathFloor  = mathBuiltin1 "floor"  floor  |> fun (n, v) -> (n, v)
let mathCeil   = mathBuiltin1 "ceil"   ceil
let mathRound  = mathBuiltin1 "round"  System.Math.Round
let mathSin    = mathBuiltin1 "sin"    sin
let mathCos    = mathBuiltin1 "cos"    cos
let mathTan    = mathBuiltin1 "tan"    tan
let mathExp    = mathBuiltin1 "exp"    exp
let mathLog    = mathBuiltin1 "log"    log
let mathLog2   = mathBuiltin1 "log2"   (fun x -> System.Math.Log(x, 2.0))
let mathLog10  = mathBuiltin1 "log10"  (fun x -> System.Math.Log10 x)
let mathAbs    = builtin1 "abs" (fun v ->
    match v with
    | VInt n   -> Ok (VInt (abs n))
    | VFloat f -> Ok (VFloat (abs f))
    | _ -> typeErr "abs expects a number")

let mathMax =
    builtin2 "max" (fun a b ->
        match a, b with
        | VInt x,   VInt y   -> Ok (if x > y then VInt x else VInt y)
        | VFloat x, VFloat y -> Ok (if x > y then VFloat x else VFloat y)
        | _ -> typeErr "max expects two numbers of same type")

let mathMin =
    builtin2 "min" (fun a b ->
        match a, b with
        | VInt x,   VInt y   -> Ok (if x < y then VInt x else VInt y)
        | VFloat x, VFloat y -> Ok (if x < y then VFloat x else VFloat y)
        | _ -> typeErr "min expects two numbers of same type")

let mathToFloat =
    builtin1 "toFloat" (fun v ->
        match v with
        | VInt n -> Ok (VFloat (float n))
        | VFloat f -> Ok (VFloat f)
        | _ -> typeErr "toFloat expects a number")

let mathToInt =
    builtin1 "toInt" (fun v ->
        match v with
        | VFloat f -> Ok (VInt (int64 f))
        | VInt n   -> Ok (VInt n)
        | _ -> typeErr "toInt expects a number")

let listHead =
    builtin1 "head" (fun v ->
        match v with
        | VCons (h, _) -> Ok h
        | VNil -> typeErr "head: empty list"
        | _ -> typeErr "head expects a list")

let listTail =
    builtin1 "tail" (fun v ->
        match v with
        | VCons (_, tRef) -> forceThunk tRef
        | VNil -> typeErr "tail: empty list"
        | _ -> typeErr "tail expects a list")

let listIsNil =
    builtin1 "null" (fun v ->
        match v with
        | VNil -> Ok (VBool true)
        | VCons _ -> Ok (VBool false)
        | _ -> typeErr "null expects a list")

let applyFn (fn: Value) (arg: Value) : EvalResult =
    applyValue fn arg noSpan

let listMap =
    builtin2 "map" (fun f lst ->
        match toFsList lst with
        | Error e -> Error e
        | Ok items ->
            let rec go acc = function
                | [] -> Ok (fromFsList (List.rev acc))
                | x :: rest ->
                    match applyFn f x with
                    | Error e -> Error e
                    | Ok v    -> go (v :: acc) rest
            go [] items)

let listFilter =
    builtin2 "filter" (fun pred lst ->
        match toFsList lst with
        | Error e -> Error e
        | Ok items ->
            let rec go acc = function
                | [] -> Ok (fromFsList (List.rev acc))
                | x :: rest ->
                    match applyFn pred x with
                    | Ok (VBool true)  -> go (x :: acc) rest
                    | Ok (VBool false) -> go acc rest
                    | Ok _  -> typeErr "filter predicate must return Bool"
                    | Error e -> Error e
            go [] items)

let listFold =
    builtin3 "fold" (fun f init lst ->
        match toFsList lst with
        | Error e -> Error e
        | Ok items ->
            let rec go acc = function
                | [] -> Ok acc
                | x :: rest ->
                    match applyFn f acc with
                    | Error e -> Error e
                    | Ok f'   ->
                        match applyFn f' x with
                        | Error e -> Error e
                        | Ok acc' -> go acc' rest
            go init items)

let listFoldr =
    builtin3 "foldr" (fun f init lst ->
        match toFsList lst with
        | Error e -> Error e
        | Ok items ->
            let rec go = function
                | [] -> Ok init
                | x :: rest ->
                    match go rest with
                    | Error e -> Error e
                    | Ok acc  ->
                        match applyFn f x with
                        | Error e -> Error e
                        | Ok f'   -> applyFn f' acc
            go items)

let listLength =
    builtin1 "length" (fun lst ->
        match toFsList lst with
        | Error e -> Error e
        | Ok items -> Ok (VInt (int64 (List.length items))))

let listTake =
    builtin2 "take" (fun n lst ->
        match n with
        | VInt count ->
            let rec go acc remaining v =
                if remaining <= 0L then Ok (fromFsList (List.rev acc))
                else
                    match v with
                    | VNil -> Ok (fromFsList (List.rev acc))
                    | VCons (h, tRef) ->
                        match forceThunk tRef with
                        | Error e -> Error e
                        | Ok t    -> go (h :: acc) (remaining - 1L) t
                    | VThunk tref ->
                        match forceThunk tref with
                        | Error e -> Error e
                        | Ok v'   -> go acc remaining v'
                    | _ -> typeErr "take expects a list"
            go [] count lst
        | _ -> typeErr "take expects Int")

let listDrop =
    builtin2 "drop" (fun n lst ->
        match n with
        | VInt count ->
            let rec go remaining v =
                if remaining <= 0L then Ok v
                else
                    match v with
                    | VNil -> Ok VNil
                    | VCons (_, tRef) ->
                        match forceThunk tRef with
                        | Error e -> Error e
                        | Ok t    -> go (remaining - 1L) t
                    | VThunk tref ->
                        match forceThunk tref with
                        | Error e -> Error e
                        | Ok v'   -> go remaining v'
                    | _ -> typeErr "drop expects a list"
            go count lst
        | _ -> typeErr "drop expects Int")

let listReverse =
    builtin1 "reverse" (fun lst ->
        match toFsList lst with
        | Error e -> Error e
        | Ok items -> Ok (fromFsList (List.rev items)))

let listNth =
    builtin2 "nth" (fun n lst ->
        match n with
        | VInt idx ->
            let rec go i v =
                match v with
                | VNil -> typeErr $"nth: index {n} out of bounds"
                | VCons (h, tRef) ->
                    if i = 0L then Ok h
                    else
                        match forceThunk tRef with
                        | Error e -> Error e
                        | Ok t    -> go (i - 1L) t
                | _ -> typeErr "nth expects a list"
            go idx lst
        | _ -> typeErr "nth expects Int")

let listZip =
    builtin2 "zip" (fun la lb ->
        match toFsList la, toFsList lb with
        | Ok a, Ok b ->
            let n = min (List.length a) (List.length b)
            let pairs = List.map2 (fun x y -> VTuple [x; y]) (List.truncate n a) (List.truncate n b)
            Ok (fromFsList pairs)
        | Error e, _ | _, Error e -> Error e)

let listSort =
    builtin1 "sort" (fun lst ->
        match toFsList lst with
        | Error e -> Error e
        | Ok items ->
            let sorted =
                items |> List.sortWith (fun a b ->
                    match a, b with
                    | VInt x,    VInt y    -> compare x y
                    | VFloat x,  VFloat y  -> compare x y
                    | VString x, VString y -> compare x y
                    | _ -> 0)
            Ok (fromFsList sorted))

let allBuiltins : (Ident * Value) list =
    [
      ioPrint; ioPrintln; ioInput; ioReadFile; ioWriteFile
      ioAppendFile; ioFileExists; ioGetArgs; ioExit

      strLength; strChars; strWords; strLines; strTrim
      strToUpper; strToLower; strContains; strReplace
      strShow; strParseInt; strParseFloat; errorBuiltin

      mathSqrt; mathFloor; mathCeil; mathRound
      mathSin; mathCos; mathTan; mathExp
      mathLog; mathLog2; mathLog10
      mathAbs; mathMax; mathMin; mathToFloat; mathToInt
      ("pi", VFloat System.Math.PI)
      ("e",  VFloat System.Math.E)

      listHead; listTail; listIsNil; listMap; listFilter
      listFold; listFoldr; listLength; listTake; listDrop
      listReverse; listNth; listZip; listSort ]

let baseEnv : Env =
    extendMany allBuiltins empty
