module MathSharp.Builtins

open System
open System.IO
open MathSharp.Interpreter

let rec valueToString v =
    match v with
    | IntVal i -> string i
    | RealVal f -> string f
    | BoolVal b -> string b
    | StringVal s -> s
    | ListVal xs ->
        let inner = xs |> List.map valueToString |> String.concat ", "
        $"[{inner}]"
    | FunctionVal _ -> "<function>"
    | UnitVal -> ""

let builtinSqrt =
    BuiltinFunc(fun args ->
        match args with
        | [x] -> RealVal(Math.Sqrt(toFloat x))
        | _ -> runtimeError "sqrt expects 1 argument")

let builtinExp =
    BuiltinFunc(fun args ->
        match args with
        | [x] -> RealVal(Math.Exp(toFloat x))
        | _ -> runtimeError "exp expects 1 argument")

let builtinPrintln =
    BuiltinFunc(fun args ->
        args
        |> List.map valueToString
        |> String.concat " "
        |> Console.WriteLine
        UnitVal)

let builtinPrint =
    BuiltinFunc(fun args ->
        args
        |> List.map valueToString
        |> String.concat " "
        |> Console.Write
        UnitVal)

let builtinReadLine =
    BuiltinFunc(fun args ->
        match args with
        | [] ->
            let line = Console.ReadLine()
            StringVal line
        | _ -> runtimeError "readLine expects 0 arguments")

let builtinReadInt =
    BuiltinFunc(fun args ->
        match args with
        | [] ->
            let line = Console.ReadLine()
            match Int32.TryParse line with
            | true, v -> IntVal v
            | _ -> runtimeError $"readInt: cannot parse '{line}'"
        | _ -> runtimeError "readInt expects 0 arguments")

let builtinReadReal =
    BuiltinFunc(fun args ->
        match args with
        | [] ->
            let line = Console.ReadLine()
            match Double.TryParse line with
            | true, v -> RealVal v
            | _ -> runtimeError $"readReal: cannot parse '{line}'"
        | _ -> runtimeError "readReal expects 0 arguments")

let builtinReadFile =
    BuiltinFunc(fun args ->
        match args with
        | [StringVal path] ->
            StringVal(File.ReadAllText path)
        | _ -> runtimeError "readFile expects (string)")

let builtinWriteFile =
    BuiltinFunc(fun args ->
        match args with
        | [StringVal path; StringVal content] ->
            File.WriteAllText(path, content)
            BoolVal true
        | _ -> runtimeError "writeFile expects (string, string)")

let builtinHead =
    BuiltinFunc(fun args ->
        match args with
        | [ListVal xs] ->
            match xs with
            | h :: _ -> h
            | [] -> runtimeError "head of empty list"
        | _ -> runtimeError "head expects (list)")

let builtinTail =
    BuiltinFunc(fun args ->
        match args with
        | [ListVal xs] ->
            match xs with
            | _ :: t -> ListVal t
            | [] -> runtimeError "tail of empty list"
        | _ -> runtimeError "tail expects (list)")

let builtinIsEmpty =
    BuiltinFunc(fun args ->
        match args with
        | [ListVal xs] -> BoolVal xs.IsEmpty
        | _ -> runtimeError "isEmpty expects (list)")

let builtinCons =
    BuiltinFunc(fun args ->
        match args with
        | [x; ListVal xs] -> ListVal(x :: xs)
        | _ -> runtimeError "cons expects (value, list)")

let builtinMap =
    BuiltinFunc(fun args ->
        match args with
        | [FunctionVal f; ListVal xs] ->
            xs
            |> List.map (fun v -> applyClosure f [v])
            |> ListVal
        | _ -> runtimeError "map expects (function, list)")

let builtinFold =
    BuiltinFunc(fun args ->
        match args with
        | [FunctionVal f; acc; ListVal xs] ->
            let res =
                xs
                |> List.fold (fun state x -> applyClosure f [state; x]) acc
            res
        | _ -> runtimeError "fold expects (function, acc, list)")

let addBuiltins env =
    env
    |> withVar "PI" (RealVal Math.PI)
    |> withVar "E" (RealVal Math.E)
    |> withBuiltin "sqrt" builtinSqrt
    |> withBuiltin "exp" builtinExp
    |> withBuiltin "println" builtinPrintln
    |> withBuiltin "print" builtinPrint
    |> withBuiltin "readLine" builtinReadLine
    |> withBuiltin "readInt" builtinReadInt
    |> withBuiltin "readReal" builtinReadReal
    |> withBuiltin "readFile" builtinReadFile
    |> withBuiltin "writeFile" builtinWriteFile
    |> withBuiltin "head" builtinHead
    |> withBuiltin "tail" builtinTail
    |> withBuiltin "isEmpty" builtinIsEmpty
    |> withBuiltin "cons" builtinCons
    |> withBuiltin "map" builtinMap
    |> withBuiltin "fold" builtinFold