module MathSharp.Program

open System
open FParsec
open MathSharp.Parser
open MathSharp.Interpreter
open MathSharp.Builtins

let runSource src =
    match run pProgram src with
    | Success (prog, _, _) ->
        let env0 = emptyEnv |> addBuiltins
        let env = evalProgramWith env0 prog
        match env.Funcs |> Map.tryFind "main" with
        | None ->
            Console.WriteLine "No 'main' function defined"
        | Some def ->
            let result = applyUserFunc env def []
            Console.WriteLine(valueToString result)
    | Failure (err, _, _) ->
        Console.WriteLine err

[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        let path = $"../../../{argv[0]}"
        let src = IO.File.ReadAllText path
        runSource src
        0
    else
        Console.WriteLine "Usage: dotnet run -- <file.ms>"
        1