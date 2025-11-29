module MathSharp.Program

open FParsec
open MathSharp.Parser

let runParse input =
    match run pProgram input with
    | Success (prog, _, _) ->
        printfn "%A" prog
    | Failure (err, _, _) ->
        printfn "%s" err

[<EntryPoint>]    
let main argv =
    let path = "../../../test.ms"
    let src = System.IO.File.ReadAllText path
    runParse src
    0