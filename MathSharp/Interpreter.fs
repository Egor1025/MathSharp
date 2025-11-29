module MathSharp.Interpreter

open System
open MathSharp.Ast

type FunctionClosure =
    | UserFunc of FuncDef * EvalEnv
    | BuiltinFunc of (Value list -> Value)

and Value =
    | IntVal of int
    | RealVal of float
    | BoolVal of bool
    | StringVal of string
    | ListVal of Value list
    | FunctionVal of FunctionClosure

and EvalEnv =
    { Vars: Map<string, Value>
      Funcs: Map<string, FuncDef>
      Builtins: Map<string, FunctionClosure> }

let emptyEnv =
    { Vars = Map.empty
      Funcs = Map.empty
      Builtins = Map.empty }

let withVar name value env =
    { env with Vars = env.Vars.Add(name, value) }

let withFunc (def: FuncDef) env =
    { env with Funcs = env.Funcs.Add(def.Name, def) }

let withBuiltin name closure env =
    { env with Builtins = env.Builtins.Add(name, closure) }

let tryFindVar name env =
    env.Vars |> Map.tryFind name

let tryFindFunc name env =
    env.Funcs |> Map.tryFind name

let tryFindBuiltin name env =
    env.Builtins |> Map.tryFind name

exception RuntimeError of string

let runtimeError msg = raise (RuntimeError msg)

let asBool v =
    match v with
    | BoolVal b -> b
    | _ -> runtimeError "Expected boolean"

let toFloat v =
    match v with
    | IntVal i -> float i
    | RealVal f -> f
    | _ -> runtimeError "Expected numeric value"

let numericAdd v1 v2 =
    match v1, v2 with
    | IntVal a, IntVal b -> IntVal(a + b)
    | _ -> RealVal(toFloat v1 + toFloat v2)

let numericSub v1 v2 =
    match v1, v2 with
    | IntVal a, IntVal b -> IntVal(a - b)
    | _ -> RealVal(toFloat v1 - toFloat v2)

let numericMul v1 v2 =
    match v1, v2 with
    | IntVal a, IntVal b -> IntVal(a * b)
    | _ -> RealVal(toFloat v1 * toFloat v2)

let numericDiv v1 v2 =
    RealVal(toFloat v1 / toFloat v2)

let numericPow v1 v2 =
    RealVal(Math.Pow(toFloat v1, toFloat v2))

let numericNeg v =
    match v with
    | IntVal i -> IntVal(-i)
    | RealVal f -> RealVal(-f)
    | _ -> runtimeError "Unary '-' expects numeric"

let absValue v =
    match v with
    | IntVal i -> IntVal(abs i)
    | RealVal f -> RealVal(abs f)
    | _ -> runtimeError "abs expects numeric"

let cmpInts op a b =
    match op with
    | CmpOp.Eq -> a = b
    | CmpOp.Neq -> a <> b
    | CmpOp.Lt -> a < b
    | CmpOp.Le -> a <= b
    | CmpOp.Gt -> a > b
    | CmpOp.Ge -> a >= b
    | _ -> runtimeError "Invalid integer comparison"

let cmpFloats op a b =
    match op with
    | CmpOp.Eq -> a = b
    | CmpOp.Neq -> a <> b
    | CmpOp.Lt -> a < b
    | CmpOp.Le -> a <= b
    | CmpOp.Gt -> a > b
    | CmpOp.Ge -> a >= b
    | _ -> runtimeError "Invalid float comparison"

let eqForIn v1 v2 =
    match v1, v2 with
    | IntVal a, IntVal b -> a = b
    | RealVal a, RealVal b -> a = b
    | BoolVal a, BoolVal b -> a = b
    | StringVal a, StringVal b -> a = b
    | _ -> runtimeError "Equality in 'in' is only supported for simple scalar values"

let compareValues op v1 v2 =
    match op with
    | CmpOp.In ->
        match v2 with
        | ListVal xs -> BoolVal(xs |> List.exists (fun x -> eqForIn v1 x))
        | _ -> runtimeError "Right operand of 'in' must be list"
    | CmpOp.NotIn ->
        match v2 with
        | ListVal xs -> BoolVal(not (xs |> List.exists (fun x -> eqForIn v1 x)))
        | _ -> runtimeError "Right operand of 'not in' must be list"
    | _ ->
        match v1, v2 with
        | IntVal a, IntVal b -> BoolVal(cmpInts op a b)
        | IntVal _, RealVal _ -> BoolVal(cmpFloats op (toFloat v1) (toFloat v2))
        | RealVal _, IntVal _ -> BoolVal(cmpFloats op (toFloat v1) (toFloat v2))
        | RealVal _, RealVal _ -> BoolVal(cmpFloats op (toFloat v1) (toFloat v2))
        | BoolVal a, BoolVal b ->
            match op with
            | CmpOp.Eq -> BoolVal(a = b)
            | CmpOp.Neq -> BoolVal(a <> b)
            | _ -> runtimeError "Invalid boolean comparison"
        | StringVal a, StringVal b ->
            match op with
            | CmpOp.Eq -> BoolVal(a = b)
            | CmpOp.Neq -> BoolVal(a <> b)
            | _ -> runtimeError "Invalid string comparison"
        | _ -> runtimeError "Incompatible values for comparison"

let rec evalExpr (env: EvalEnv) (expr: Expr) : Value =
    match expr with
    | IntLit i -> IntVal i
    | RealLit f -> RealVal f
    | BoolLit b -> BoolVal b
    | StringLit s -> StringVal s
    | Var name ->
        match tryFindVar name env with
        | Some v -> v
        | None ->
            match tryFindFunc name env with
            | Some def -> FunctionVal(UserFunc(def, env))
            | None ->
                match tryFindBuiltin name env with
                | Some c -> FunctionVal c
                | None -> runtimeError $"Unknown variable '{name}'"
    | Unary (UnaryOp.Neg, e) ->
        numericNeg (evalExpr env e)
    | Unary (UnaryOp.Not, e) ->
        BoolVal(not (asBool (evalExpr env e)))
    | Binary (l, op, r) ->
        let lv = evalExpr env l
        let rv = evalExpr env r
        match op with
        | BinOp.Add -> numericAdd lv rv
        | BinOp.Sub -> numericSub lv rv
        | BinOp.Mul -> numericMul lv rv
        | BinOp.Div -> numericDiv lv rv
        | BinOp.Pow -> numericPow lv rv
    | Compare (l, op, r) ->
        let lv = evalExpr env l
        let rv = evalExpr env r
        compareValues op lv rv
    | Logical (l, LogicalOp.And, r) ->
        let lv = evalExpr env l
        match lv with
        | BoolVal false -> BoolVal false
        | BoolVal true ->
            let rv = asBool (evalExpr env r)
            BoolVal rv
        | _ -> runtimeError "Logical 'and' expects booleans"
    | Logical (l, LogicalOp.Or, r) ->
        let lv = evalExpr env l
        match lv with
        | BoolVal true -> BoolVal true
        | BoolVal false ->
            let rv = asBool (evalExpr env r)
            BoolVal rv
        | _ -> runtimeError "Logical 'or' expects booleans"
    | Abs e ->
        absValue (evalExpr env e)
    | Call (name, args) ->
        let argVals = args |> List.map (evalExpr env)
        evalFunctionCall env name argVals
    | ListLiteral xs ->
        xs |> List.map (evalExpr env) |> ListVal
    | AnonFunc (ps, body) ->
        let def =
            { Name = "<anon>"
              Parameters = ps
              Body = FuncBody.Single body }
        FunctionVal(UserFunc(def, env))

and applyUserFunc (closureEnv: EvalEnv) (def: FuncDef) (args: Value list) : Value =
    let ps = def.Parameters
    if ps.Length <> args.Length then
        runtimeError $"Function '{def.Name}' expected {ps.Length} args but got {args.Length}"
    else
        let varsWithArgs =
            List.zip ps args
            |> List.fold
                (fun (m: Map<string, Value>) (p, v) -> m.Add(p.Name, v))
                closureEnv.Vars
        let callEnv = { closureEnv with Vars = varsWithArgs }
        match def.Body with
        | FuncBody.Single expr -> evalExpr callEnv expr
        | FuncBody.Piecewise pieces ->
            let rec evalPieces ps =
                match ps with
                | [] -> runtimeError $"No matching branch in function '{def.Name}'"
                | p :: rest ->
                    let condOk =
                        match p.Condition with
                        | Condition.Otherwise -> true
                        | Condition.ConditionExpr cexpr ->
                            match evalExpr callEnv cexpr with
                            | BoolVal b -> b
                            | _ -> runtimeError "Condition must be boolean"
                    if condOk then evalExpr callEnv p.Body
                    else evalPieces rest
            evalPieces pieces

and applyClosure (closure: FunctionClosure) (args: Value list) : Value =
    match closure with
    | BuiltinFunc f -> f args
    | UserFunc (def, closureEnv) -> applyUserFunc closureEnv def args

and evalFunctionCall (env: EvalEnv) (name: string) (args: Value list) : Value =
    match tryFindBuiltin name env with
    | Some closure -> applyClosure closure args
    | None ->
        match tryFindFunc name env with
        | Some def -> applyUserFunc env def args
        | None -> runtimeError $"Unknown function '{name}'"

let evalProgramWith (initialEnv: EvalEnv) (prog: Program) : EvalEnv =
    let envWithFuncs =
        prog
        |> List.fold (fun env def ->
            match def with
            | TopLevelDef.Function f -> withFunc f env
            | _ -> env) initialEnv
    let envWithConsts =
        prog
        |> List.fold (fun env def ->
            match def with
            | TopLevelDef.Const c ->
                let v = evalExpr env c.Value
                withVar c.Name v env
            | _ -> env) envWithFuncs
    envWithConsts

let evalProgram (prog: Program) : EvalEnv =
    evalProgramWith emptyEnv prog