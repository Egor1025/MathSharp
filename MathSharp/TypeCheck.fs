module MathSharp.TypeCheck

open MathSharp.Ast

type Ty =
    | TN
    | TZ
    | TQ
    | TR
    | TBool
    | TString
    | TList of Ty
    | TCustom of string
    | TUnknown

type FuncType =
    { ParamTypes: Ty list
      ReturnType: Ty option }

type TypeEnv =
    { Vars: Map<string, Ty>
      Funcs: Map<string, FuncType> }

exception TypeError of string

let typeError msg = raise (TypeError msg)

let rec tyFromTypeExpr (te: TypeExpr) : Ty =
    match te with
    | SimpleType "N" -> TN
    | SimpleType "Z" -> TZ
    | SimpleType "Q" -> TQ
    | SimpleType "R" -> TR
    | SimpleType "Bool" -> TBool
    | SimpleType "String" -> TString
    | SimpleType name -> TCustom name
    | SetType [SimpleType "0"; SimpleType "1"] -> TBool
    | SetType _ -> TUnknown
    | ArrowType _ -> TUnknown

let isNumeric =
    function
    | TN
    | TZ
    | TQ
    | TR -> true
    | _ -> false

let numericRank =
    function
    | TN -> 1
    | TZ -> 2
    | TQ -> 3
    | TR -> 4
    | _ -> failwith "not numeric"

let promoteNumeric t1 t2 =
    if not (isNumeric t1 && isNumeric t2) then
        typeError "Numeric operation on non-numeric types"
    else
        let r1 = numericRank t1
        let r2 = numericRank t2
        if r1 >= r2 then t1 else t2

let rec canAssign (actual: Ty) (expected: Ty) : bool =
    match expected with
    | TUnknown -> true
    | _ when actual = expected -> true
    | _ ->
        match actual, expected with
        | a, e when isNumeric a && isNumeric e ->
            numericRank a <= numericRank e
        | TList a, TList e -> canAssign a e
        | _ -> false

let canAssignOrError actual expected msg =
    if not (canAssign actual expected) then
        typeError msg

let emptyEnv =
    { Vars = Map.empty
      Funcs = Map.empty }

let withVar name ty env =
    { env with Vars = env.Vars.Add(name, ty) }

let withFuncType name fty env =
    { env with Funcs = env.Funcs.Add(name, fty) }

let tryFindVar name env =
    env.Vars |> Map.tryFind name

let tryFindFuncType name env =
    env.Funcs |> Map.tryFind name

let buildFuncTypeEnv (prog: Program) : Map<string, FuncType> =
    let initial =
        prog
        |> List.fold
            (fun (acc: Map<string, FuncType>) def ->
                match def with
                | Signature sig' ->
                    let rec splitArrow t =
                        match t with
                        | ArrowType (a, b) -> Some(a, b)
                        | _ -> None
                    match splitArrow sig'.Type with
                    | Some (argT, retT) ->
                        let argTy = tyFromTypeExpr argT
                        let retTy = tyFromTypeExpr retT
                        acc.Add(sig'.Name, { ParamTypes = [ argTy ]; ReturnType = Some retTy })
                    | None ->
                        acc.Add(sig'.Name, { ParamTypes = []; ReturnType = Some(tyFromTypeExpr sig'.Type) })
                | _ -> acc)
            Map.empty

    prog
    |> List.fold
        (fun (acc: Map<string, FuncType>) def ->
            match def with
            | Function f ->
                let existing =
                    match acc |> Map.tryFind f.Name with
                    | Some x -> x
                    | None ->
                        { ParamTypes = []
                          ReturnType = None }
                let paramTypesFromAnn =
                    f.Parameters
                    |> List.map (fun p -> p.Type |> Option.map tyFromTypeExpr |> Option.defaultValue TUnknown)
                let paramTypes =
                    if List.exists ((<>) TUnknown) paramTypesFromAnn then
                        paramTypesFromAnn
                    else
                        existing.ParamTypes
                let merged =
                    { existing with
                        ParamTypes = paramTypes }
                acc.Add(f.Name, merged)
            | _ -> acc)
        initial

let rec typeOfExpr (env: TypeEnv) (expr: Expr) : Ty =
    match expr with
    | IntLit _ -> TN
    | RealLit _ -> TR
    | BoolLit _ -> TBool
    | StringLit _ -> TString
    | Var name ->
        match tryFindVar name env with
        | Some t -> t
        | None ->
            match tryFindFuncType name env with
            | Some _ -> TUnknown
            | None -> TUnknown
    | Unary (UnaryOp.Neg, e) ->
        let t = typeOfExpr env e
        if not (isNumeric t || t = TUnknown) then
            typeError "Unary '-' expects numeric"
        else TZ
    | Unary (UnaryOp.Not, e) ->
        let t = typeOfExpr env e
        if t <> TBool && t <> TUnknown then
            typeError "Unary 'not' expects bool"
        TBool
    | Binary (l, op, r) ->
        let tl = typeOfExpr env l
        let tr = typeOfExpr env r
        match op with
        | BinOp.Add
        | BinOp.Sub
        | BinOp.Mul
        | BinOp.Div
        | BinOp.Pow ->
            if tl = TUnknown || tr = TUnknown then
                TUnknown
            else
                promoteNumeric tl tr
    | Compare (l, op, r) ->
        let tl = typeOfExpr env l
        let tr = typeOfExpr env r
        match op with
        | CmpOp.In
        | CmpOp.NotIn ->
            match tr with
            | TList elemTy ->
                if tl <> TUnknown && elemTy <> TUnknown && not (canAssign tl elemTy) then
                    typeError "Type mismatch in 'in'/'not in'"
            | TUnknown ->
                ()  // не знаем, что справа, считаем ок
            | _ ->
                typeError "Right operand of 'in' must be a list"
            TBool
        | _ ->
            if tl = TUnknown || tr = TUnknown then
                TBool
            else
                if isNumeric tl && isNumeric tr then
                    ignore (promoteNumeric tl tr)
                    TBool
                else if tl = tr then
                    TBool
                else
                    typeError "Incompatible types in comparison"
    | Logical (l, _, r) ->
        let tl = typeOfExpr env l
        let tr = typeOfExpr env r
        if (tl <> TBool && tl <> TUnknown) || (tr <> TBool && tr <> TUnknown) then
            typeError "Logical operations expect booleans"
        TBool
    | Abs e ->
        let t = typeOfExpr env e
        if t <> TUnknown && not (isNumeric t) then
            typeError "abs expects numeric"
        t
    | Call (name, args) ->
        let argTypes = args |> List.map (typeOfExpr env)
        match tryFindFuncType name env with
        | Some fty ->
            if fty.ParamTypes.Length > 0 && fty.ParamTypes.Length <> argTypes.Length then
                typeError $"Function '{name}' expected {fty.ParamTypes.Length} args but got {argTypes.Length}"
            let expected =
                if fty.ParamTypes.Length = argTypes.Length && fty.ParamTypes.Length > 0 then
                    fty.ParamTypes
                else
                    List.replicate argTypes.Length TUnknown
            List.zip argTypes expected
            |> List.iter (fun (a, e) ->
                if e <> TUnknown && a <> TUnknown && not (canAssign a e) then
                    typeError $"Argument type mismatch in call to '{name}'")
            defaultArg fty.ReturnType TUnknown
        | None ->
            TUnknown
    | ListLiteral xs ->
        match xs with
        | [] -> TList TUnknown
        | x :: rest ->
            let t0 = typeOfExpr env x
            let tElem =
                rest
                |> List.fold
                    (fun acc e ->
                        let t = typeOfExpr env e
                        match acc, t with
                        | TUnknown, t -> t
                        | t, TUnknown -> t
                        | a, b when a = b -> a
                        | a, b when isNumeric a && isNumeric b -> promoteNumeric a b
                        | _ -> typeError "Heterogeneous list elements are not allowed")
                    t0
            TList tElem
    | AnonFunc (ps, body) ->
        let paramTypes =
            ps
            |> List.map (fun p -> p.Type |> Option.map tyFromTypeExpr |> Option.defaultValue TUnknown)
        let localVars =
            (env.Vars, List.zip ps paramTypes)
            ||> List.fold (fun (m: Map<string, Ty>) (p, t) -> m.Add(p.Name, t))
        let bodyTy = typeOfExpr { env with Vars = localVars } body
        ignore bodyTy
        TUnknown

let checkCondition (env: TypeEnv) (cond: Condition) =
    match cond with
    | Otherwise -> ()
    | ConditionExpr e ->
        let t = typeOfExpr env e
        if t <> TBool && t <> TUnknown then
            typeError "Condition must have type Bool"

let checkFuncDef (globalFuncs: Map<string, FuncType>) (sigMap: Map<string, FuncType>) (f: FuncDef) =
    let fty =
        match sigMap |> Map.tryFind f.Name with
        | Some ft -> ft
        | None ->
            { ParamTypes =
                  f.Parameters
                  |> List.map (fun p -> p.Type |> Option.map tyFromTypeExpr |> Option.defaultValue TUnknown)
              ReturnType = None }

    if fty.ParamTypes.Length > 0 && fty.ParamTypes.Length <> f.Parameters.Length then
        typeError $"Function '{f.Name}' signature param count mismatch"

    let paramTypes =
        if fty.ParamTypes.Length = f.Parameters.Length && fty.ParamTypes.Length > 0 then
            fty.ParamTypes
        else
            f.Parameters
            |> List.map (fun p -> p.Type |> Option.map tyFromTypeExpr |> Option.defaultValue TUnknown)

    let localVars =
        (Map.empty<string, Ty>, List.zip f.Parameters paramTypes)
        ||> List.fold (fun m (p, t) -> m.Add(p.Name, t))

    let env =
        { Vars = localVars
          Funcs = globalFuncs }

    let bodyTy =
        match f.Body with
        | FuncBody.Single expr -> typeOfExpr env expr
        | FuncBody.Piecewise pieces ->
            let bodyTypes =
                pieces
                |> List.map (fun p ->
                    checkCondition env p.Condition
                    typeOfExpr env p.Body)
            match bodyTypes with
            | [] -> TUnknown
            | t0 :: rest ->
                rest
                |> List.fold
                    (fun acc t ->
                        match acc, t with
                        | TUnknown, t -> t
                        | t, TUnknown -> t
                        | a, b when a = b -> a
                        | a, b when isNumeric a && isNumeric b -> promoteNumeric a b
                        | _ -> typeError $"Branches of '{f.Name}' return incompatible types")
                    t0

    match fty.ReturnType with
    | None -> ()
    | Some rt ->
        if bodyTy <> TUnknown && not (canAssign bodyTy rt) then
            typeError $"Function '{f.Name}' body type does not match declared return type"

let checkProgram (prog: Program) =
    let funcTypes = buildFuncTypeEnv prog
    let sigMap = funcTypes
    prog
    |> List.iter (fun def ->
        match def with
        | Function f -> checkFuncDef funcTypes sigMap f
        | _ -> ())