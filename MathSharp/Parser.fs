module MathSharp.Parser

open FParsec
open MathSharp.Ast

type UserState = unit
type P<'a> = Parser<'a, UserState>

let ws p = spaces >>. p .>> spaces

let isIdentStart c = isLetter c || c = '_'
let isIdentCont c = isLetter c || isDigit c || c = '_'

let reserved =
    set [ "in"; "not"; "and"; "or"; "otherwise"; "true"; "false" ]

let keyword s: P<string> =
    ws (pstring s .>> notFollowedBy (satisfy isIdentCont))

let sym s: P<string> = ws (pstring s)

let pIdentifier: P<string> =
    ws (
        many1Satisfy2L isIdentStart isIdentCont "identifier"
        >>= fun name ->
                if reserved.Contains name then
                    fail $"reserved keyword '%s{name}'"
                else
                    preturn name
    )

let pTypeIdent: P<string> =
    ws (
        many1SatisfyL
            (fun c -> isLetter c || isDigit c || c = '_' || c = '\'')
            "type identifier"
    )

let rec pTypeExpr: P<TypeExpr> =
    let rec arrowType s =
        pipe2
            simpleType
            (opt (sym "->" >>. arrowType))
            (fun t1 rest ->
                match rest with
                | Some t2 -> ArrowType(t1, t2)
                | None -> t1)
            s
    and simpleType s =
        (attempt pSetType) <|> (pTypeIdent |>> SimpleType) <| s
    and pSetType s =
        between
            (sym "{")
            (sym "}")
            (sepBy1 pTypeExpr (sym ","))
        |>> SetType
        <| s
    arrowType

let pNumberLiteral: P<Expr> =
    let opts =
        NumberLiteralOptions.AllowFraction
        ||| NumberLiteralOptions.AllowExponent
    ws (
        numberLiteral opts "number"
        |>> fun nl ->
                if nl.IsInteger then IntLit(int nl.String)
                else RealLit(float nl.String)
    )

let pBoolLiteral: P<Expr> =
    (keyword "true" >>% BoolLit true)
    <|> (keyword "false" >>% BoolLit false)

let pStringLiteral: P<Expr> =
    let escapedChar: P<char> =
        choice [
            pstring "\\n" >>% '\n'
            pstring "\\t" >>% '\t'
            pstring "\\\"" >>% '"'
            pstring "\\\\" >>% '\\'
        ]

    let normalChar: P<char> =
        satisfy (fun c -> c <> '"' && c <> '\\' && c <> '\n' && c <> '\r')

    ws (
        between
            (pchar '"')
            (pchar '"')
            (manyChars (escapedChar <|> normalChar))
        |>> StringLit
    )

let pLiteral: P<Expr> =
    choice [
        attempt pBoolLiteral
        attempt pNumberLiteral
        pStringLiteral
    ]

let pParam: P<Param> =
    pipe2
        pIdentifier
        (opt (sym ":" >>. pTypeExpr))
        (fun name t -> { Name = name; Type = t })

let pParamList: P<Param list> =
    sepBy pParam (sym ",")

let rec pExpr s = pBoolOr s

and pAbs s =
    between (sym "|") (sym "|") pExpr
    |>> Abs
    <| s

and pListLiteral s =
    let inner = sepBy pExpr (sym ",")
    between (sym "[") (sym "]") inner
    |>> ListLiteral
    <| s

and pAnonFunc s =
    attempt (
        pipe2
            (between (sym "(") (sym ")") pParamList)
            (sym "->" >>. pExpr)
            (fun ps body -> AnonFunc(ps, body))
    ) s

and pParenExpr s =
    between (sym "(") (sym ")") pExpr s

and pCallOrVar s =
    let parser =
        pipe2
            pIdentifier
            (opt (between (sym "(") (sym ")") (sepBy pExpr (sym ","))))
            (fun name argsOpt ->
                match argsOpt with
                | Some args -> Call(name, args)
                | None -> Var name
            )
    parser s

and pPrimary s =
    choice [
        attempt pAnonFunc
        attempt pAbs
        attempt pListLiteral
        attempt pParenExpr
        attempt pCallOrVar
        pLiteral
    ] s

and pPostfix s =
    pipe2
        pPrimary
        (many (sym "!"))
        (fun e bangs ->
            List.fold (fun acc _ -> Factorial acc) e bangs
        )
        s

and pUnary s =
    let neg =
        sym "-" >>. pUnary
        |>> fun e -> Unary(UnaryOp.Neg, e)
    neg <|> pPostfix <| s

and pPow s =
    let powOp =
        sym "^" >>% (fun l r -> Binary(l, BinOp.Pow, r))
    chainr1 pUnary powOp s

and pMul s =
    let mulOp =
        (sym "*" >>% (fun l r -> Binary(l, BinOp.Mul, r)))
        <|> (sym "/" >>% (fun l r -> Binary(l, BinOp.Div, r)))
    chainl1 pPow mulOp s

and pSum s =
    let addOp =
        (sym "+" >>% (fun l r -> Binary(l, BinOp.Add, r)))
        <|> (sym "-" >>% (fun l r -> Binary(l, BinOp.Sub, r)))
    chainl1 pMul addOp s

and pCmpOp: P<CmpOp> =
    choice [
        attempt (keyword "not" >>. keyword "in" >>% CmpOp.NotIn)
        keyword "in" >>% CmpOp.In
        sym "<=" >>% CmpOp.Le
        sym ">=" >>% CmpOp.Ge
        sym "!=" >>% CmpOp.Neq
        sym "<" >>% CmpOp.Lt
        sym ">" >>% CmpOp.Gt
        sym "=" >>% CmpOp.Eq
    ]

and pCmp s =
    let parser =
        pipe2
            pSum
            (opt (pipe2 pCmpOp pSum (fun op right -> op, right)))
            (fun left cmpOpt ->
                match cmpOpt with
                | None -> left
                | Some (op, right) -> Compare(left, op, right)
            )
    parser s

and pBoolNot s =
    let notParser =
        keyword "not" >>. pBoolNot
        |>> fun e -> Unary(UnaryOp.Not, e)
    attempt notParser <|> pCmp <| s

and pBoolAnd s =
    let andOp =
        keyword "and" >>% (fun l r -> Logical(l, LogicalOp.And, r))
    chainl1 pBoolNot andOp s

and pBoolOr s =
    let orOp =
        keyword "or" >>% (fun l r -> Logical(l, LogicalOp.Or, r))
    chainl1 pBoolAnd orOp s

let pCondition: P<Condition> =
    (keyword "otherwise" >>% Otherwise)
    <|> (pExpr |>> ConditionExpr)

let pFuncHeader: P<string * Param list> =
    pipe2
        pIdentifier
        (between (sym "(") (sym ")") (opt pParamList))
        (fun name psOpt -> name, defaultArg psOpt [])

let pPieceLine: P<Piece> =
    pipe2
        pExpr
        (sym "," >>. pCondition)
        (fun body cond -> { Body = body; Condition = cond })

let pFuncBody: P<FuncBody> =
    let piecewise =
        between
            (sym "{")
            (sym "}")
            (many1 pPieceLine)
        |>> Piecewise
    attempt piecewise <|> (pExpr |>> Single)

let pFuncSig: P<FuncSig> =
    pipe2
        pIdentifier
        (sym ":" >>. pTypeExpr)
        (fun name ty ->
            { Name = name
              Type = ty })

let pConstName: P<string> =
    pIdentifier .>> notFollowedBy (spaces >>. pchar '(')

let pConstDef: P<ConstDef> =
    pipe2
        pConstName
        (sym "=" >>. pExpr)
        (fun name value ->
            { Name = name
              Value = value })

let pFuncDef: P<FuncDef> =
    pipe2
        pFuncHeader
        (sym "=" >>. pFuncBody)
        (fun (name, ps) body ->
            { Name = name
              Parameters = ps
              Body = body })

let pTopLevelDef: P<TopLevelDef> =
    choice [
        attempt (pFuncSig |>> Signature)
        attempt (pFuncDef |>> Function)
        pConstDef |>> Const
    ]

let pProgram: P<Program> =
    spaces >>. many (pTopLevelDef .>> spaces) .>> eof