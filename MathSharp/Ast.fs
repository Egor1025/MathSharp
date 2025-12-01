module MathSharp.Ast

type TypeExpr =
    | SimpleType of string
    | ArrowType of TypeExpr * TypeExpr
    | SetType of TypeExpr list

type Param =
    { Name: string
      Type: TypeExpr option }

type BinOp =
    | Add
    | Sub
    | Mul
    | Div
    | Pow

type CmpOp =
    | Eq
    | Neq
    | Lt
    | Le
    | Gt
    | Ge
    | In
    | NotIn

type LogicalOp =
    | And
    | Or

type UnaryOp =
    | Neg
    | Not

type Expr =
    | IntLit of int
    | RealLit of float
    | BoolLit of bool
    | StringLit of string
    | Var of string
    | Unary of UnaryOp * Expr
    | Binary of Expr * BinOp * Expr
    | Compare of Expr * CmpOp * Expr
    | Logical of Expr * LogicalOp * Expr
    | Abs of Expr
    | Call of string * Expr list
    | ListLiteral of Expr list
    | AnonFunc of Param list * Expr
    | Factorial of Expr

type Condition =
    | Otherwise
    | ConditionExpr of Expr

type Piece =
    { Body: Expr
      Condition: Condition }

type FuncBody =
    | Single of Expr
    | Piecewise of Piece list

type FuncSig =
    { Name: string
      Type: TypeExpr }

type ConstDef =
    { Name: string
      Value: Expr }

type FuncDef =
    { Name: string
      Parameters: Param list
      Body: FuncBody }

type TopLevelDef =
    | Const of ConstDef
    | Signature of FuncSig
    | Function of FuncDef

type Program = TopLevelDef list