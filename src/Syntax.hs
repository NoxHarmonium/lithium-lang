module Syntax where

type Name = String
type Path = String

data Expr
  = Float Double
  | BinaryOp Name Expr Expr
  | Var Name
  | VarDef Name Name
  | Call Name [Expr]
  | MethodCall Name [Expr]
  | Function Name [Expr] Expr
  | Module Name [Expr]
  | Class Name [Expr]
  | Import Name Path
  | Extern Name [Expr]
  | When [Expr]
  | Else
  | Clause Expr Expr
  | UnaryOp Name Expr
  deriving (Eq, Ord, Show)