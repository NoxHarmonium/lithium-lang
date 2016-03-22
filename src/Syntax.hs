module Syntax where

type Name = String
type Path = String


data Expr
  = Float Double
  | BinOp Op Expr Expr
  | BinComp Comparison Expr Expr
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
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)

data Comparison
  = Equal
  | NotEqual
  | GreaterThan
  | LessThan
  | EqualToOrGreaterThan
  | EqualToOrGreaterLess
  deriving (Eq, Ord, Show)
