module AST where

type Id = String

data Op = Add | Sub | Mul deriving (Eq,Show)

data Type = Type Id
           deriving (Eq, Show)

data Stmt = Expr Expr
          | Declaration Declaration
          deriving (Eq, Show)

data Declaration = ConstDecl ConstDecl
                 | TypeDecl TypeDecl
                  deriving (Eq, Show)

data ConstDecl = ConstSpec Id Type Expr
                deriving (Eq, Show)

data TypeDecl = TypeSpec Id Type  --id type
                deriving (Eq, Show)

data Expr = Abs Id Expr
          | App Stmt Expr
          | Num Int
          | Var Id
          | Binop Op Expr Expr
          deriving (Eq, Show)

