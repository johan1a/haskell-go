module AST where

type Id = String

data Op = Add | Sub | Mul deriving (Eq,Show)



data Stmt = Expr Expr
          | Decl Id Id Expr
          deriving (Eq, Show)


data Expr = Abs Id Expr
          | App Stmt Expr
          | Num Int
          | Var Id
          | Binop Op Expr Expr
          deriving (Eq, Show)

--data Statement      = Declaration 
  --                    deriving (Eq, Show)

--data Declaration    = Bajs Id 
    --                  deriving (Eq, Show)

--data ConstDecl      = ConstSpec 
--type ConstSpec      = Id Stmt Id --IdentifierList Type StmtessionList 

--type IdentifierList = Id 
--data StmtessionList = Stmt deriving (Eq, Show)

--type Type           = Id 



