module AST where

type Id = String

data Op = Add | Sub | Mul deriving (Eq,Show)


data Decl =  Id 

data Stmt = Abs Id Stmt
          | App Stmt Stmt
          | Var Id
          | Num Int
          | Binop Op Stmt Stmt
          | Bajs Id
          deriving (Eq,Show)


--data Statement      = Declaration 
  --                    deriving (Eq, Show)

--data Declaration    = Bajs Id 
    --                  deriving (Eq, Show)

--data ConstDecl      = ConstSpec 
--type ConstSpec      = Id Stmt Id --IdentifierList Type StmtessionList 

--type IdentifierList = Id 
--data StmtessionList = Stmt deriving (Eq, Show)

--type Type           = Id 



