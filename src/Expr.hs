module Expr where

type Id = String

data Op = Add | Sub | Mul deriving (Eq,Show)

data Expr = Abs Id Expr
          | App Expr Expr
          | Var Id
          | Num Int
          | Binop Op Expr Expr
          deriving (Eq,Show)

pretty :: Expr -> String
pretty expr = case expr of
  (Abs x e) -> parens $ "\\" ++ x ++ " -> " ++ pretty e
  (App e1 e2) -> parens $ pretty e1 ++ " " ++ pretty e2
  (Binop op e1 e2) -> parens $ pretty e1 ++ sourceOp op ++ pretty e2
  (Var x) -> x
  (Num n) -> show n
  where sourceOp Add = " + "
        sourceOp Sub = " - "
        sourceOp Mul = " * "
        parens s = "(" ++ s ++ ")"

addExpr :: Expr -> Expr -> Expr
addExpr = Binop Add

subExpr :: Expr -> Expr -> Expr
subExpr = Binop Sub

mulExpr :: Expr -> Expr -> Expr
mulExpr = Binop Mul
