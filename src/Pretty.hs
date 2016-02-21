module Pretty where
import Expr

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
