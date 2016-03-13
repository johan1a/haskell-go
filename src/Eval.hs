module Eval where
import qualified Data.Map as Map
import AST

data Val = FunVal Env Id Expr
         | NumVal Int

instance Show Val where
  show (FunVal _ _ _) = "<fun>"
  show (NumVal n) = show n

type Env = Id -> Val

empty :: Env
empty = \_ -> error "Not found!"

eval :: Expr -> Val
eval = evalIn empty

evalIn :: Env -> Expr -> Val
evalIn env (Var x) = env x
evalIn _ (Num n) = NumVal n

--evalStmt :: Statement -> Env
--evalStmt stmt = 
--    case stmt of 
--        Declaration -> evalDecl 
--        _ -> error "fuck"

--state, program state

execute stmt state = 
    case stmt of
        Assignment a -> executeAssign a state
        _ -> error "ey"

executeAssign  stmt state =
    case stmt of
        Assign lhs rhs -> 2 --bind lhs rhs state 
        --OpAssign op e1 e2 -> 3
        _ -> error "ey"

--type state = Map

bind id val state = Map.insert id val state
