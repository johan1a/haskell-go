module Eval where
import Data.Map (Map)
import qualified Data.Map as Map
import AST
import Data.Maybe

data Val = FunVal Expr
         | NumVal Int

type Env = Name -> Val
{-
empty :: Env
empty = \_ -> error "Not found!"

eval :: Expr -> Val
eval = evalIn empty


-}
--evalStmt :: Statement -> Env
--evalStmt stmt = 
--    case stmt of 
--        Declaration -> evalDecl 
--        _ -> error "fuck"

--state, program state

execute :: SimpleStmt -> State -> State
execute stmt state = 
    case stmt of
        Assignment a -> executeAssign a state
        _ -> error "ey"


-- TODO assigns first in lhs to first in rhs.
-- does not yet support multiple declarations at once
executeAssign :: Assignment -> State -> State
executeAssign  stmt state =
    case stmt of
        Assign lhs rhs -> bind (lhs !! 0) (rhs !! 0) state 
        --OpAssign op e1 e2 -> 3
        _ -> error "ey"

type State = Map Name Expr

empty :: State
empty = Map.empty

bind :: Expr -> Expr -> State -> State
bind id val state = 
    case id of
        Num x -> error "x"
        IdUse name -> Map.insert name val state

env state x = fromJust  $ Map.lookup x state

lookup :: State -> Expr -> Expr
lookup state (IdUse x) = env state x
lookup _ (Num n) = (Num n)