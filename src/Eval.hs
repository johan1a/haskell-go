module Eval where
import Data.Map (Map)
import qualified Data.Map as Map
import AST
import Data.Maybe

data Val = FunVal Expr
         | NumVal Int
         deriving (Show, Eq)

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

runProgram :: [Statement] -> IO (State)
runProgram stmts = return $ execStmts empty stmts

execStmts :: State -> [Statement] -> State
execStmts state [] = state
execStmts state (s:ss) = execStmts (execStmt state s) ss

execStmt :: State -> Statement -> State
execStmt state (Expr e) = state
execStmt state (Declaration decl) = execDecl state decl
execStmt state (SimpleStmt simpleStmt) = execSimpleStmt state simpleStmt
execStmt state (BlockStmt block) = error "TODO execute stmts in block"
execStmt state (IfStmt ifStmt) = error "TODO"

--TODO implement types, multiple declarations
execDecl :: State -> Declaration -> State
execDecl state (ConstDecl idDecls type_ exprs) = bind state (getName $ idDecls !! 0) (exprs !! 0)
execDecl state (TypeDecl name type_) = error "Types not implemented"
execDecl state (VarDecl idDecls type_ exprs) = bind state (getName $ idDecls !! 0) (exprs !! 0)

execSimpleStmt :: State -> SimpleStmt -> State
execSimpleStmt state stmt = 
    case stmt of
        Assignment a -> executeAssign state a 
        _ -> error "ey"


-- TODO assigns first in lhs to first in rhs.
-- does not yet support multiple declarations at once
executeAssign :: State -> Assignment -> State
executeAssign  state stmt =
    case stmt of
        Assign lhs rhs -> bindAssign state (lhs !! 0) (rhs !! 0)
        --OpAssign op e1 e2 -> 3
        _ -> error "ey"


bindAssign state (IdUse name) rhs = bind state name rhs
bindAssign state _ rhs = error "TODO"

type State = Map Name Expr

empty :: State
empty = Map.empty


getName (IdDecl name) = name

bind :: State -> Name -> Expr -> State
bind state name val = Map.insert name val state

env state x = fromJust $ Map.lookup x state

-- TODO should only work with id use?
-- maybe name instead of iduse?
lookup1 :: State -> Expr -> Expr
lookup1 state (IdUse x) = env state x
lookup1 _ (Num n) = error "not an idUse"


eval :: State -> Expr -> Val
eval state (IdUse x) = eval state $ lookup1 state (IdUse x)
eval _ (Num n) = NumVal n 




{-
TODO add tests for  
eval empty (Num 5) : NumVal 5

-}
