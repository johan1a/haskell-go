module Eval where
import Data.Map (Map)
import qualified Data.Map as Map
import AST
import Data.Maybe


type Env = Name -> Expr
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
runProgram stmts = return $ execStmts stmts empty

execStmts :: [Statement] -> State -> State
execStmts [] state = state
execStmts (s:ss) state = execStmts ss (execStmt s state)

execStmt :: Statement -> State -> State
execStmt (Expr e) =  error "todo"
execStmt (Declaration decl) = execDecl decl 
execStmt (SimpleStmt simpleStmt) = execSimpleStmt simpleStmt
execStmt (BlockStmt block) = error "TODO execute stmts in block"
execStmt (IfStmt ifStmt) = error "TODO"

--TODO implement types, multiple declarations
execDecl :: Declaration -> State -> State
execDecl (ConstDecl idDecls type_ exprs) state = bind (getName $ idDecls !! 0) (exprs !! 0) state 
execDecl (TypeDecl name type_) state = error "Types not implemented"
execDecl (VarDecl idDecls type_ exprs) state = bind (getName $ idDecls !! 0) (exprs !! 0) state 

execSimpleStmt :: SimpleStmt -> State -> State
execSimpleStmt (Assignment a) = executeAssign a


-- TODO assigns first in lhs to first in rhs.
-- does not yet support multiple declarations at once
executeAssign :: Assignment -> State -> State
executeAssign (Assign lhs rhs) state = bindAssign (lhs !! 0) (rhs !! 0) state 
executeAssign _ state = error "ey"


bindAssign :: Expr -> Expr -> State -> State
bindAssign (IdUse name) rhs state = bind name (eval rhs state) state
bindAssign _ rhs state = error "TODO"

type State = Map Name Expr

empty :: State
empty = Map.empty


getName (IdDecl name) = name

bind :: Name -> Expr -> State -> State
bind name val state = Map.insert name val state

env state x = fromJust $ Map.lookup x state

-- TODO should only work with id use?
-- maybe name instead of iduse?
lookup1 :: State -> Expr -> Expr
lookup1 state (IdUse x) = env state x
lookup1 _ (Num n) = error "not an idUse"


eval :: Expr -> State -> Expr
eval (IdUse x) state  = eval (lookup1 state (IdUse x)) state
eval (BinExpr e ) state = evalBin e state
eval (Num n) _ = Num n 

evalBin :: BinExpr -> State -> Expr
evalBin (AddExpr l r) state = add (eval l state) (eval r state) 
evalBin _ _ = error "ToDO"

add :: Expr -> Expr -> Expr
add (Num l) (Num r) = (Num (l + r))
add _ _ = error "todo"

sub :: Expr -> Expr -> Expr
sub (Num l) (Num r) = (Num (l - r))
sub _ _ = error "todo"

mul :: Expr -> Expr -> Expr
mul (Num l) (Num r) = (Num (l * r))
mul _ _ = error "todo"

div :: Expr -> Expr -> Expr
--div (Num l) (Num r) = (Num (l / r))
div  _ _ = error "todo"

{-
TODO add tests for  
eval empty (Num 5) : NumVal 5

-}
