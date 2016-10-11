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
runProgram stmts = return $ execStmts empty stmts

execStmts :: State -> [Statement] -> State
execStmts state [] = state
execStmts state (s:ss) = execStmts (execStmt s state) ss

execStmt :: Statement -> State -> State
execStmt (Expr e) =  error "todo"
execStmt (Declaration decl) = execDecl decl 
execStmt (SimpleStmt simpleStmt) = execSimpleStmt simpleStmt
execStmt (BlockStmt block) = error "TODO execute stmts in block"
execStmt (IfStmt ifStmt) = error "TODO"

--TODO implement types, multiple declarations
execDecl :: Declaration -> State -> State
execDecl (ConstDecl idDecls type_ exprs) state = bind state (getName $ idDecls !! 0) (exprs !! 0)
execDecl (TypeDecl name type_) state = error "Types not implemented"
execDecl (VarDecl idDecls type_ exprs) state = bind state (getName $ idDecls !! 0) (exprs !! 0)

execSimpleStmt :: SimpleStmt -> State -> State
execSimpleStmt (Assignment a) = executeAssign a


-- TODO assigns first in lhs to first in rhs.
-- does not yet support multiple declarations at once
executeAssign :: Assignment -> State -> State
executeAssign (Assign lhs rhs) state = bindAssign state (lhs !! 0) (rhs !! 0)
executeAssign _ state = error "ey"


bindAssign :: State -> Expr -> Expr -> State
bindAssign state (IdUse name) rhs = bind state name $ eval state rhs
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


eval :: State -> Expr -> Expr
eval state (IdUse x) = eval state $ lookup1 state (IdUse x)
eval state (BinExpr e ) = evalBin state e
eval _ (Num n) = Num n 

evalBin :: State -> BinExpr -> Expr
evalBin state (AddExpr l r) = add (eval state l) (eval state r) 
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
