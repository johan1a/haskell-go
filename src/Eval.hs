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
executeAssign  state (Assign lhs rhs) = bindAssign state (lhs !! 0) (rhs !! 0)
executeAssign state  _ = error "ey"


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
