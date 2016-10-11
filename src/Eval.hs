module Eval where
import Data.Map (Map)
import qualified Data.Map as Map
import AST
import Data.Maybe


{-
type Env = Name -> Expr
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

type Values = Map Name Expr

type Parameters = Map Name [Name]

type Functions = Map Name [Statement]

data State = State Values Parameters Functions

lookup2 name map  = fromJust $ Map.lookup name map



empty :: State
empty = State Map.empty Map.empty Map.empty

runProgram :: [Statement] -> IO (State)
runProgram stmts = return $ execStmts stmts empty

execStmts :: [Statement] -> State -> State
execStmts [] state = state
execStmts (s:ss) state= execStmts ss $ execStmt s state

execStmt :: Statement -> State -> State
execStmt (Expr e) =  error "todo"
execStmt (Declaration decl) = execDecl decl 
execStmt (SimpleStmt simpleStmt) = execSimpleStmt simpleStmt
execStmt (BlockStmt block) = error "TODO exec stmts in block"
execStmt (IfStmt ifStmt) = error "TODO"

--TODO implement types, multiple declarations
execDecl :: Declaration -> State -> State
execDecl (ConstDecl idDecls type_ exprs) = bindVal (getName $ idDecls !! 0) (exprs !! 0) 
execDecl (TypeDecl name type_) = error "Types not implemented"
execDecl (VarDecl idDecls type_ exprs) = bindVal (getName $ idDecls !! 0) (exprs !! 0)

execSimpleStmt :: SimpleStmt -> State -> State
execSimpleStmt (Assignment a) = execAssign a
execSimpleStmt (ExpressionStmt e) = execExprStmt e


execExprStmt :: Expr  -> State -> State
execExprStmt (Call name e) = execFunc name e 

execFunc :: Name -> [Expr] -> State -> State
execFunc name args (State v p f) = execStmts (lookup2 name f) (bindArgs name args (State v p f))


-- TODO assigns first in lhs to first in rhs.
-- does not yet support multiple declarations at once
execAssign :: Assignment -> State -> State
execAssign (Assign lhs rhs) = bindAssign (lhs !! 0) (rhs !! 0) 
execAssign _ = error "ey"


bindAssign :: Expr -> Expr -> State -> State
bindAssign (IdUse name) rhs state = bindVal name (eval rhs state) state
bindAssign _ rhs state = error "TODO"


getName (IdDecl name) = name


bindMany :: [Name] -> [Expr] -> State -> State
bindMany (n:nn) (a:aa) state = bindMany nn aa $ bindVal n a state
bindMany _ _ state = state

bindArgs :: Name -> [Expr] -> State -> State
bindArgs funcName args (State v p f) = bindMany (lookup2 funcName p) args (State v p f)

bindVal :: Name -> Expr -> State -> State
bindVal name val (State v p f) = State (Map.insert name val v) p f


-- TODO should only work with id use?
-- maybe name instead of iduse?
lookup1 :: Expr -> State -> Expr
lookup1 (IdUse name) (State v p f) = lookup2 name v
lookup1 (Num n) _ = error "not an idUse"





eval :: Expr -> State -> Expr
eval (IdUse x) state  = eval (lookup1 (IdUse x) state ) state
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
