module Eval where
import Data.Map (Map)
import qualified Data.Map as Map
import AST
import Data.Maybe
import Debug.Trace


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

-- rename to exprs?
type Values = Map Name Expr

type Parameters = Map Name [Name]

type Functions = Map Name [Statement]

data State = State Values Parameters Functions
	deriving (Show)

lookup2 name map_  = fromJust $ Map.lookup name map_



empty :: State
empty = State Map.empty Map.empty Map.empty

runProgram :: [Statement] -> IO State
runProgram [] = return empty
runProgram stmts = execStmts stmts empty  

execStmts :: [Statement] -> State -> IO State
execStmts [] state = return state
execStmts (s:ss) state = execStmt s state >>= execStmts ss 

execStmt :: Statement -> State -> IO State
execStmt (Expr e) =  error "todo"
execStmt (Declaration decl) = return . execDecl decl  
execStmt (SimpleStmt simpleStmt) = execSimpleStmt simpleStmt
execStmt (BlockStmt block) = execBlock block
execStmt (IfStmt ifStmt) = execIfStmt ifStmt

execBlock :: Block -> State -> IO State
execBlock (Block []) = return
execBlock (Block stmts) = execStmts stmts


--TODO implement types, multiple declarations
execDecl :: Declaration -> State -> State
execDecl (ConstDecl idDecls type_ exprs) state = bindVal (getName $ idDecls !! 0) ( (exprs !! 0) ) state
execDecl (TypeDecl name type_) state = error "Types not implemented"
execDecl (VarDecl idDecls type_ exprs) state = bindVal (getName $ idDecls !! 0) ( (exprs !! 0) ) state

execSimpleStmt :: SimpleStmt -> State -> IO State
execSimpleStmt (Assignment a) = return . execAssign a
execSimpleStmt (ExpressionStmt e) = execExprStmt e 

execIfStmt :: IfStmt -> State -> IO State
execIfStmt (Ifstmt1 expr block)  state
	| evalBool expr state = execBlock block state
	| otherwise = return state-- TODO error?
execIfStmt (Ifstmt2 expr block els) state
	| evalBool expr state = execBlock block state
	| otherwise = execElse els state

execElse :: Else -> State -> IO State
execElse (Else1 ifStmt) = execIfStmt ifStmt
execElse (Else2 block) = execBlock block 


execExprStmt :: Expr  -> State -> IO State
execExprStmt (PrintCall e) st = do 
			putStrLn $ show $ eval (e !! 0 ) st --Print multiple
			return st 
execExprStmt (Call name e) st = execFunc name e st
execExprStmt (Num n) st = return st

execFunc :: Name -> [Expr] -> State -> IO State
execFunc name args (State v p f) = execStmts (lookup2 name f) (bindArgs name args (State v p f))


-- TODO assigns first in lhs to first in rhs.
-- does not yet support multiple declarations at once
execAssign :: Assignment -> State -> State
execAssign (Assign lhs rhs) = bindAssign (lhs !! 0) (rhs !! 0) 
execAssign _ = error "ey"


bindAssign :: Expr -> Expr -> State -> State
bindAssign (IdUse name) rhs state = bindVal name rhs state
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
lookupExpr :: Expr -> State -> Expr
lookupExpr (IdUse name) (State v p f) = lookup2 name v
lookupExpr (Num n) _ = error "not an idUse"


evalBool :: Expr -> State -> Bool --TODO return state?
evalBool expr state = asBoolVal (eval expr state)

asBoolVal :: Value -> Bool
asBoolVal (BoolVal v) = v
asBoolVal (NumVal _) = error "not a bool"
asBoolVal (StringVal _) = error "not a bool"

eval :: Expr -> State -> Value
eval (IdUse x) state  = eval (lookupExpr (IdUse x) state ) state
eval (BinExpr e ) state = evalBin e state
eval (Num n) _ = NumVal n 
eval (BoolExpr b) _ = BoolVal b
eval (StringExpr s) _ = StringVal s

evalBin :: BinExpr -> State -> Value
evalBin (AritmExpr a) state = evalAritm a state
evalBin (CondExpr c) state = evalCond c state
evalBin _ _ = error "ToDO"

evalAritm :: AritmExpr -> State -> Value
evalAritm (AddExpr l r) state = add (eval l state) (eval r state) 
evalAritm (SubExpr l r) state = sub (eval l state) (eval r state) 
evalAritm (MulExpr l r) state = mul (eval l state) (eval r state) 
evalAritm (DivExpr l r) state = div_ (eval l state) (eval r state) 

evalCond :: CondExpr -> State -> Value
evalCond (Eq_ l r) state = BoolVal $ (eval l state) == (eval r state)

add :: Value -> Value -> Value
add (NumVal l) (NumVal r) = NumVal (l + r)
add _ _ = error "todo"

sub :: Value -> Value -> Value
sub (NumVal l) (NumVal r) = (NumVal (l - r))
sub _ _ = error "todo"

mul :: Value -> Value -> Value
mul (NumVal l) (NumVal r) = (NumVal (l * r))
mul _ _ = error "todo"

div_ :: Value -> Value -> Value
--div (Num l) (Num r) = (Num (l / r))
div_  _ _ = error "todo"

{-
TODO add tests for  
eval empty (Num 5) : NumVal 5

-}
