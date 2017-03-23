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

data State = State { values :: Values,
                     params :: Parameters, 
                     decls  :: Map Name Expr, --TODO types
                     funcs :: Map Name FunctionDecl
                   } deriving (Show)

lookup2 name map_ = traceShow map_ $ fromJust $ Map.lookup name map_



empty :: State
empty = State Map.empty Map.empty Map.empty Map.empty

runProgram :: SourceFile -> IO State
runProgram (SourceFile package decls) = readTopLevelDecls decls empty >>= runMain

runMain :: State -> IO State
runMain state = runFuncDecl (getFuncDecl "main" state) state

runFuncDecl :: FunctionDecl -> State -> IO State
runFuncDecl (FunctionDecl1 _ _ ) = error "No function body!"
runFuncDecl (FunctionDecl2 name signature body) = execFunc name signature body
-- TODO lookup parameters parameters

getFuncDecl :: FunctionName -> State -> FunctionDecl
getFuncDecl name state = lookup2 name $ funcs state

readTopLevelDecls :: [TopLevelDecl] -> State -> IO State
readTopLevelDecls [] state = return state
readTopLevelDecls (s:ss) state = readTopLevelDecl s state >>= readTopLevelDecls ss 

readTopLevelDecl :: TopLevelDecl -> State -> IO State
readTopLevelDecl (TopLevelDecl1 decl) state = return $ storeDecl decl state
readTopLevelDecl (TopLevelDecl2 fDecl) state = return $ storeFuncDecl fDecl state

--TODO rename
storeDecl :: Declaration -> State -> State
storeDecl (ConstDecl idDecls type_ exprs) state = bindDecls idDecls type_ exprs state 
storeDecl (TypeDecl name type_) state = error "TODO typedecl"
storeDecl (VarDecl idDecls type_ exprs) state = bindDecls idDecls type_ exprs state 

bindDecls :: [IdDecl] -> Type -> [Expr] -> State -> State
bindDecls [] type_ [] state = state
bindDecls [] type_ _ state = error "mismatch1 TODO error?"
bindDecls _ type_ [] state = error "mismatch2 TODO error?"
bindDecls (d:dd) type_ (e:ee) state = bindDecls dd type_ ee $ bindDecls dd type_ ee $ bindDecl d type_ e state

--TODO type
bindDecl :: IdDecl -> Type -> Expr -> State -> State
bindDecl (IdDecl name) type_ expr state = state { decls = (Map.insert name expr $ decls state) }


storeFuncDecl :: FunctionDecl -> State -> State
storeFuncDecl decl state = state { funcs = (Map.insert (fName decl) decl $ funcs state) }
 
fName :: FunctionDecl -> String
fName (FunctionDecl1 name _) = name
fName (FunctionDecl2 name _ _) = name

topDeclName :: TopLevelDecl -> String
topDeclName (TopLevelDecl1 decl) = declName decl
topDeclName (TopLevelDecl2 funcDecl) = funcDeclName funcDecl

funcDeclName :: FunctionDecl -> FunctionName
funcDeclName (FunctionDecl1 name _) = name
funcDeclName (FunctionDecl2 name _ _) = name

declName :: Declaration -> String
declName (ConstDecl idDecls _ _) = idDeclName $ idDecls !! 0 -- TODO all names etc blabla
declName (TypeDecl name _ ) = name
declName (VarDecl idDecls _ _) = idDeclName $ idDecls !! 0

idDeclName (IdDecl name) = name

readDeclaration :: Declaration -> State -> IO State
readDeclaration decl state = error "TODO"




-- Execution below

execStmts :: [Statement] -> State -> IO State
execStmts [] state = return state
execStmts (s:ss) state = execStmt s state >>= execStmts ss 

execStmt :: Statement -> State -> IO State
execStmt (Expr e) =  error "todo"
execStmt (DeclarationStmt decl) = return . execDecl decl  
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
execSimpleStmt EmptyStmt = return 
execSimpleStmt (ExpressionStmt expr) = execExprStmt expr 
execSimpleStmt (IncDecStmt stmt) = execIncDecStmt stmt 
execSimpleStmt (ShortVarDecl decls exprs) = execShortVarDecl decls exprs

execIncDecStmt :: IncDecStmt -> State -> IO State
execIncDecStmt (IncStmt expr) = error "TODO" 
execIncDecStmt (DecStmt expr) = error "TODO" 

execShortVarDecl :: [IdDecl] -> [Expr] -> State -> IO State
execShortVarDecl decls exprs = error "TODO" 


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
execExprStmt (Call name e) st = traceShow "call" $ execFuncCall name e st
execExprStmt (PrintCall e) st = do 
			putStrLn $ traceShow "printCall" $ show $ eval (e !! 0 ) st --Print multiple
			return st 
execExprStmt (Num n) st = return st

execFuncCall :: Name -> [Expr] -> State -> IO State
execFuncCall name args state = runFuncDecl (getFuncDecl name state) (bindArgs name args state)

execFunc :: FunctionName -> Signature -> FunctionBody -> State -> IO State
execFunc name sig body state = execBlock body state

traceShowX state = traceShow (lookup2 "x" (decls state)) 

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

--Bind the given arguments to the formal parameter names of the function
bindArgs :: Name -> [Expr] -> State -> State
bindArgs funcName exprs state = traceShow state $ bindMany (paramNames funcName state) exprs state

paramNames funcName state = lookup2 funcName $ params state

bindVal :: Name -> Expr -> State -> State
bindVal name val state = state { decls = Map.insert name val $ decls state}

-- TODO should only work with id use?
-- maybe name instead of iduse?
lookupExpr :: Expr -> State -> Expr
lookupExpr (IdUse name) state = lookup2 name (decls state)
lookupExpr (Num n) _ = error "not an idUse"

lookupName :: String -> State -> Expr
lookupName name state = lookup2 name $ decls state

evalBool :: Expr -> State -> Bool --TODO return state?
evalBool expr state = asBoolVal (eval expr state)

asBoolVal :: Value -> Bool
asBoolVal (BoolVal v) = v
asBoolVal (NumVal _) = error "not a bool"
asBoolVal (StringVal _) = error "not a bool"

eval :: Expr -> State -> Value
eval (IdUse name) state  = eval (lookupName name state) state
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
