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


type ActRec = Map Name Expr

data State = State { actRecs :: [ActRec], --TODO types
                     params  :: Map Name [Name],
                     funcs   :: Map Name FunctionDecl,
                     retVal  :: Value
                   } deriving (Show)

-- Returns the activation record of the current scope
decls :: State -> ActRec
decls state = (actRecs state) !! 0

lookup2 name map_ = Map.lookup name map_

empty :: State
empty = State { actRecs = [], 
                params = Map.empty,
                funcs = Map.empty, 
                retVal = NullVal }

runProgram :: SourceFile -> IO State
runProgram (SourceFile package decls) = readTopLevelDecls decls empty >>= runMain

runMain :: State -> IO State
runMain = execFuncCall "main" []

getFuncDecl :: FunctionName -> State -> FunctionDecl
getFuncDecl name state = fromJust $ lookup2 name $ funcs state

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


storeFuncDecl :: FunctionDecl -> State -> State
storeFuncDecl decl state = state { funcs = (Map.insert (fName decl) decl $ funcs state),
                                   params = (Map.insert (fName decl) (fParams decl) $ params state) }
                      
fName :: FunctionDecl -> String
fName (FunctionDecl1 name _) = name
fName (FunctionDecl2 name _ _) = name

-- Get the param names of a function declaration
fParams :: FunctionDecl -> [Name]
fParams (FunctionDecl1 _ sig) = sParams sig
fParams (FunctionDecl2 _ sig _) = sParams sig

sParams :: Signature -> [String]
sParams Signature1 = []
sParams (Signature2 paramDecls ) = paramDecls >>= paramDeclNames 
sParams (Signature3 paramDecls _ ) = paramDecls >>= paramDeclNames 

paramDeclNames :: ParameterDecl -> [Name]
paramDeclNames (ParameterDecl1 type_) = error "TODO unnamed parameters"
paramDeclNames (ParameterDecl2 idDecls type_) = map idDeclName idDecls

idDeclName :: IdDecl -> Name
idDeclName (IdDecl name) = name


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

readDeclaration :: Declaration -> State -> IO State
readDeclaration decl state = error "TODO"




-- Execution below

execStmts :: [Statement] -> State -> IO State
execStmts [] state = return state
execStmts (s:ss) state = execStmt s state >>= execStmts ss 

execStmt :: Statement -> State -> IO State
execStmt (Expr e) st =  error "todo"
execStmt (DeclarationStmt decl) st = return $ execDecl decl st
execStmt (SimpleStmt simpleStmt) st = execSimpleStmt simpleStmt st
execStmt (BlockStmt block) st = execBlock block st
execStmt (IfStmt ifStmt) st = execIfStmt ifStmt st
execStmt (ReturnStmt expr) st = do
    val <- eval expr st        
    return $ st { retVal = val }

--TODO implement types, multiple declarations
execDecl :: Declaration -> State -> State
execDecl (ConstDecl idDecls type_ exprs) state = bindExpr (getName $ idDecls !! 0) ( (exprs !! 0) ) state 
execDecl (TypeDecl name type_) state = error "Types not implemented"
execDecl (VarDecl idDecls type_ exprs) state = bindExpr (getName $ idDecls !! 0) ( (exprs !! 0) ) state

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


-- Yikes...
execIfStmt :: IfStmt -> State -> IO State
execIfStmt (Ifstmt1 expr block) state = do
    b <- evalBool expr state
    st <- execIfStmt2 b block Nothing state
    return st
execIfStmt (Ifstmt2 expr block els) state = do
    b <- evalBool expr state
    st <- execIfStmt2 b block (Just els) state
    return st

execIfStmt2 :: Bool -> Block -> Maybe Else -> State -> IO State
execIfStmt2 cond ifBlock (Just els) state
    | cond = execBlock ifBlock state
    | otherwise = execElse els state
execIfStmt2 cond ifBlock Nothing state
    | cond = execBlock ifBlock state
    | otherwise = return state

execElse :: Else -> State -> IO State
execElse (Else1 ifStmt) = execIfStmt ifStmt
execElse (Else2 block) = execBlock block 

--Print multiple
execExprStmt :: Expr  -> State -> IO State
execExprStmt (Call name e) st = execFuncCall name e st
execExprStmt (PrintLnCall e) st = do 
            v <- eval (e !! 0 ) st
            putStrLn $ show v  
            return st 
execExprStmt (PrintCall e) st = do 
            v <- eval (e !! 0 ) st
            putStr $ show v  
            return st 
execExprStmt (BoolExpr b) st = error "error bool"
execExprStmt (Num n) st = error "error num" 
execExprStmt (IdUse id) st = error $  "error id: " ++ id
execExprStmt (StringExpr str) st = error "error: str"

execFuncCall :: Name -> [Expr] -> State -> IO State
execFuncCall name args state = execFuncDecl (getFuncDecl name state) (bindArgs name args state)

execFuncDecl :: FunctionDecl -> State -> IO State
execFuncDecl (FunctionDecl1 _ _ ) st = error "No function body!"
execFuncDecl (FunctionDecl2 name sig body) st = execBlock body st
-- TODO lookup parameters parameters

execBlock :: Block -> State -> IO State
execBlock (Block []) st = return st
execBlock (Block stmts) st = execStmts stmts st

-- TODO assigns first in lhs to first in rhs.
-- does not yet support multiple declarations at once
execAssign :: Assignment -> State -> State
execAssign (Assign lhs rhs) = bindAssign (lhs !! 0) (rhs !! 0) 
execAssign _ = error "ey"

bindAssign :: Expr -> Expr -> State -> State
bindAssign (IdUse name) rhs state = bindExpr name rhs state 
bindAssign _ rhs state = error "TODO"

getName (IdDecl name) = name

--Bind the given arguments to the formal parameter names of the function
bindArgs :: Name -> [Expr] -> State -> State
bindArgs funcName exprs state = bindExprs (fParams $ getFuncDecl funcName state) exprs (state { actRecs = [Map.empty] ++ (actRecs state)})

bindExprs :: [Name] -> [Expr] -> State-> State
bindExprs [] [] state = traceShow "bindExprs1 " $ state
bindExprs (n:nn) (a:aa) state = traceShow ("bindExprs " ++ (show state))  $ (bindExpr n a state)
bindExprs _ _ state = traceShow "bindExprs2 " $ state

bindExpr :: Name -> Expr -> State -> State
bindExpr name expr state = traceShowId $ state { actRecs = [Map.insert name (lookupExpr expr state) $ decls state ] ++ (tail $ actRecs state)}

--TODO type
bindDecl :: IdDecl -> Type -> Expr -> State -> State
bindDecl (IdDecl name) type_ expr state = bindExpr name expr state

paramNames :: String -> State -> [String]
paramNames funcName state = fromJust $ lookup2 funcName $ params state

-- If given an IdUse, it tries to find what expression is actually referenced
lookupExpr :: Expr -> State -> Expr
lookupExpr (IdUse name) state = case found of (Just expr) -> lookupExpr expr (scopeAbove state)
                                              Nothing -> lookupExpr (IdUse name) (scopeAbove state) -- TODO should actually throw an error here?
    where found = traceShow (actRecs state) $ traceShow ("found " ++ (show $ lookup2 name (decls state))) $ lookup2 name (decls state)
lookupExpr (Num n) state = (Num n)
lookupExpr (BinExpr e ) state = lookupBinExpr e state
lookupExpr (BoolExpr b) _ = (BoolExpr b)
lookupExpr (StringExpr s) _ = (StringExpr s)
lookupExpr (Call fName exprs) state = (Call fName (map (\x -> lookupExpr x state) exprs))  
lookupExpr expr state = error "i felt like it " --expr

lookupBinExpr :: BinExpr -> State -> Expr
lookupBinExpr (AritmExpr expr) state = BinExpr (AritmExpr (lookupAritmExpr expr state))
lookupBinExpr (CondExpr expr) state = BinExpr (CondExpr (lookupCondExpr expr state))

lookupAritmExpr :: AritmExpr -> State -> AritmExpr
lookupAritmExpr (AddExpr l r) state = (AddExpr (lookupExpr l state) (lookupExpr r state))
lookupAritmExpr (SubExpr l r) state = (SubExpr (lookupExpr l state) (lookupExpr r state))
lookupAritmExpr (MulExpr l r) state = (MulExpr (lookupExpr l state) (lookupExpr r state))
lookupAritmExpr (DivExpr l r) state = (DivExpr (lookupExpr l state) (lookupExpr r state))
lookupAritmExpr (ModExpr l r) state = (ModExpr (lookupExpr l state) (lookupExpr r state))


lookupCondExpr :: CondExpr -> State -> CondExpr
lookupCondExpr (Eq_ l r) state = (Eq_ (lookupExpr l state) (lookupExpr r state))
lookupCondExpr (Neq l r) state = (Neq (lookupExpr l state) (lookupExpr r state))
lookupCondExpr (Less l r) state = (Less (lookupExpr l state) (lookupExpr r state))
lookupCondExpr (LessEq l r) state = (LessEq (lookupExpr l state) (lookupExpr r state))
lookupCondExpr (Greater l r) state = (Greater (lookupExpr l state) (lookupExpr r state))
lookupCondExpr (GreaterEq l r) state = (GreaterEq (lookupExpr l state) (lookupExpr r state))



scopeAbove :: State -> State
scopeAbove state = traceShow ( actRecs state) $ state { actRecs = (tail $ actRecs state) }

evalBool :: Expr -> State -> IO Bool --TODO return state?
evalBool expr state = fmap asBoolVal (eval expr state)

asBoolVal :: Value -> Bool
asBoolVal (BoolVal v) = v
asBoolVal (NumVal _) = error "not a bool"
asBoolVal (StringVal _) = error "not a bool"

eval :: Expr -> State -> IO Value
eval (IdUse name) state = eval (lookupExpr (IdUse name) state) state
eval (BinExpr e ) state = evalBin e state
eval (Num n) _ = return $ NumVal n 
eval (BoolExpr b) _ = return $ BoolVal b
eval (StringExpr s) _ = return $ StringVal s
eval (Call fName exprs) state = execFuncCall fName  exprs state >>= return . retVal

evalBin :: BinExpr -> State -> IO Value
evalBin (AritmExpr a) state = evalAritm a state
evalBin (CondExpr c) state = evalCond c state
evalBin _ _ = error "ToDO"

evalAritm :: AritmExpr -> State -> IO Value
evalAritm (AddExpr l r) state = add <$> (eval l state) <*>(eval r state) 
evalAritm (SubExpr l r) state = sub <$> (eval l state) <*> (eval r state) 
evalAritm (MulExpr l r) state = mul <$> (eval l state) <*> (eval r state) 
evalAritm (DivExpr l r) state = div_ <$> (eval l state) <*> (eval r state) 

evalCond :: CondExpr -> State -> IO Value
evalCond (Neq l r) = evalCond2 (==) l r
evalCond (Eq_ l r) = evalCond2 (==) l r
evalCond (Less l r) = evalCond2 (<) l r
evalCond (LessEq l r) = evalCond2 (<=) l r
evalCond (Greater l r) = evalCond2 (>) l r
evalCond (GreaterEq l r) = evalCond2 (>=) l r

evalCond2 f l r state = do
    b <- f <$> (eval l state) <*> (eval r state)
    return $ BoolVal b

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
