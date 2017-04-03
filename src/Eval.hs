module Eval where
import Data.Map (Map)
import qualified Data.Map as Map
import AST
import Data.Maybe
import Debug.Trace
import Control.Applicative


-- Activation record
type ActRec = Map Name Expr

type Values = Map Name Value

data Object = O1 Type Value
            | O2 Type Values
            deriving (Show)

instance Eq Object where
    (==) (O1 _ l) (O1 _ r) = l == r
    (/=) (O1 _ l) (O1 _ r) = l /= r

instance Ord Object where
    (>=) (O1 _ l) (O1 _ r) = l >= r
    (>) (O1 _ l) (O1 _ r) = l > r
    (<=) (O1 _ l) (O1 _ r) = l <= r
    (<) (O1 _ l) (O1 _ r) = l < r



    

data State = State { actRecs :: [ActRec], --TODO types
                     params  :: Map Name [Name],
                     funcs   :: Map Name FunctionDecl,
                     types   :: Map Name Type,
                     vars    :: Map Name Object, 
                     callStack :: [String],
                     retVal  :: Object,
                     emitter ::  String -> IO()
                   } 

-- Returns the activation record of the current scope
decls :: State -> ActRec
decls state = traceShow "decls" head $ actRecs state


currentFunc :: State -> String
currentFunc state = case func of
                    [] -> error "There is no current function. This should not be possible"
                    ss -> head ss
                    where func = callStack state

emptyState :: State
emptyState = State { actRecs = [Map.empty], 
                params = Map.empty,
                funcs = Map.empty, 
                callStack = ["TOPLEVEL"],
                vars = Map.empty,
                retVal = O1 (Type "nulltype") NullVal, 
                emitter = (putStr )
}

testState :: String -> State
testState outFile = emptyState { emitter = (appendFile outFile) }

-- Runs the program and writes output to a standard out
runProgram :: SourceFile -> IO State
runProgram (SourceFile package dd) = do
    newState <- readTopLevelDecls dd emptyState 
    runMain $   newState

-- Runs the program and writes output to a file
runTestProgram :: String -> SourceFile -> IO State
runTestProgram outFile (SourceFile package tDecls) = readTopLevelDecls tDecls (testState outFile) >>= runMain

runMain :: State -> IO State
runMain = execFuncCall "main" []

getFuncDecl :: FunctionName -> State -> FunctionDecl
getFuncDecl name state = fRetOrFail name $ Map.lookup name $ funcs state


fRetOrFail name Nothing = error $ "Error: Could not find function " ++ name
fRetOrFail name (Just x) = x

readTopLevelDecls :: [TopLevelDecl] -> State -> IO State
readTopLevelDecls [] state = return state
readTopLevelDecls (s:ss) state = readTopLevelDecl s state >>= readTopLevelDecls ss 

readTopLevelDecl :: TopLevelDecl -> State -> IO State
readTopLevelDecl (TopLevelDecl1 decl) state = return $ storeDecl decl state
readTopLevelDecl (TopLevelDecl2 fDecl) state = return $ storeFuncDecl fDecl state

--TODO rename
storeDecl :: Declaration -> State -> State
storeDecl (ConstDecl idDecls type_ exprs) state = bindDecls idDecls type_ exprs state 
storeDecl (TypeDecl typeSpecs) state = bindTypeSpecs typeSpecs state
storeDecl (VarDecl idDecls type_ exprs) state = bindDecls idDecls type_ exprs state 

bindTypeSpecs :: [TypeSpec] -> State  -> State
bindTypeSpecs [] s = s
bindTypeSpecs (x:xs) s = bindTypeSpecs xs $ bindTypeSpec x s

bindTypeSpec :: TypeSpec -> State -> State
bindTypeSpec (TypeSpec n t) s = s { types = Map.insert n t (types s)}


bindDecls :: [IdDecl] -> Type -> [Expr] -> State -> State
bindDecls [] type_ [] state = state
bindDecls [] type_ _ state = error "mismatch1 TODO error?"
bindDecls _ type_ [] state = error "mismatch2 TODO error?"
bindDecls (d:dd) type_ (e:ee) state = bindDecls dd type_ ee $ bindDecl d type_ e state
    where b = "bound decl: " ++ (show d) ++ " " ++ (show e)


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
sParams (Signature1 paramDecls ) = paramDecls >>= paramDeclNames 
sParams (Signature2 paramDecls _ ) = paramDecls >>= paramDeclNames 

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
declName (TypeDecl typeSpecs ) = error "declName"
declName (VarDecl idDecls _ _) = idDeclName $ idDecls !! 0

readDeclaration :: Declaration -> State -> IO State
readDeclaration decl state = error "readdeclaration TODO"




-- Execution below

execStmts :: [Statement] -> State -> IO State
execStmts [] state = return state
execStmts (s:ss) state = execStmt s state >>= execStmts ss 

execStmt :: Statement -> State -> IO State
execStmt (DeclarationStmt decl) st = return $ execDecl decl st
execStmt (SimpleStmt simpleStmt) st = execSimpleStmt simpleStmt st
execStmt (BlockStmt block) st = execBlock block st
execStmt (IfStmt ifStmt) st = execIfStmt ifStmt st
execStmt (ReturnStmt expr) st = do
    val <- eval expr st        
    return $ st { retVal = val }

--TODO implement types, multiple declarations
execDecl :: Declaration -> State -> State
execDecl (ConstDecl idDecls type_ exprs) = bindExpr (getName $ idDecls !! 0) ( (exprs !! 0) )  
execDecl (TypeDecl typeSpecs) = bindTypeSpecs typeSpecs 
execDecl (VarDecl idDecls type_ exprs) = bindExpr (getName $ idDecls !! 0) ( (exprs !! 0) ) 

execSimpleStmt :: SimpleStmt -> State -> IO State
execSimpleStmt (Assignment a) = return . execAssign a
execSimpleStmt EmptyStmt = return 
execSimpleStmt (ExpressionStmt expr) = execExprStmt expr
execSimpleStmt (IncDecStmt stmt) = execIncDecStmt stmt 
execSimpleStmt (ShortVarDecl dd exprs) = execShortVarDecl dd exprs

execIncDecStmt :: IncDecStmt -> State -> IO State
execIncDecStmt (IncStmt expr) = error "execincdecstmt TODO" 
execIncDecStmt (DecStmt expr) = error "incdedc TODO" 

execShortVarDecl :: [IdDecl] -> [Expr] -> State -> IO State
execShortVarDecl dd exprs s = do
    obj <- (eval (head exprs ) s)
    bindVar (getName $ head dd) obj s

bindVar :: Name -> Object -> State -> IO State
bindVar n o s = return $ s { vars = Map.insert n o (vars s) }

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


emit :: [Expr] -> String -> State -> IO State
emit e suffix st = do
    v <- eval (e !! 0) st
    (emitter st) $ (show v) ++ suffix
    return st 

--Print multiple
execExprStmt :: Expr  -> State -> IO State
execExprStmt (UnaryExpr u) = execUnary u
--execExprStmt (Call name e) st = execFuncCall name e st
--execExprStmt (PrintLnCall e) st = emit e "\n" st
--execExprStmt (PrintCall e) st = emit e "" st 
--execExprStmt (BoolExpr b) st = error "error bool"
--execExprStmt (Num n) st = error "error num" 
--execExprStmt (IdUse id) st = error $  "error id: " ++ id
--execExprStmt (StringExpr str) st = error "error: str"
execExprStmt e = error $ traceShow e "execExprStmt"

execUnary :: UnaryExpr -> State -> IO State
execUnary (PrimaryExpr p) = execPrimary p

execPrimary :: PrimaryExpr -> State -> IO State
execPrimary (PrimaryExpr1 o) = execOperand o
execPrimary (PrimaryExpr7 primary args) = execPrimaryFuncCall primary args

execPrimaryFuncCall :: PrimaryExpr -> Arguments -> State -> IO State
execPrimaryFuncCall (PrimaryExpr1 (Operand2 (OperandName2 (QualifiedIdent p n)))) args 
    | p == "fmt" = execFmtFunction n args
execPrimaryFuncCall (PrimaryExpr1 (Operand2 (OperandName1 name))) args =
    execFuncCall name (getExprsFromArgs args)
    
execPrimaryFuncCall x args = error $ ("215 " ++ (show x) ++ " - " ++ (show args))

execFmtFunction name (Arguments5 exprs) 
    | name == "Println" = emit exprs "\n"
    | name == "Print" = emit exprs ""
    | otherwise = error $ "fmt func: " ++ name
execFmtFunction name args = error $ "fmt args: " ++ (show args) 


execOperand :: Operand -> State -> IO State
execOperand (Operand1 lit) = execLit lit
execOperand (Operand2 name) = error "execop2"
execOperand (Operand3 methodExpr) = error "execop3"
execOperand (Operand4 expr) = error "execop4"
execOperand op = error "execOp"

execLit :: Literal -> State -> IO State
execLit (BasicLit lit) s= return s-- TODO should we evaluate the literal here?
execLit (CompositeLit typ val) s= bindComposite typ val s 
execLit (FunctionLit sig block) s= return s-- TODO should we evaluate the literal here?

bindComposite :: LiteralType -> LiteralValue -> State -> IO State
--bindComposite (LiteralType1 (StructType1 [FieldDecl])) (LiteralValue2 elements) s =
bindComposite lt lv s = error $ (show lt) ++ " " ++ (show lv) 
    
    

evalFuncCall :: String -> Arguments -> State -> IO Object
evalFuncCall funcName args state = do 
    s <- execFuncCall funcName (getExprsFromArgs args) state 
    return $ retVal s


getExprsFromArgs :: Arguments -> [Expr]
getExprsFromArgs Arguments1 = []
getExprsFromArgs (Arguments2 exprs) = exprs
getExprsFromArgs (Arguments3 exprs) = exprs
getExprsFromArgs (Arguments4 exprs) = exprs
getExprsFromArgs (Arguments5 exprs) = exprs


execFuncCall :: Name -> [Expr] -> State -> IO State
execFuncCall name args state = execFuncDecl fDecl newState
    where newState = bindArgs name args state
          fDecl = getFuncDecl name state

execFuncDecl :: FunctionDecl -> State -> IO State
execFuncDecl (FunctionDecl1 _ _ ) st = error "No function body!"
execFuncDecl (FunctionDecl2 name sig body) st = execBlock body st {callStack = [name] ++ (callStack st)}
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
bindAssign lhs rhs state = bindExpr (getExprName lhs) rhs state 
bindAssign _ rhs state = error "bindassign TODO"

getName (IdDecl name) = name

getExprName (UnaryExpr (PrimaryExpr (PrimaryExpr1 (Operand2 (OperandName1 name))))) = name

--Bind the given arguments to the formal parameter names of the function
bindArgs :: Name -> [Expr] -> State -> State
bindArgs funcName exprs state = bindArgs2 (fParams $ getFuncDecl funcName state) exprs $  ( state { actRecs = [Map.empty] ++ (actRecs state)})

bindArgs2 :: [Name] -> [Expr] -> State-> State
bindArgs2 [] [] state = state
bindArgs2 (n:nn) (a:aa) state = (bindArg n a state)
bindArgs2 _ _ state = state

--TODO change to bindValue instead
bindExpr :: Name -> Expr -> State -> State
bindExpr name expr state = state { actRecs =( [Map.insert name (lookupExpr expr state) $ decls state ] ++ (tail $ actRecs state))}

bindArg :: Name -> Expr -> State -> State
bindArg name expr state = state { actRecs = [Map.insert name (lookupExpr expr state) $ decls state ] ++ (tail $ actRecs state)}

--TODO type
bindDecl :: IdDecl -> Type -> Expr -> State -> State
bindDecl (IdDecl name) type_ expr state = bindExpr name expr state

paramNames :: String -> State -> [String]
paramNames funcName state = pRetOrFail funcName $ Map.lookup funcName $ params state


pRetOrFail funcName Nothing = error "Could not find function" funcName
pRetOrFail funcName (Just val) = val 

--lookup paramexpr, upp en nivå

-- If given an IdUse, it tries to find what expression is actually referenced
lookupExpr :: Expr -> State -> Expr
lookupExpr (UnaryExpr (PrimaryExpr (PrimaryExpr1 (Operand2 (OperandName1 name))))) s = lookupIdUse name s
lookupExpr x@(UnaryExpr (PrimaryExpr (PrimaryExpr1 (Operand1 (BasicLit (IntLit n)))))) s = x
--lookupExpr (IdUse name) state = lookupIdUse name state
--lookupExpr (Num n) state = (Num n)
lookupExpr (BinExpr e ) state = lookupBinExpr e state
--lookupExpr (BoolExpr b) _ = (BoolExpr b)
--lookupExpr (StringExpr s) _ = (StringExpr s)
--lookupExpr (Call fName exprs) state = (Call fName (map (\x -> lookupExpr x state) exprs))  
lookupExpr x@(UnaryExpr (PrimaryExpr (PrimaryExpr7 (PrimaryExpr1 (Operand2 (OperandName1 funcName))) args))) s = x
lookupExpr (UnaryExpr (PrimaryExpr (PrimaryExpr1 (Operand4 expr)))) s = lookupExpr expr s
lookupExpr x@(UnaryExpr (PrimaryExpr (PrimaryExpr1 (Operand1 (BasicLit lit))))) s = x
lookupExpr b@(BoolExpr _) s = b
lookupExpr expr state = error $ traceShow expr "lookupExpr " --expr


lookupIdUse :: String -> State -> Expr
lookupIdUse "true" state = BoolExpr True
lookupIdUse "false" state = BoolExpr False
lookupIdUse (name) state = lookupIdRef name found state
    where found = (Map.lookup name $ decls state)

makeIdUse name = UnaryExpr (PrimaryExpr (PrimaryExpr1 (Operand2 (OperandName1 name))))

-- Lookup an Expr referenced by the IdUse
lookupIdRef :: String -> Maybe Expr -> State -> Expr
lookupIdRef name (Just expr) state = lookupExpr expr state
lookupIdRef name Nothing state
    | canContinue = lookupExpr (makeIdUse name) (scopeAbove state) 
    | otherwise = error $ "undefined: " ++ name
    where canContinue = (not atTopLevel) && (isParameter || inMainFunc)
          isParameter = (isParam name state) 
          inMainFunc = (currentFunc state) == "main"
          atTopLevel = (currentFunc state) == "TOPLEVEL" -- TODO refactor

isParam :: Name -> State -> Bool
isParam name state = elem name $ fRetOrFail (currentFunc state) $ Map.lookup (currentFunc state ) $  params state

scopeAbove :: State -> State
scopeAbove state = state { actRecs = (tail $ actRecs state), callStack = (tail $ callStack state)}

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


evalBool :: Expr -> State -> IO Bool --TODO return state?
evalBool expr state = fmap asBoolVal (eval expr state)

asBoolVal :: Object -> Bool
asBoolVal (O1 _ (BoolVal v)) = v
asBoolVal (O1 _ (IntVal _)) = error "not a bool"
asBoolVal (O1 _ (StringVal _)) = error "not a bool"

eval :: Expr -> State -> IO Object
--eval (IdUse name) state = eval (lookupExpr (IdUse name) state) state
eval (BinExpr e ) state = evalBin e state
--eval (Num n) _ = return $ IntVal n 
eval (BoolExpr b) _ = return $ O1 boolType (BoolVal b)
--eval (StringExpr s) _ = return $ StringVal s
--eval (Call fName exprs) state = execFuncCall fName  exprs state >>= return . retVal
eval (UnaryExpr ue) s =  evalUnary ue s
eval e s = error $ "eval2: " ++ (show e)

evalUnary :: UnaryExpr -> State -> IO Object
evalUnary (PrimaryExpr pe) = evalPrimary pe

evalPrimary :: PrimaryExpr -> State -> IO Object
evalPrimary (PrimaryExpr1 op) = evalOperand op
evalPrimary (PrimaryExpr7 (PrimaryExpr1 (Operand2 (OperandName1 funcName))) args) = evalFuncCall funcName args



evalOperand :: Operand -> State -> IO Object
evalOperand (Operand1 lit) s = evalLiteral lit s
evalOperand (Operand2 (opName)) s = evalOperandName opName s
evalOperand (Operand3 methodExpr) s = error "evalOperand1"
evalOperand (Operand4 expr) s = eval expr s

evalOperandName :: OperandName -> State -> IO Object
evalOperandName (OperandName1 name) s = eval ( lookupIdUse name s) s
evalOperandName (OperandName2 qualifiedIdent) s = error $ show qualifiedIdent

evalLiteral :: Literal -> State -> IO Object
evalLiteral (BasicLit bl) = evalBasicLit bl
evalLiteral x = error $ "evalLiteral "  ++ ( show x)

evalBasicLit :: BasicLit -> State -> IO Object
evalBasicLit (IntLit n) s = return $ (O1 intType (IntVal n))
evalBasicLit (StringLit str) s = return $ O1 strType (StringVal str)

evalBin :: BinExpr -> State -> IO Object
evalBin (AritmExpr a) state = evalAritm a state
evalBin (CondExpr c) state = evalCond c state

evalAritm :: AritmExpr -> State -> IO Object
evalAritm (AddExpr l r) state = add <$> (eval l state) <*>(eval r state) 
evalAritm (SubExpr l r) state = sub <$> (eval l state) <*> (eval r state) 
evalAritm (MulExpr l r) state = mul <$> (eval l state) <*> (eval r state) 
evalAritm (DivExpr l r) state = div_ <$> (eval l state) <*> (eval r state) 

evalCond :: CondExpr -> State -> IO Object
evalCond (Neq l r) = evalCond2 (==) l r
evalCond (Eq_ l r) = evalCond2 (==) l r
evalCond (Less l r) = evalCond2 (<) l r
evalCond (LessEq l r) = evalCond2 (<=) l r
evalCond (Greater l r) = evalCond2 (>) l r
evalCond (GreaterEq l r) = evalCond2 (>=) l r





intType :: Type
intType = Type "int"

strType :: Type
strType = Type "string"

boolType :: Type
boolType = Type "bool"


evalCond2 f l r state = do
    left <- eval l state
    right <- eval r state
    return $ (O1 boolType (BoolVal (f left right)))

add :: Object -> Object -> Object
add (O1 _ (IntVal l)) (O1 _ (IntVal r)) = (O1 intType (IntVal (l + r)))
add _ _ = error "todo add"

sub :: Object -> Object -> Object
sub (O1 _ (IntVal l)) (O1 _ ( IntVal r)) = (O1 intType (IntVal (l - r)))
sub _ _ = error "todo sub"

mul :: Object -> Object -> Object
mul (O1 _ (IntVal l)) (O1 _ (IntVal r)) = (O1 intType (IntVal (l * r)))
mul _ _ = error "todo mul "

div_ :: Object -> Object -> Object
--div (Num l) (Num r) = (Num (l / r))
div_  _ _ = error "todo div"

{-
TODO add tests for  
eval empty (Num 5) : IntVal 5

-}
