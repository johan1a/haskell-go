module Eval where
import Data.Map (Map)
import qualified Data.Map as Map
import AST
import Data.Maybe

instance Show Object where
    show (O1 _ v) = show v
    show (O2 _ ff) = show $ Map.elems ff

instance Eq Object where
    (==) (O1 _ l) (O1 _ r) = l == r
    (/=) (O1 _ l) (O1 _ r) = l /= r

instance Ord Object where
    (>=) (O1 _ l) (O1 _ r) = l >= r
    (>) (O1 _ l) (O1 _ r) = l > r
    (<=) (O1 _ l) (O1 _ r) = l <= r
    (<) (O1 _ l) (O1 _ r) = l < r



-- Activation record
type ActRec = Objects

type Objects = Map Name Object

-- e.g. fields of astruct
type Fields = Map Name Object

data Object = O1 Type Value
            | O2 {o2t :: Type, fields :: Fields}

data State = State { actRecs :: [ActRec], --TODO types
                     params  :: Map Name [Name],
                     funcs   :: Map Name FunctionDecl,
                     types   :: Map Name Type,
                     callStack :: [String],
                     retVal  :: Object,
                     emitter ::  String -> IO()
                   } 

-- Returns the activation record of the current scope
vars :: State -> ActRec
vars state = head $ actRecs state


currentFunc :: State -> String
currentFunc state = case func of
                    [] -> error "There is no current function. This should not be possible"
                    ss -> head ss
                    where func = callStack state

emptyState :: State
emptyState = State { actRecs = [Map.empty], 
                params = Map.empty,
                types = Map.empty,
                funcs = Map.empty, 
                callStack = ["TOPLEVEL"],
                retVal = O1 (Type "nulltype") NullVal, 
                emitter = (putStr )
}

testState :: String -> State
testState outFile = emptyState { emitter = (appendFile outFile) }

-- Runs the program and writes output to a standard out
runProgram :: SourceFile -> IO State
runProgram (SourceFile package _ dd) = readTopLevelDecls dd emptyState >>= runMain

-- Runs the program and writes output to a file
runTestProgram :: String -> SourceFile -> IO State
runTestProgram outFile (SourceFile package _ tDecls) = 
    readTopLevelDecls tDecls (testState outFile) >>= runMain

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
readTopLevelDecl (TopLevelDecl1 decl) state = storeDecl decl state
readTopLevelDecl (TopLevelDecl2 fDecl) state = return $ storeFuncDecl fDecl state

--TODO rename
storeDecl :: Declaration -> State -> IO State
storeDecl (ConstDecl idDecls type_ exprs) state = bindDecls idDecls type_ exprs state 
storeDecl (TypeDecl typeSpecs) state = return $ bindTypeSpecs typeSpecs state
storeDecl (VarDecl idDecls type_ exprs) state = bindDecls idDecls type_ exprs state 

bindTypeSpecs :: [TypeSpec] -> State  -> State
bindTypeSpecs [] s = s
bindTypeSpecs (x:xs) s = bindTypeSpecs xs $ bindTypeSpec x s

bindTypeSpec :: TypeSpec -> State -> State
bindTypeSpec (TypeSpec n t) s = s { types = Map.insert n t (types s)}


bindDecls :: [IdDecl] -> Type -> [Expr] -> State -> IO State
bindDecls [] type_ [] state = return state
bindDecls [] type_ _ state = error "mismatch1 TODO error?"
bindDecls _ type_ [] state = error "mismatch2 TODO error?"
bindDecls (d:dd) type_ (e:ee) state = bindDecl d type_ e state >>= bindDecls dd type_ ee 

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
execStmt (DeclarationStmt decl) st = execDecl decl st
execStmt (SimpleStmt simpleStmt) st = execSimpleStmt simpleStmt st
execStmt (BlockStmt block) st = execBlock block st
execStmt (IfStmt ifStmt) st = execIfStmt ifStmt st
execStmt (ReturnStmt expr) st = do
    val <- eval expr st        
    return $ st { retVal = val }

--TODO implement types, multiple declarations
execDecl :: Declaration -> State -> IO State
execDecl (ConstDecl idDecls type_ exprs) = bindExpr (getName $ idDecls !! 0) ( (exprs !! 0) )  
execDecl (TypeDecl typeSpecs) = return . bindTypeSpecs typeSpecs 
execDecl (VarDecl idDecls type_ exprs) = bindExpr (getName $ idDecls !! 0) ( (exprs !! 0) ) 

execSimpleStmt :: SimpleStmt -> State -> IO State
execSimpleStmt (Assignment a) = execAssign a
execSimpleStmt EmptyStmt = return 
execSimpleStmt (ExpressionStmt expr) = execExprStmt expr
execSimpleStmt (IncDecStmt stmt) = execIncDecStmt stmt 
execSimpleStmt (ShortVarDecl dd exprs) = execShortVarDecl dd exprs

execIncDecStmt :: IncDecStmt -> State -> IO State
execIncDecStmt (IncStmt expr) = error "execincdecstmt TODO" 
execIncDecStmt (DecStmt expr) = error "incdedc TODO" 

execShortVarDecl :: [IdDecl] -> [Expr] -> State -> IO State
execShortVarDecl dd exprs s = do
    obj <- (eval ex s)
    bindVar (getName $ head dd) obj s
    where ex = head exprs

execIfStmt :: IfStmt -> State -> IO State
execIfStmt (Ifstmt1 expr block) state = execIfStmt' expr block Nothing state
execIfStmt (Ifstmt2 expr block els) state = execIfStmt' expr block (Just els) state

execIfStmt':: Expr -> Block -> Maybe Else -> State -> IO State
execIfStmt' expr block els state = do
    b <- evalBool expr state
    execIfStmt2 b block els state

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

execExprStmt :: Expr  -> State -> IO State
execExprStmt (UnaryExpr u) = execUnary u
execExprStmt e = error "execExprStmt"

execUnary :: UnaryExpr -> State -> IO State
execUnary (PrimaryExpr p) = execPrimary p

execPrimary :: PrimaryExpr -> State -> IO State
execPrimary (PrimaryExpr1 o) = execOperand o
execPrimary (PrimaryExpr7 primary args) = execPrimaryFuncCall primary args

execPrimaryFuncCall :: PrimaryExpr -> Arguments -> State -> IO State
execPrimaryFuncCall (PrimaryExpr1 (Operand2 (OperandName2 (QualifiedIdent p n)))) args s
    | p == "fmt" = execFmtFunction n args s
execPrimaryFuncCall (PrimaryExpr1 (Operand2 (OperandName1 name))) args s =
    execFuncCall name (getExprsFromArgs args) s

execFmtFunction :: String -> Arguments -> State -> IO State
execFmtFunction name (Arguments5 exprs) 
    | name == "Println" = emit exprs "\n" 
    | name == "Print" = emit exprs "" 
    | otherwise = error $ "fmt func: " ++ name

emit :: [Expr] -> String -> State -> IO State
emit e suffix st = do
    v <- eval (e !! 0) st
    (emitter st) $ (show v) ++ suffix
    return st 

execOperand :: Operand -> State -> IO State
execOperand (Operand1 lit) = execLit lit

execLit :: Literal -> State -> IO State
execLit (BasicLit lit) s= return s-- TODO should we evaluate the literal here?
execLit (CompositeLit typ val) s = bindComposite typ val s 
execLit (FunctionLit sig block) s = return s-- TODO should we evaluate the literal here?

bindComposite :: LiteralType -> LiteralValue -> State -> IO State
bindComposite lt LiteralValue1 s = return s -- No values given

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
execFuncCall name args state = do
    newState <- bindArgs name args state
    execFuncDecl fDecl newState
    where fDecl = getFuncDecl name state

execFuncDecl :: FunctionDecl -> State -> IO State
execFuncDecl (FunctionDecl1 _ _ ) st = error "No function body!"
execFuncDecl (FunctionDecl2 name sig body) st = execBlock body st {callStack = [name] ++ (callStack st)}
-- TODO lookup parameters parameters

execBlock :: Block -> State -> IO State
execBlock (Block []) st = return st
execBlock (Block stmts) st = execStmts stmts st

-- TODO assigns first in lhs to first in rhs.
-- does not yet support multiple declarations at once
execAssign :: Assignment -> State -> IO State
execAssign (Assign lhs rhs) = bindAssign (lhs !! 0) (rhs !! 0) 

bindAssign :: Expr -> Expr -> State -> IO State
bindAssign (UnaryExpr (PrimaryExpr (PrimaryExpr1 (Operand2 (OperandName2 qualIdent))))) rhs state =
   bindField qualIdent rhs state
bindAssign lhs rhs state = bindExpr (getExprName lhs) rhs state 

bindField :: QualifiedIdent -> Expr -> State -> IO State
bindField (QualifiedIdent n1 n2) rhs s = do
    val <- eval rhs s
    let updated = Map.insert n2 val (fields struct)
    let newStruct = struct { fields = updated }
    bindObj n1 newStruct s
    where struct = (lookupIdUse n1 s)
          
-- type Fields = Map Name Object
--O2 Type Fields

getName (IdDecl name) = name

getExprName (UnaryExpr (PrimaryExpr (PrimaryExpr1 (Operand2 (OperandName1 name))))) = name

--Bind the given arguments to the formal parameter names of the function
bindArgs :: Name -> [Expr] -> State -> IO State
bindArgs funcName exprs state = bindArgs2 (fParams $ getFuncDecl funcName state) exprs $  ( state { actRecs = [Map.empty] ++ (actRecs state)})

bindArgs2 :: [Name] -> [Expr] -> State-> IO State
bindArgs2 [] [] state = return state
bindArgs2 (n:nn) (a:aa) state = (bindArg n a state)
bindArgs2 _ _ state = return state


bindVar :: Name -> Object -> State -> IO State
bindVar n o s = return $ s { actRecs = ( [Map.insert n o $ vars s ] ++ (tail $ actRecs s))}

--TODO change to bindValue instead
bindExpr :: Name -> Expr -> State -> IO State
bindExpr name expr state = do
    obj <- eval expr state
    bindVar name obj state

bindArg :: Name -> Expr -> State -> IO State
bindArg  = bindExpr

--TODO type
bindDecl :: IdDecl -> Type -> Expr -> State -> IO State
bindDecl (IdDecl name) type_ expr state = bindExpr name expr state

paramNames :: String -> State -> [String]
paramNames funcName state = pRetOrFail funcName $ Map.lookup funcName $ params state


pRetOrFail funcName Nothing = error "Could not find function" funcName
pRetOrFail funcName (Just val) = val 

--TODO refactor booleans
lookupIdUse :: String -> State -> Object
lookupIdUse "true" s = O1 (Type "bool") (BoolVal True)
lookupIdUse "false" s = O1 (Type "bool") (BoolVal False)
lookupIdUse name state = lookupIdRef name found state
    where found = Map.lookup name $ vars state

makeIdUse name = UnaryExpr (PrimaryExpr (PrimaryExpr1 (Operand2 (OperandName1 name))))

-- Lookup an Object referenced by the IdUse
lookupIdRef :: String -> Maybe Object -> State -> Object
lookupIdRef name (Just o) state = o
lookupIdRef name Nothing state 
    | canContinue = lookupIdUse name (scopeAbove state) 
    | otherwise = error $ "undefined id: " ++ name
    where canContinue = (not atTopLevel) && (isParameter || inMainFunc)
          isParameter = (isParam name state) 
          inMainFunc = (currentFunc state) == "main"
          atTopLevel = (currentFunc state) == "TOPLEVEL" -- TODO refactor

bindObj :: Name -> Object -> State -> IO State
bindObj n o s = case found of 
    (Just _) -> bindVar n o s
    Nothing -> do 
        upper <- upperState 
        return $ s { actRecs = ([vars s] ++ (actRecs upper))}
    where found = Map.lookup n $ vars s
          upperState = bindObj n o (scopeAbove s) 

isParam :: Name -> State -> Bool
isParam name state = elem name $ fRetOrFail (currentFunc state) $ Map.lookup (currentFunc state ) $  params state

scopeAbove :: State -> State
scopeAbove state = state { actRecs = (tail $ actRecs state), callStack = (tail $ callStack state)}



evalBool :: Expr -> State -> IO Bool --TODO return state?
evalBool expr state = fmap asBoolVal (eval expr state)

asBoolVal :: Object -> Bool
asBoolVal (O1 _ (BoolVal v)) = v
asBoolVal (O1 _ (IntVal _)) = error "not a bool"
asBoolVal (O1 _ (StringVal _)) = error "not a bool"

eval :: Expr -> State -> IO Object
eval (BinExpr e ) state = evalBin e state
eval (BoolExpr b) _ = return $ O1 boolType (BoolVal b)
eval (UnaryExpr ue) s =  evalUnary ue s

evalUnary :: UnaryExpr -> State -> IO Object
evalUnary (PrimaryExpr pe) s = evalPrimary pe s
evalUnary (BoolNegExpr expr) s = do 
    ev <- evalBool (UnaryExpr expr) s
    return $ (O1 boolType (BoolVal (not $ ev)))
evalUnary x s = error $ "asdfgh: " ++ (show x)

evalPrimary :: PrimaryExpr -> State -> IO Object
evalPrimary (PrimaryExpr1 op) s = evalOperand op s
evalPrimary (PrimaryExpr7 (PrimaryExpr1 (Operand2 (OperandName1 funcName))) args) s = evalFuncCall funcName args s

evalOperand :: Operand -> State -> IO Object
evalOperand (Operand1 lit) s = evalLiteral lit s
evalOperand (Operand2 (opName)) s = evalOperandName opName s
evalOperand (Operand4 expr) s = eval expr s

evalOperandName :: OperandName -> State -> IO Object
evalOperandName (OperandName1 name) s = return $ lookupIdUse name s

-- TODO check if n1 is package or object
evalOperandName (OperandName2 (QualifiedIdent n1 n2)) s = 
    let obj = lookupIdUse n1 s
        attr = field n2 $ obj   
    in return attr

-- TODO throw error if not found
field :: Name -> Object -> Object
field name (O2 t ff) = case found of 
    (Just o) -> o
    Nothing -> error $ "Error: field '" ++ name ++ "' not found in type " ++ (show t) ++ " vals: " ++ (show ff)
    where found = Map.lookup name ff

evalLiteral :: Literal -> State -> IO Object
evalLiteral (BasicLit bl) s = evalBasicLit bl s
evalLiteral (CompositeLit (LiteralType6 (TypeNameIdentifier name)) (LiteralValue2 ee))  s
 = instantiate name ee s

instantiate :: Name -> [KeyedElement] -> State -> IO Object
instantiate typeName ee s = do
    e <- (mapElements type_ ee s)
    return (O2 type_ e)
    where type_ = fromJust $ (Map.lookup typeName $ types s)

mapElements :: Type -> [KeyedElement] -> State -> IO Fields
mapElements (TypeLit lit) ee = mapElementsLit lit ee

mapElementsLit :: TypeLit -> [KeyedElement] -> State -> IO Fields
mapElementsLit (StructTypeLit struct) ee = mapElementsStruct struct ee

mapElementsStruct :: StructType -> [KeyedElement] -> State -> IO Fields
mapElementsStruct (StructType1 dd) ee s = bindFieldDecls dd ee s Map.empty 

bindFieldDecls :: [FieldDecl] -> [KeyedElement] -> State -> Fields -> IO Fields
bindFieldDecls [] [] s f = return f
bindFieldDecls (d:dd) (e:ee) s f = bindFieldDecl d e s f >>= bindFieldDecls dd ee s  

-- TODO multiple idDecls
bindFieldDecl :: FieldDecl -> KeyedElement -> State -> Fields -> IO Fields
bindFieldDecl (FieldDecl1 idDecls type_) (KeyedElement2 k e) s f = bindKeyedElement f (head idDecls) type_ k e s
bindFieldDecl (FieldDecl1 idDecls type_) (KeyedElement1 e) s f = bindKeyedElement2 f (head idDecls) type_ e s

-- TODO types and names should match
-- TODO should return state and Fields?
bindKeyedElement :: Fields -> IdDecl -> Type -> Key -> Element -> State -> IO Fields
bindKeyedElement v (IdDecl name) t (Key1 fieldName) (Element1 expr) s = do
    ev <- eval expr s
    return $ Map.insert name ev v
--TODO parser should parse to Key1 if just identifier
bindKeyedElement f d t (Key2 (UnaryExpr(PrimaryExpr (PrimaryExpr1 (Operand2 (OperandName1 fName)))))) (Element1 expr) s = insertField fName expr f s

bindKeyedElement2 :: Fields -> IdDecl -> Type -> Element -> State -> IO Fields
bindKeyedElement2 f (IdDecl name) t (Element1 expr) s = insertField name expr f s 

insertField :: String -> Expr -> Fields -> State -> IO Fields
insertField fieldName expr ff s = do
    ev <- eval expr s
    return $ Map.insert fieldName ev ff 
    
evalBasicLit :: BasicLit -> State -> IO Object
evalBasicLit (IntLit n) s = return $ (O1 intType (IntVal n))
evalBasicLit (StringLit str) s = return $ O1 strType (StringVal str)
evalBasicLit (FloatLit flt) s = return $ O1 floatType (FloatVal flt)

evalBin :: BinExpr -> State -> IO Object
evalBin (AritmExpr a) state = evalAritm a state
evalBin (CondExpr c) state = evalCond c state

evalAritm :: AritmExpr -> State -> IO Object
evalAritm (AddExpr l r) state = add <$> (eval l state) <*>(eval r state) 
evalAritm (SubExpr l r) state = sub <$> (eval l state) <*> (eval r state) 
evalAritm (MulExpr l r) state = mul <$> (eval l state) <*> (eval r state) 
evalAritm (DivExpr l r) state = div_ <$> (eval l state) <*> (eval r state) 

evalCond :: CondExpr -> State -> IO Object
evalCond (Neq l r) s = evalCond2 (==) l r s
evalCond (Eq_ l r) s = evalCond2 (==) l r s
evalCond (Less l r) s = evalCond2 (<) l r s
evalCond (LessEq l r) s = evalCond2 (<=) l r s
evalCond (Greater l r) s = evalCond2 (>) l r s
evalCond (GreaterEq l r) s = evalCond2 (>=) l r s
evalCond (And l r) s = evalBoolOp (&&) l r s
evalCond (Or l r) s = evalBoolOp (||) l r s


evalBoolOp f l r s = do
    lBool <- evalBool l s
    rBool <- evalBool r s
    return $ O1 boolType (BoolVal (f lBool rBool))

intType :: Type
intType = Type "int"

floatType :: Type
floatType = Type "int"

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
add (O1 _ (StringVal l)) (O1 _ (StringVal r)) = O1 strType (StringVal (l ++ r))
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
