module AST where


type Name = String

data SourceFile = SourceFile Package [TopLevelDecl]
                deriving (Eq, Show)

data Package = Package String
             deriving (Eq, Show)

data TopLevelDecl = TopLevelDecl1 Declaration 
                  | TopLevelDecl2 FunctionDecl 
                  | TopLevelDecl3 MethodDecl 
                  deriving (Eq, Show)


data Block = Block [Statement]
           deriving (Eq, Show)

data Type = TypeName TypeName 
          | TypeLit TypeLit
          | Type Name
           deriving (Eq, Show)

data TypeName = TypeNameIdentifier Name
              | TypeNameQualifiedIdent QualifiedIdent
              deriving (Eq, Show)

data QualifiedIdent = QualifiedIdent PackageName Name
              deriving (Eq, Show)

type PackageName = Name

data TypeLit = ArrayType ArrayLength ElementType
             | StructTypeLit StructType
             | PointerTypeLit PointerType
             | FunctionTypeLit FunctionType
             | InterfaceTypeLit InterfaceType
             | SliceTypeLit SliceType
             | MapTypeLit MapType
             | ChannelTypeLit ChannelType
             deriving (Eq, Show)

type ArrayLength = Expr
type ElementType = Type

data PointerType = PointerType Type
                 deriving (Eq, Show)

data FunctionType = FunctionType Signature
                  deriving (Eq, Show)

data InterfaceType = InterfaceType [MethodSpec]
                   deriving (Eq, Show)

data MethodSpec = MethodSpec1 MethodName Signature
                | MethodSpec2 InterfaceTypeName
                deriving (Eq, Show)

type MethodName = String

data SliceType = SliceType ElementType
               deriving (Eq, Show)

data MapType = MapType KeyType ElementType
              deriving (Eq, Show)

type KeyType = Type

data ChannelType = ChannelType1 ElementType 
                 | ChannelType2 ElementType
                 | ChannelType3 ElementType
                 deriving (Eq, Show)

type InterfaceTypeName = TypeName

-- TODO duplicate expression stmt?
data Statement =  DeclarationStmt Declaration
               | SimpleStmt SimpleStmt
       --       | LabeledStmt 
      --        | GoStmt 
               | ReturnStmt Expr
           --   | BreakStmt 
          --    | ContinueStmt 
         --     | GotoStmt 
           --   | FallthroughStmt 
               | BlockStmt Block 
               | IfStmt IfStmt
        --      | SwitchStmt 
       --       | SelectStmt 
      --        | ForStmt 
             -- | DeferStmt 
              deriving (Eq, Show)

-- "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
data IfStmt = Ifstmt1 Expr Block
            | Ifstmt2 Expr Block Else
            | Ifstmt3 SimpleStmt Expr Block 
            | Ifstmt4 SimpleStmt Expr Block Else
            deriving (Eq, Show)

data Else = Else1 IfStmt
          | Else2 Block
          deriving (Eq, Show)
          
data SimpleStmt = EmptyStmt 
                | ExpressionStmt Expr
        --        | SendStmt 
                | IncDecStmt IncDecStmt
                | Assignment Assignment
                | ShortVarDecl [IdDecl] [Expr]  
                deriving (Eq, Show)

data IncDecStmt = IncStmt Expr
                | DecStmt Expr
                deriving (Eq, Show)

data Assignment = Assign [Expr] [Expr]
                | OpAssign Op [Expr] [Expr]
                deriving (Eq, Show)

data Op = AddOp
        | SubOp
        | PipeOp
        | UpOp
        | MulOp
        | DivOp
        | ModOp
        | LeftOp
        | RightOp
        | AmpOp
        | AmpUpOp
        deriving (Eq, Show)

type Statements = [Statement]


data FunctionDecl = FunctionDecl1 FunctionName Signature
                  | FunctionDecl2 FunctionName Signature FunctionBody
                  deriving (Eq, Show)

type FunctionName = String

data Signature = Signature1
               | Signature2 [ParameterDecl]
               | Signature3 [ParameterDecl] Result
               deriving (Eq, Show)

type FunctionBody = Block

data Result = Result1
            | Result2 [ParameterDecl]
            | Result3 Type
            deriving (Eq, Show)

data ParameterDecl = ParameterDecl1 Type
                   | ParameterDecl2 [IdDecl] Type
                   deriving (Eq, Show)

data MethodDecl = MethodDecl1 Receiver MethodName Signature FunctionBody
                | MethodDecl2 Receiver MethodName Signature
                deriving (Eq, Show)

type Receiver = [ParameterDecl]

data Declaration = ConstDecl [IdDecl] Type [Expr]  
                 | TypeDecl Name Type 
                 | VarDecl [IdDecl] Type [Expr]
                  deriving (Eq, Show)

type IdentifierList = [Name]

type ExpressionList = [Expr]


data StructType = StructType1 [FieldDecl]
                | StructType2
                deriving (Eq, Show)

data FieldDecl = FieldDecl1 Type
               | FieldDecl2 Type Tag
               | AnonFieldDecl1 AnonFieldType 
               | AnonFieldDecl2 AnonFieldType Tag
               deriving (Eq, Show)

data AnonFieldType = AnonFieldType1 TypeName
                   | AnonFieldType2 TypeName
                   deriving (Eq, Show)

type Tag = String

data Value = NumVal Int
     | BoolVal Bool
     | StringVal String
     | NullVal
     deriving (Ord, Eq)

instance Show Value where
    show (NumVal num) = show num
    show (BoolVal bool) = show bool
    show (StringVal string) = show string
    show (NullVal) = "Null"

data Expr = BinExpr BinExpr
          | PrintCall [Expr]
          | PrintLnCall [Expr]
          | Call Name [Expr]
          | BoolExpr Bool 
          | Num Int
          | IdUse Name
          | StringExpr String
          deriving (Eq, Show)

data BinExpr = AritmExpr AritmExpr
             | CondExpr CondExpr
             deriving (Eq, Show)
        
data AritmExpr = AddExpr Expr Expr
               | SubExpr Expr Expr
               | MulExpr Expr Expr
               | DivExpr Expr Expr
               | ModExpr Expr Expr
                deriving (Eq, Show)

data CondExpr = Eq_ Expr Expr
          | Neq Expr Expr
          | Less Expr Expr
          | LessEq Expr Expr
          | Greater Expr Expr
          | GreaterEq Expr Expr
            deriving (Eq, Show)

data IdDecl = IdDecl Name
            deriving (Eq, Show)
