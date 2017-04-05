module AST where

import Data.Char (toLower)


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

data QualifiedIdent = QualifiedIdent Name Name
              deriving (Eq, Show)

data TypeLit = ArrayTypeLit ArrayType
             | StructTypeLit StructType
             | PointerTypeLit PointerType
             | FunctionTypeLit FunctionType
             | InterfaceTypeLit InterfaceType
             | SliceTypeLit SliceType
             | MapTypeLit MapType
             | ChannelTypeLit ChannelType
             deriving (Eq, Show)

data ArrayType = ArrayType ArrayLength ElementType
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

data Signature = Signature1 [ParameterDecl]
               | Signature2 [ParameterDecl] Result
               deriving (Eq, Show)

type FunctionBody = Block

data Result = Result1 [ParameterDecl]
            | Result2 Type
            deriving (Eq, Show)

data ParameterDecl = ParameterDecl1 Type
                   | ParameterDecl2 [IdDecl] Type
                   deriving (Eq, Show)

data MethodDecl = MethodDecl1 Receiver MethodName Signature FunctionBody
                | MethodDecl2 Receiver MethodName Signature
                deriving (Eq, Show)

type Receiver = [ParameterDecl]

data Declaration = ConstDecl [IdDecl] Type [Expr]  
                 | TypeDecl [TypeSpec]
                 | VarDecl [IdDecl] Type [Expr]
                  deriving (Eq, Show)

data TypeSpec = TypeSpec Name Type
               deriving (Eq, Show)

type IdentifierList = [Name]

type ExpressionList = [Expr]


data StructType = StructType1 [FieldDecl]
                | StructType2
                deriving (Eq, Show)

data FieldDecl = FieldDecl1 [IdDecl] Type
               | FieldDecl2 [IdDecl] Type Tag
               | AnonFieldDecl1 AnonFieldType 
               | AnonFieldDecl2 AnonFieldType Tag
               deriving (Eq, Show)

data AnonFieldType = AnonFieldType1 TypeName
                   | AnonFieldType2 TypeName
                   deriving (Eq, Show)

type Tag = String

data Value = IntVal Int
     | BoolVal Bool
     | StringVal String
     | NullVal
     deriving (Ord, Eq)

instance Show Value where
    show (IntVal num) = show num
    show (BoolVal bool) = map toLower $ show bool
    show (StringVal string) = show string
    show (NullVal) = "Null"

{-
data Expr = BinExpr BinExpr
          | PrintCall [Expr]
          | PrintLnCall [Expr]
          | Call Name [Expr]
          | BoolExpr Bool 
          | Num Int
          | IdUse Name
          | StringExpr String
          deriving (Eq, Show)
-}
data Expr = UnaryExpr UnaryExpr
          | BinExpr BinExpr
          | BoolExpr Bool -- not in spec, should be constant instead of expr
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

data UnaryExpr = PrimaryExpr PrimaryExpr
               | PosExpr UnaryExpr
               | NegExpr UnaryExpr
               | BoolNegExpr UnaryExpr
               | UpArrowExpr UnaryExpr
               | StarExpr UnaryExpr
               | RefExpr UnaryExpr
               | LeftArrowExpr UnaryExpr
               deriving (Eq, Show)

data PrimaryExpr = PrimaryExpr1 Operand
                 | PrimaryExpr2 Conversion
                 | PrimaryExpr3 PrimaryExpr Selector
                 | PrimaryExpr4 PrimaryExpr Index
                 | PrimaryExpr5 PrimaryExpr Slice
                 | PrimaryExpr6 PrimaryExpr TypeAssertion
                 | PrimaryExpr7 PrimaryExpr Arguments
                 deriving (Eq, Show)

data Conversion = Conversion Type Expr
                deriving (Eq, Show)

data Operand = Operand1 Literal
             | Operand2 OperandName
             | Operand3 MethodExpr
             | Operand4 Expr
             deriving (Eq, Show)

data OperandName = OperandName1 String
                 | OperandName2 QualifiedIdent
                 deriving (Eq, Show)

data MethodExpr = MethodExpr ReceiverType MethodName
                deriving (Eq, Show)

data ReceiverType = ReceiverType1 TypeName
                  | ReceiverType2 TypeName
                  | ReceiverType3 ReceiverType
                  deriving (Eq, Show)

data Literal = BasicLit BasicLit
             | CompositeLit LiteralType LiteralValue
             | FunctionLit Signature Block
             deriving (Eq, Show)

data BasicLit = IntLit Int
              | StringLit String
              deriving (Eq, Show)

data LiteralType = LiteralType1 StructType
                 | LiteralType2 ArrayType
                 | LiteralType3 ElementType
                 | LiteralType4 SliceType
                 | LiteralType5 MapType
                 | LiteralType6 TypeName
                 deriving (Eq, Show)

data LiteralValue = LiteralValue1
                  | LiteralValue2 [KeyedElement]
                  deriving (Eq, Show)

data KeyedElement = KeyedElement1 Element
                  | KeyedElement2 Key Element
                  deriving (Eq, Show)

data Key = Key1 FieldName
         | Key2 Expr
         | Key3 LiteralValue
         deriving (Eq, Show)
             
type FieldName = String

data Element = Element1 Expr
             | Element2 LiteralValue
             deriving (Eq, Show)

data Selector = Selector Name
              deriving (Eq, Show)

data Index = Index Expr
           deriving (Eq, Show)

data Slice = Slice1 Expr Expr
           | Slice2 Expr
           | Slice3 Expr
           | Slice4
           | Slice5 Expr Expr Expr
           | Slice6 Expr Expr
           deriving (Eq, Show)

data TypeAssertion = TypeAssertion Type
                   deriving (Eq, Show)

data Arguments = Arguments1
               | Arguments2 [Expr]
               | Arguments3 [Expr]
               | Arguments4 [Expr]
               | Arguments5 [Expr]
               | Arguments6 Type [Expr]
               | Arguments7 Type [Expr]
               | Arguments8 Type [Expr]
               | Arguments9 Type [Expr] 
               | Arguments10 Type 
               | Arguments11 Type 
               | Arguments12 Type 
               | Arguments13 Type 
               deriving (Eq, Show)








