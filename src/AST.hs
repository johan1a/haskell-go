module AST where

type Id = String

data Block = Block [Statement]
           deriving (Eq, Show)

data Type = TypeName TypeName 
          | TypeLit TypeLit
          | Type Id
           deriving (Eq, Show)

data TypeName = TypNameIdentifier Id
              | TypeNameQualifiedIdent QualifiedIdent
              deriving (Eq, Show)

data QualifiedIdent = QualifiedIdent PackageName Id
              deriving (Eq, Show)
type PackageName = Id
data TypeLit = ArrayType ArrayLength ElementType
          deriving (Eq, Show)
        --      | StructType ArrayType
        --      | PointerType ArrayType
        --      | FunctionType ArrayType
        --      | InterfaceType ArrayType
       --       | SliceType ArrayType
          --    | MapType ArrayType
           --   | ChannelType ArrayType

type ArrayLength = Expr
type ElementType = Type


data Statement = Expr Expr
              | Declaration Declaration
              | SimpleStmt SimpleStmt
       --       | LabeledStmt 
      --        | GoStmt 
     --         | ReturnStmt 
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
                | ShortVarDecl [Id] [Expr]  
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

data Declaration = ConstDecl ConstDecl
                 | TypeDecl TypeDecl
                 | VarDecl VarSpec
                  deriving (Eq, Show)

data VarSpec     = VarSpec [Id] Type [Expr]
                  deriving (Eq, Show)

type IdentifierList = [Id]

type ExpressionList = [Expr]

data ConstDecl = ConstSpec [Id] Type [Expr]  
                deriving (Eq, Show)

data TypeDecl = TypeSpec Id Type 
                deriving (Eq, Show)


data Expr = Num Int
          | Var Id
          deriving (Eq, Show)

