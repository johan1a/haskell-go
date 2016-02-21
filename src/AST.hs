module AST where

type Id = String

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
          deriving (Eq, Show)
              
       --       | LabeledStmt 
      --        | GoStmt 
     --         | ReturnStmt 
           --   | BreakStmt 
          --    | ContinueStmt 
         --     | GotoStmt 
           --   | FallthroughStmt 
          --    | Block 
         --     | IfStmt 
        --      | SwitchStmt 
       --       | SelectStmt 
      --        | ForStmt 
             -- | DeferStmt 

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

data Op = Op String
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
