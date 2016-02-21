module AST where

type Id = String

data Op = Add | Sub | Mul deriving (Eq,Show)

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
          deriving (Eq, Show)

type Statements = [Statement]

data Declaration = ConstDecl ConstDecl
                 | TypeDecl TypeDecl
                  deriving (Eq, Show)

data ConstDecl = ConstSpec Id Type Expr
                deriving (Eq, Show)

data TypeDecl = TypeSpec Id Type  --id type
                deriving (Eq, Show)

data VarSpec = VarSpec Id Type Expr

data Expr = Abs Id Expr
          | App Statement Expr
          | Num Int
          | Var Id
          | Binop Op Expr Expr
          deriving (Eq, Show)

