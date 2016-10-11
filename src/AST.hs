module AST where

type Name = String

data Block = Block [Statement]
           deriving (Eq, Show)

data Type = TypeName TypeName 
          | TypeLit TypeLit
          | Type Name
           deriving (Eq, Show)

data TypeName = TypNameIdentifier Name
              | TypeNameQualifiedIdent QualifiedIdent
              deriving (Eq, Show)

data QualifiedIdent = QualifiedIdent PackageName Name
              deriving (Eq, Show)
type PackageName = Name
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

data Declaration = ConstDecl [IdDecl] Type [Expr]  
                 | TypeDecl Name Type 
                 | VarDecl [IdDecl] Type [Expr]
                  deriving (Eq, Show)

type IdentifierList = [Name]

type ExpressionList = [Expr]

data Expr =  BinExpr BinExpr
		  | Call Name [Expr]
          | Num Int
          | IdUse Name
           deriving (Eq, Show)

data BinExpr = AddExpr Expr Expr
             | SubExpr Expr Expr
             | MulExpr Expr Expr
             | DivExpr Expr Expr
             | ModExpr Expr Expr
            deriving (Eq, Show)

data IdDecl = IdDecl Name
            deriving (Eq, Show)
