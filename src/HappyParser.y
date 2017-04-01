{
module HappyParser where

import AlexToken
import AST

import Control.Exception
import Data.Typeable
}

%name parse
%monad { IO }
%tokentype { Lexeme Token }
%error { parseError }

%token
    "true"  { Lexeme TokenTrue _ }
    "false" { Lexeme TokenFalse _ }
    "const" { Lexeme TokenConst _ }
    "struct"{ Lexeme TokenStruct _ }
    "type"  { Lexeme TokenType _ }
    "func"  { Lexeme TokenFunc _ }
    "var"   { Lexeme TokenVar _ }
    "fmt.Print" { Lexeme TokenPrint _ }
    "chan"  { Lexeme TokenChan _ }
    "map"   { Lexeme TokenMap _ }
    "fmt.Println" { Lexeme TokenPrintLn _ }
    "return"{ Lexeme TokenReturn _ }
    "interface"{ Lexeme TokenInterface _ }
    let     { Lexeme TokenLet _ }
    in      { Lexeme TokenIn _ }
    STRING  { Lexeme (TokenString $$) _ }
    NUM     { Lexeme (TokenNum $$) _ }
    NAME    { Lexeme (TokenSym $$) _ }
    '\\'    { Lexeme TokenLambda _ }
    '->'    { Lexeme TokenArrow _ }
    "<-"    { Lexeme TokenLeftArrow _ }
    "=="    { Lexeme TokenEq2 _ }
    "!="    { Lexeme TokenNeq _ }
    "<"     { Lexeme TokenLess _ }
    "<="    { Lexeme TokenLessEq _ }
    ">"     { Lexeme TokenGreater _ }
    ">="    { Lexeme TokenGreaterEq _ }
    '='     { Lexeme TokenEq _ }
    '('     { Lexeme TokenLParen _ }
    ')'     { Lexeme TokenRParen _ }
    '['     { Lexeme TokenLBracket _ }
    ']'     { Lexeme TokenRBracket _ }
    "..."   { Lexeme TokenDots _ }
    '.'     { Lexeme TokenDot _ }
    ','     { Lexeme TokenComma _ }
    '+'     { Lexeme TokenAdd _ }
    '-'     { Lexeme TokenSub _ }
    '|'     { Lexeme TokenOpPipe _ }
    '^'     { Lexeme TokenOpUpArrow _ }
    '*'     { Lexeme TokenOpMul _ }
    '/'     { Lexeme TokenOpSlash _ }
    '%'     { Lexeme TokenOpModulo _ }
    "<<"    { Lexeme TokenOpLeftStream _ }
    ">>"    { Lexeme TokenOpRightStream _ }
    '&'     { Lexeme TokenOpAnd _ }
    "&^"    { Lexeme TokenOpAndUp _ }
    ":="    { Lexeme TokenShortVarDecl _ }
    "++"    { Lexeme TokenInc _ }
    "--"    { Lexeme TokenDec _ }
    '{'     { Lexeme TokenLCParen _ }
    '}'     { Lexeme TokenRCParen _ }
    ';'     { Lexeme TokenSemicolon _ }
    "if"    { Lexeme TokenIf _ }
    "else"  { Lexeme TokenElse _ }
    "package"{ Lexeme TokenPackage _ }

%left '+' '-'
%left '*' '/' '%'
%%


-- Unsure if the last semicolon is correct
SourceFile : PackageClause ';' TopLevelDecls            { SourceFile $1 $3 }

PackageClause : "package" NAME                          { Package $2 }

TopLevelDecls : TopLevelDecl ';'                        { [$1] }
              | TopLevelDecls TopLevelDecl ';'          { $1 ++ [$2] }

TopLevelDecl : Declaration                              { TopLevelDecl1 $1 }
             | FunctionDecl                             { TopLevelDecl2 $1 }
             | MethodDecl                               { TopLevelDecl3 $1 }

StatementList : Statement ';'                           { [$1] }
           | StatementList Statement ';'            { $1 ++ [$2] }

Statement : IfStmt                                      { IfStmt $1 }
          | "return" Expr                               { ReturnStmt $2 }
          | Block                                       { BlockStmt $1 }
          | SimpleStmt                                  { SimpleStmt $1 }
          | Declaration                                 { DeclarationStmt $1 }

SimpleStmt : 
           ShortVarDecl                                 { $1 }
           |  Assignment                                { Assignment $1 }
        --    | SendStmt   
           | IncDecStmt                                 { IncDecStmt $1 }
           | Expr                                       { ExpressionStmt $1 }
           |  {- empty -}                               { EmptyStmt }

IfStmt : "if" Expr Block Else                           { Ifstmt2 $2 $3 $4 }
       | "if" Expr Block                                { Ifstmt1 $2 $3 }
       | "if" SimpleStmt ';' Expr Block Else            { Ifstmt4 $2 $4 $5 $6 }
       | "if" SimpleStmt ';' Expr Block                 { Ifstmt3 $2 $4 $5 }

Block : '{' '}'                                         { Block [] }
      | '{' StatementList '}'                           { Block $2 }

SimpleStmts : SimpleStmt ';'                            { [$1] }
            | SimpleStmts SimpleStmt ';'                { $1 ++ [$2] }

ElseList : Else                                         { [$1] }
         | ElseList Else                                { $1 ++ [$2] }

Else : "else" IfStmt                                    { Else1 $2 }
     | "else" Block                                     { Else2 $2 }

ShortVarDecl : IdentifierList ":=" ExpressionList       { ShortVarDecl $1 $3 }

BinExpr : AritmExpr                                     { AritmExpr $1 } 
    | CondExpr                                          { CondExpr $1 } 

Expr : '(' Expr ')'                                     { $2 }
     | BinExpr                                          { BinExpr $1}
     | "fmt.Print" '(' ExpressionList ')'                   { PrintCall $3 }
     | "fmt.Println" '(' ExpressionList ')'                 { PrintLnCall $3 }
     | NAME '(' ExpressionList ')'                      { Call $1 $3 } 
     | "true"                                           { BoolExpr True }
     | "false"                                          { BoolExpr False }
     | STRING                                           { StringExpr $1 }
     | NAME                                             { IdUse $1 }
     | NUM                                              { Num $1 }

Assignment : ExpressionList '=' ExpressionList          { Assign $1 $3 }
           | ExpressionList AssignOp '=' ExpressionList { OpAssign $2 $1 $4 }
 
AssignOp : AddOp '='                                    { $1 }
         | MulOp '='                                    { $1 }



Op : AddOp                                              { $1 }
   | MulOp                                              { $1 }


IncDecStmt : Expr "++"                                  { IncStmt $1 }
           | Expr "--"                                  { DecStmt $1 }

{-- TODO support functions without bodies --}
FunctionDecl : "func" FunctionName Signature            { FunctionDecl1 $2 $3 }
             | "func" FunctionName Signature FunctionBody { FunctionDecl2 $2 $3 $4 } 
             
FunctionName : NAME                                     { $1 }

FunctionBody : Block                                    { $1 }

Signature : '(' ')'                                     { Signature1 }
          | '(' Parameters ')'                          { Signature2 $2 }
          | '(' Parameters ')' Result                   { Signature3 $2 $4 }

Result : '(' ')'                                        { Result1 }
       | '(' Parameters ')'                             { Result2 $2 }
       | Type                                           { Result3 $1 }

{-- TODO should there be 0..n lists  --}
Parameters :  ParameterList                             { $1 }
           |  ParameterList ','                         { $1 }

ParameterList : ParameterDecl                           { [$1] }
              | ParameterList ',' ParameterDecl         { $1 ++ [$3] }

ParameterDecl : Type                                    { ParameterDecl1 $1 }
              | IdentifierList Type                     { ParameterDecl2 $1 $2 }
              | IdentifierList "..." Type               { ParameterDecl2 $1 $3 }

MethodDecl : "func" Receiver MethodName Signature FunctionBody { MethodDecl1 $2 $3 $4 $5 }
           | "func" Receiver MethodName Signature       { MethodDecl2 $2 $3 $4}

Receiver : '(' Parameters ')'                           { $2 }

Declaration : ConstDecl                                 { $1 }
            | TypeDecl                                  { $1 }
            | VarDecl                                   { $1 }

--TODO can have multiple specs
ConstDecl : "const" ConstSpec                           { $2 }

ConstSpec : IdentifierList Type '=' ExpressionList      { ConstDecl $1 $2 $4 }

-- TODO more typespecs
TypeDecl : "type" TypeSpec                              { $2 }

TypeSpec : NAME Type                                    { TypeDecl $1 $2 }

VarDecl : "var" VarSpec                                 { $2 }
VarSpec : IdentifierList Type '=' ExpressionList        { VarDecl $1 $2 $4 }

IdentifierList : NAME                                   { [(IdDecl $1)] }
               | IdentifierList ',' NAME                { $1 ++ [(IdDecl $3)] }

ExpressionList : Expr                                   { [$1] }
               | ExpressionList ',' Expr                { $1 ++ [$3] } 
               |                                        { [] }

Type : TypeLit                                          { TypeLit $1 }
     | TypeName                                         { TypeName $1 }
     | '(' Type ')'                                     { $2 }

TypeName : NAME                                         { TypeNameIdentifier $1 } 
         | QualifiedIdent                               { TypeNameQualifiedIdent $1 }

QualifiedIdent : NAME '.' NAME                          { QualifiedIdent  $1 $3 }

TypeLit : ArrayType                                     { $1 } 
        | StructType                                    { StructTypeLit $1 } 
        | PointerType                                   { PointerTypeLit $1 }
        | FunctionType                                  { FunctionTypeLit $1 }
        | InterfaceType                                 { InterfaceTypeLit $1 }
        | SliceType                                     { SliceTypeLit $1 }
        | MapType                                       { MapTypeLit $1 }
        | ChannelType                                   { ChannelTypeLit $1 }

PointerType : '*' BaseType                              { PointerType $2 }
BaseType : Type                                         { $1 }

FunctionType : "func" Signature                         { FunctionType $2 }

InterfaceType : "interface" '{' MethodSpecs '}'         { InterfaceType $3 }
              | "interface" '{' '}'                     { InterfaceType [] }

MethodSpecs : MethodSpec ';'                            { [$1] }
            | MethodSpecs MethodSpec ';'                { $1 ++ [$2] }

SliceType : '[' ']' ElementType                         { SliceType $3 }

ElementType : Type                                      { $1 }

MapType : "map" '[' KeyType ']' ElementType             { MapType $3 $5 }
KeyType : Type                                          { $1 }

ChannelType : "chan" ElementType                        { ChannelType1 $2 }
            | "chan" "<-"  ElementType                  { ChannelType2 $3 }
            | "<-" "chan" ElementType                   { ChannelType3 $3 }

MethodSpec : MethodName Signature                       { MethodSpec1 $1 $2 }
           | InterfaceTypeName                          { MethodSpec2 $1 }

MethodName : NAME                                       { $1 } 

InterfaceTypeName : TypeName                            { $1 }

ArrayType : '[' Expr ']' ElementType                    { ArrayType $2 $4 } 

StructType : "struct" '{' FieldDecls '}'                { StructType1 $3 }
           | "struct" '{' '}'                           { StructType2 } 

{- TODO semicolons after fielddecl -}
FieldDecls : FieldDecl ';' FieldDecls                   { [$1] ++ $3 }
           | FieldDecl ';'                              { [$1] }

FieldDecl : IdentifierList Type                         { FieldDecl1 $2 }
          | IdentifierList Type Tag                     { FieldDecl2 $2 $3 }
          | AnonymousField                              { AnonFieldDecl1 $1 }
          | AnonymousField Tag                          { AnonFieldDecl2 $1 $2 }

AnonymousField : TypeName                               { AnonFieldType1 $1 }
               | '*' TypeName                           { AnonFieldType2 $2 }


Tag : STRING                                            { $1 }


AritmExpr : Expr '+' Expr                                { AddExpr $1 $3 }
        | Expr '-' Expr                                  { SubExpr $1 $3 }
        | Expr '*' Expr                                  { MulExpr $1 $3 }
        | Expr '/' Expr                                  { DivExpr $1 $3 }
        | Expr '%' Expr                                  { ModExpr $1 $3 }

CondExpr : Expr "==" Expr                                { Eq_ $1 $3 }
        | Expr "!=" Expr                                  { Neq $1 $3 }
        | Expr "<" Expr                                  { Less $1 $3 }
        | Expr "<=" Expr                                  { LessEq $1 $3 }
        | Expr ">" Expr                                  { Greater $1 $3 }
        | Expr ">=" Expr                                  { GreaterEq $1 $3 }

AddOp   : '+'                                           { AddOp }
        | '-'                                           { SubOp }
        | '|'                                           { PipeOp }
        | '^'                                           { UpOp }

MulOp  : '*'                                            { MulOp }
       | '/'                                            { DivOp }
       | '%'                                            { ModOp }
       | "<<"                                           { LeftOp }
       | ">>"                                           { RightOp }
       | '&'                                            { AmpOp }
       | "&^"                                           { AmpUpOp }


{-
Expr : let NAME '=' Expr in Expr                         { App (Abs $2 $6) $4 }
     | '\\' NAME '->' Expr                               { Abs $2 $4 }
     | Form                                             { $1 }


Form : Form '+' Form                                    { Binop Add $1 $3 }
     | Form '-' Form                                    { Binop Sub $1 $3 }
     | Form '*' Form                                    { Binop Mul $1 $3 }
     | Juxt                                             { $1 }

Juxt : Juxt Atom                                        { App $1 $2 }
     | Atom                                             { $1 }

Atom : '(' Expr ')'                                     { $2 }
     | NUM                                              { Num $1 }
     | NAME                                              { Var $1 }


-}

{


data SyntaxError = SyntaxError String
    deriving (Show)

instance Exception SyntaxError

parseError :: [Lexeme Token] -> IO a
parseError ts = throw $ SyntaxError $ "Unexpected token: " ++ show (head ts) ++ ", rest: " ++ show ( tail ts)

}
