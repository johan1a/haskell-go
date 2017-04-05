{
module HappyParser (
parse
) where

import AlexToken
import AST

import Control.Exception
import Data.Typeable

import Control.Monad.Except
}

%name baseParse
%monad { Except String }
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
    string_lit  { Lexeme (TokenString $$) _ }
    int_lit     { Lexeme (TokenNum $$) _ }
    identifier    { Lexeme (TokenSym $$) _ }
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
    ':'     { Lexeme TokenColon _ }
    '!'     { Lexeme TokenExclamation _ }
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

%left COMPOSITE
%left '+' '-'
%left '*' '/' '%'
%%


-- Unsure if the last semicolon is correct
SourceFile : PackageClause ';' TopLevelDecls            { SourceFile $1 $3 }

PackageClause : "package" identifier                          { Package $2 }

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


Expr : BinExpr                                          { BinExpr $1 }
     | UnaryExpr                                        { UnaryExpr $1 }

UnaryExpr : PrimaryExpr                                 { PrimaryExpr $1 }
          | '+' UnaryExpr                               { PosExpr $2 }
          | '-' UnaryExpr                               { NegExpr $2 }
          | '!' UnaryExpr                               { BoolNegExpr $2 }
          | '^' UnaryExpr                               { UpArrowExpr $2 }
          | '*' UnaryExpr                               { StarExpr $2 }
          | '&' UnaryExpr                               { RefExpr $2 }
          | "<-" UnaryExpr                              { LeftArrowExpr $2 }

BinExpr : AritmExpr                                     { AritmExpr $1 } 
        | CondExpr                                      { CondExpr $1 } 

PrimaryExpr : Operand                                   { PrimaryExpr1 $1 }
      --      | PrimaryExpr Selector                      { PrimaryExpr3 $1 $2 }
    --        | PrimaryExpr Index                         { PrimaryExpr4 $1 $2 }
  --          | PrimaryExpr Slice                         { PrimaryExpr5 $1 $2 }
--            | PrimaryExpr TypeAssertion                 { PrimaryExpr6 $1 $2 }
              | PrimaryExpr Arguments                     { PrimaryExpr7 $1 $2 }
--            | Conversion                                { PrimaryExpr2 $1 }

Conversion : Type '(' Expr ',' ')'                      { Conversion $1 $3 }
           | Type '(' Expr ')'                          { Conversion $1 $3}

MethodExpr : ReceiverType '.' MethodName                { MethodExpr $1 $3 }

ReceiverType : TypeName                                 { ReceiverType1 $1 }
             | '(' '*' TypeName ')'                     { ReceiverType2 $3 }
             | '(' ReceiverType ')'                     { ReceiverType3 $2 }



ElementList : KeyedElement                              { [$1]  }
            | ElementList ',' KeyedElement              { $1 ++ [$3] }

KeyedElement : Element                                  { KeyedElement1 $1 }
             | Key ':' Element                          { KeyedElement2 $1 $3 }

Key : FieldName                                         { Key1 $1 }
    | Expr                                              { Key2 $1 }
    | LiteralValue                                      { Key3 $1 }

FieldName : identifier                                        { $1 }

Element : Expr                                          { Element1 $1 }
        | LiteralValue                                  { Element2 $1 }

Operand : OperandName                                   { Operand2 $1 }
        | Literal                                       { Operand1 $1 }
        | '(' Expr ')'                                  { Operand4 $2  }
    --    | MethodExpr                                  { Operand3 $1  }

-- Ambiguity in the Go syntax, TODO complits without parens
Literal : BasicLit                                      { BasicLit $1 }
--        | "func" Signature FunctionBody               { FunctionLit $2 $3  }
          | '(' LiteralType LiteralValue ')'            { CompositeLit $2 $3 } 

LiteralType : StructType                                { LiteralType1 $1 }
     --       | ArrayType                                 { LiteralType2 $1 }
   --         | '[' "..." ']' ElementType                 { LiteralType3 $4 }
 --           | SliceType                                 { LiteralType4 $1 }
--            | MapType                                   { LiteralType5 $1 }
              | TypeName                                  { LiteralType6 $1 }

LiteralValue : '{' '}'                                  { LiteralValue1 }
             | '{' ElementList '}'                      { LiteralValue2 $2 }
             | '{' ElementList ',' '}'                  { LiteralValue2 $2 }

BasicLit : int_lit                                      { IntLit $1 }
         | string_lit                                   { StringLit $1 }
{-
         | float_lit                                    { FloatLit $1 }
         | imaginary_lit                                { ImaginaryLit $1 }
         | rune_lit                                     { RuneLit $1 }
-}

OperandName : identifier                                { OperandName1 $1 }
            | QualifiedIdent                            { OperandName2 $1 }

Selector : '.' identifier                               { Selector $2 } 
         
Index : '[' Expr ']'                                    { Index $2 }

Slice : '[' Expr ':' Expr ']'                           { Slice1 $2 $4 }        
      | '[' Expr ':' ']'                                { Slice2 $2 }
      | '[' ':' Expr ']'                                { Slice3 $3 }
      | '[' ':' ']'                                     { Slice4 }
      | '[' Expr ':' Expr':' Expr ']'                   { Slice5 $2 $4 $6 }
      | '[' ':' Expr ':' Expr ']'                       { Slice6 $3 $5 }

TypeAssertion : '.' '(' Type ')'                          { TypeAssertion $3 }

Arguments : '(' ')'                                     { Arguments1 }
          | '(' ExpressionList "..." ',' ')'            { Arguments2 $2 }
          | '(' ExpressionList "..." ')'                { Arguments3 $2 }
          | '(' ExpressionList ',' ')'                  { Arguments4 $2 }
          | '(' ExpressionList ')'                      { Arguments5 $2 }
          | '(' Type ',' ExpressionList "..." ',' ')'   { Arguments6 $2 $4 }
          | '(' Type ',' ExpressionList "..." ')'       { Arguments7 $2 $4 }
          | '(' Type ',' ExpressionList ',' ')'         { Arguments8 $2 $4 }
          | '(' Type ',' ExpressionList ')'             { Arguments9 $2 $4 }
          | '(' Type "..." ',' ')'                      { Arguments10 $2 }
          | '(' Type "..." ')'                          { Arguments11 $2 }
          | '(' Type ',' ')'                            { Arguments12 $2 }
--          | '(' Type ')'                                { Arguments13 $2 }

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
             
FunctionName : identifier                                     { $1 }

FunctionBody : Block                                    { $1 }

Signature : Parameters                                  { Signature1 $1 }
          | Parameters  Result                          { Signature2 $1 $2 }

Result : Parameters                                     { Result1 $1 }
       | Type                                           { Result2 $1 }

{-- TODO should there be 0..n lists  --}
Parameters : '(' ParameterList ')'                      { $2 }
           | '(' ParameterList ',' ')'                  { $2 }
           | '(' ')'                                    { [] }

ParameterList : ParameterDecl                           { [$1] }
              | ParameterList ',' ParameterDecl         { $1 ++ [$3] }

ParameterDecl : Type                                    { ParameterDecl1 $1 }
              | IdentifierList Type                     { ParameterDecl2 $1 $2 }
              | IdentifierList "..." Type               { ParameterDecl2 $1 $3 }

MethodDecl : "func" Receiver MethodName Signature FunctionBody { MethodDecl1 $2 $3 $4 $5 }
           | "func" Receiver MethodName Signature       { MethodDecl2 $2 $3 $4}

Receiver : Parameters                                   { $1 }

Declaration : ConstDecl                                 { $1 }
            | TypeDecl                                  { $1 }
            | VarDecl                                   { $1 }

--TODO can have multiple specs
ConstDecl : "const" ConstSpec                           { $2 }

ConstSpec : IdentifierList Type '=' ExpressionList      { ConstDecl $1 $2 $4 }

-- TODO more typespecs
TypeDecl : "type" TypeSpec                              { TypeDecl [$2]  } 
         | "type" '(' TypeSpecs ')'                     { TypeDecl $3    }

TypeSpecs :  TypeSpec                                   { [$1]           }
          | TypeSpecs ';' TypeSpec                      { $1 ++ [$3]     }

TypeSpec : identifier Type                              { TypeSpec $1 $2 }

VarDecl : "var" VarSpec                                 { $2 }
VarSpec : IdentifierList Type '=' ExpressionList        { VarDecl $1 $2 $4 }

IdentifierList : identifier                             { [(IdDecl $1)] }
               | IdentifierList ',' identifier          { $1 ++ [(IdDecl $3)] }

ExpressionList : Expr                                   { [$1] }
               | ExpressionList ',' Expr                { $1 ++ [$3] } 
               |                                        { [] }

Type : TypeLit                                          { TypeLit $1 }
     | TypeName                                         { TypeName $1 }
--     | '(' Type ')'                                   { $2 }

TypeName : identifier                                   { TypeNameIdentifier $1 } 
         | QualifiedIdent                               { TypeNameQualifiedIdent $1 }

QualifiedIdent : identifier '.' identifier              { QualifiedIdent  $1 $3 }

TypeLit : ArrayType                                     { ArrayTypeLit $1 } 
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

MethodName : identifier                                       { $1 } 

InterfaceTypeName : TypeName                            { $1 }

ArrayType : '[' Expr ']' ElementType                    { ArrayType $2 $4 } 

StructType : "struct" '{' FieldDecls '}'                { StructType1 $3 }
           | "struct" '{' '}'                           { StructType2 } 

{- TODO semicolons after fielddecl -}
FieldDecls : FieldDecl ';' FieldDecls                   { [$1] ++ $3 }
           | FieldDecl ';'                              { [$1] }

FieldDecl : IdentifierList Type                         { FieldDecl1 $1 $2 }
          | IdentifierList Type Tag                     { FieldDecl2 $1 $2 $3 }
          | AnonymousField                              { AnonFieldDecl1 $1 }
          | AnonymousField Tag                          { AnonFieldDecl2 $1 $2 }

AnonymousField : TypeName                               { AnonFieldType1 $1 }
               | '*' TypeName                           { AnonFieldType2 $2 }

Tag : string_lit                                            { $1 }

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

{

data SyntaxError = SyntaxError String
    deriving (Show)

instance Exception SyntaxError

parseError :: [Lexeme Token] -> Except String a
parseError ts = throwError $ "Unexpected token: " ++ show (head ts) ++ ", rest: " ++ show ( tail ts)

parse :: [Lexeme Token] -> Either String SourceFile
parse input = runExcept $ baseParse input

}
