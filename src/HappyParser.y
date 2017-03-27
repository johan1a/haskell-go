{
module HappyParser where

import AlexToken
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    "true"  { TokenTrue }
    "false" { TokenFalse }
    "const" { TokenConst }
    "struct"{ TokenStruct }
    "type"  { TokenType }
    "func"  { TokenFunc }
    "var"   { TokenVar }
    "print" { TokenPrint }
    "println" { TokenPrintLn }
    "return"{ TokenReturn }
    let     { TokenLet }
    in      { TokenIn }
    STRING  { TokenString $$ }
    NUM     { TokenNum $$ }
    NAME    { TokenSym $$ }
    '\\'    { TokenLambda }
    '->'    { TokenArrow }
    "=="    { TokenEq2 }
    "!="    { TokenNeq }
    "<"     { TokenLess }
    "<="    { TokenLessEq }
    ">"     { TokenGreater }
    ">="    { TokenGreaterEq }
    '='     { TokenEq }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '['     { TokenLBracket }
    ']'     { TokenRBracket }
    "..."   { TokenDots }
    '.'     { TokenDot }
    ','     { TokenComma }
    '+'     { TokenAdd }
    '-'     { TokenSub }
    '|'     { TokenOpPipe }
    '^'     { TokenOpUpArrow }
    '*'     { TokenOpMul }
    '/'     { TokenOpSlash }
    '%'     { TokenOpModulo }
    "<<"    { TokenOpLeftStream }
    ">>"    { TokenOpRightStream }
    '&'     { TokenOpAnd }
    "&^"    { TokenOpAndUp }
    ":="    { TokenShortVarDecl }
    "++"    { TokenInc }
    "--"    { TokenDec }
    '{'     { TokenLCParen }
    '}'     { TokenRCParen }
    ';'     { TokenSemiColon }
    "if"    { TokenIf }
    "else"  { TokenElse }
    "package"{ TokenPackage }

%left '+' '-'
%left '*' '/' '%'
%%


SourceFile : PackageClause ';' TopLevelDecls            { SourceFile $1 $3 }

PackageClause : "package" NAME                          { Package $2 }

TopLevelDecls : TopLevelDecl                            { [$1] }
              | TopLevelDecls TopLevelDecl              { $1 ++ [$2] }

TopLevelDecl : Declaration                              { TopLevelDecl1 $1 }
             | FunctionDecl                             { TopLevelDecl2 $1 }

Statements : Statement                                  { [$1] }
           | Statements Statement                       { $1 ++ [$2] }

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

-- Should really be StatementList with fancy semicolon insertion
Block : '{' Statements '}'                              { Block $2 }

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
     | "print" '(' ExpressionList ')'                   { PrintCall $3 }
     | "println" '(' ExpressionList ')'                 { PrintLnCall $3 }
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

Declaration : ConstDecl                                 { $1 }
            | TypeDecl                                  { $1 }
            | VarDecl                                   { $1 }

ConstDecl : "const" ConstSpec                           { $2 }
ConstSpec : IdentifierList Type '=' ExpressionList      { ConstDecl $1 $2 $4 }

TypeDecl : "type" TypeSpec                              { $2 }
TypeSpec : NAME Type                                    { TypeDecl $1 $2 }

VarDecl : "var" VarSpec                                 { $2 }
VarSpec : IdentifierList Type '=' ExpressionList        { VarDecl $1 $2 $4 }

IdentifierList : NAME                                   { [(IdDecl $1)] }
               | IdentifierList ',' NAME                { $1 ++ [(IdDecl $3)] }

ExpressionList : Expr                                   { [$1] }
               | ExpressionList ',' Expr                { $1 ++ [$3] } 
               |                                        { [] }

Type : TypeName                                         { TypeName $1 }
     | TypeLit                                          { TypeLit $1 }
{--     | '(' Type ')'                                  { $2 } --}

TypeName : NAME                                         { TypeNameIdentifier $1 } 
         | QualifiedIdent                               { TypeNameQualifiedIdent $1 }

QualifiedIdent : NAME '.' NAME                          { QualifiedIdent  $1 $3 }

TypeLit : ArrayType                                     { $1 } 
        | StructType                                    { StructType $1 } 

ArrayType : '[' Expr ']' ElementType                    { ArrayType $2 $4 } 

ElementType : Type                                      { $1 } 


StructType : "struct" '{' FieldDecls '}'                { Struct1 $3 }
           | "struct" '{' '}'                           { Struct2 } 


{- TODO semicolons after fielddecl -}
FieldDecls : FieldDecl ';' FieldDecls                      { [$1] ++ $3 }
           | FieldDecl ';'                                 { [$1] }

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
parseError :: [Token] -> a
parseError tokens = error $ "Parse error " ++ (show tokens)

parseExpr :: String -> SourceFile
parseExpr = parse . scanTokens
}
