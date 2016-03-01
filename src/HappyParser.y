{
module HappyParser where

import AlexToken
import AST
}

%name stmt
%tokentype { Token }
%error { parseError }

%token
    "const" { TokenConst }
    "type"  { TokenType }
    "var"   { TokenVar }
    let     { TokenLet }
    in      { TokenIn }
    NUM     { TokenNum $$ }
    VAR     { TokenSym $$ }
    OP      { TokenSym $$ }
    '\\'    { TokenLambda }
    '->'    { TokenArrow }
    '='     { TokenEq }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '['     { TokenLBracket }
    ']'     { TokenRBracket }
    '.'     { TokenDot }
    ','     { TokenComma }
    '+'     { TokenOpAdd }
    '-'     { TokenOpSub }
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

%left '+' '-'
%left '*'
%%


Statements : Statement                                  { [$1] }
           | Statements Statement                      { $1 ++ [$2] }

-- Should really be StatementList with fancy semicolon insertion
Block : '{' Statements '}'                           { Block $2 }


Statement : Declaration                                 { Declaration $1 }
          | SimpleStmt                                  {  SimpleStmt $1 }
          | IfStmt                                      { IfStmt $1 }


SimpleStmt : 
           ShortVarDecl                                 { $1 }
           |  Assignment                                { Assignment $1 }
        --    | SendStmt   
           | IncDecStmt                                 { IncDecStmt $1 }
           | Expr                                       { ExpressionStmt $1 }
           |  {- empty -}                               { EmptyStmt }



IfStmt : "if" Expr Block                                { Ifstmt1 ( $2) $3 }
       | "if" Expr Block "else" Else                    { Ifstmt2 ( $2)  $3 $5 }
       | "if" SimpleStmt ';' Expr Block                 { Ifstmt3 $2 ($4)  $5 }
       | "if" SimpleStmt ';' Expr Block "else" Else     { Ifstmt4 $2 ( $4)  $5 $7 }

SimpleStmts : SimpleStmt ';'                            { [$1] }
            | SimpleStmts SimpleStmt ';'                { $1 ++ [$2] }

ElseList : Else                                         { [$1] }
         | ElseList Else                                { $1 ++ [$2] }

Else : "else" IfStmt                                    { Else1 $2 }
     | "else" Block                                     { Else2 $2 }

ShortVarDecl : IdentifierList ":=" ExpressionList       { ShortVarDecl $1 $3 }

Assignment : ExpressionList '=' ExpressionList          { Assign $1 $3 }
           | ExpressionList AssignOp '=' ExpressionList { OpAssign $2 $1 $4 }
 
AssignOp : AddOp '='                                    { $1 }
         | MulOp '='                                    { $1 }

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


IncDecStmt : Expr "++"                                  { IncStmt $1 }
           | Expr "--"                                  { DecStmt $1 }

Declaration : ConstDecl                                 { ConstDecl $1 }
            | TypeDecl                                  { TypeDecl $1 }
            | VarDecl                                   { $1 }

ConstDecl : "const" ConstSpec                           { $2 }
ConstSpec : IdentifierList Type '=' ExpressionList      { ConstSpec $1 $2 $4 }

TypeDecl : "type" TypeSpec                              { $2 }
TypeSpec : VAR Type                                     { TypeSpec $1 $2 }

VarDecl : "var" VarSpec                                 { VarDecl $2 }
VarSpec : IdentifierList Type '=' ExpressionList        { VarSpec $1 $2 $4 }

IdentifierList : VAR                                    { [$1] }
               | IdentifierList ',' VAR                 { $1 ++ [$3] }

ExpressionList : Expr                                   { [$1] }
               | ExpressionList ',' Expr                { $1 ++ [$3] } 

Type : TypeName                                         { TypeName $1 }
     | TypeLit                                          { $1 }
     | '(' Type ')'                                     { $2 }
     | VAR                                              { Type $1 }

TypeName : VAR                                          { TypNameIdentifier $1 } 
         | QualifiedIdent                               { TypeNameQualifiedIdent $1 }

QualifiedIdent : VAR '.' VAR                            { QualifiedIdent  $1 $3 }

TypeLit : ArrayType                                     { TypeLit $1 } 

ArrayType : '[' Expr ']' ElementType                    { ArrayType $2 $4 } 

ElementType : Type                                      { $1 } 

Expr : NUM                                              { Num $1 }
     | VAR                                              { Var $1 }






{-
Expr : let VAR '=' Expr in Expr                         { App (Abs $2 $6) $4 }
     | '\\' VAR '->' Expr                               { Abs $2 $4 }
     | Form                                             { $1 }


Form : Form '+' Form                                    { Binop Add $1 $3 }
     | Form '-' Form                                    { Binop Sub $1 $3 }
     | Form '*' Form                                    { Binop Mul $1 $3 }
     | Juxt                                             { $1 }

Juxt : Juxt Atom                                        { App $1 $2 }
     | Atom                                             { $1 }

Atom : '(' Expr ')'                                     { $2 }
     | NUM                                              { Num $1 }
     | VAR                                              { Var $1 }


-}

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error " ++ (show tokens)

parseExpr :: String -> [Statement]
parseExpr = stmt . scanTokens
}
