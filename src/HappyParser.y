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
    "print" { TokenPrint }
    let     { TokenLet }
    in      { TokenIn }
    NUM     { TokenNum $$ }
    NAME    { TokenSym $$ }
    OP      { TokenSym $$ }
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
    '.'     { TokenDot }
    ','     { TokenComma }
    '+'     { TokenAdd }
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
           | Statements Statement                       { $1 ++ [$2] }

Statement : IfStmt                                      { IfStmt $1 }
          | Block                                       { BlockStmt $1 }
 	  | SimpleStmt                                  { SimpleStmt $1 }
	  | Declaration                                 { Declaration $1 }


-- Should really be StatementList with fancy semicolon insertion
Block : '{' Statements '}'                              { Block $2 }

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

Expr : BinExpr                                          { BinExpr $1}
     | "print" '(' ExpressionList ')'			{ PrintCall $3 }
     | NAME '(' ExpressionList ')' 			{ Call $1 $3 } 
     | NAME                                             { IdUse $1 }
     | NUM                                              { Num $1 }

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

Op : AddOp                                              { $1 }
   | MulOp                                              { $1 }


IncDecStmt : Expr "++"                                  { IncStmt $1 }
           | Expr "--"                                  { DecStmt $1 }

Declaration : ConstDecl                                 { $1 }
            | TypeDecl                                  { $1 }
            | VarDecl                                   { $1 }

ConstDecl : "const" ConstSpec                           { $2 }
ConstSpec : IdentifierList Type '=' ExpressionList      { ConstDecl $1 $2 $4 }

TypeDecl : "type" TypeSpec                              { $2 }
TypeSpec : NAME Type                                     { TypeDecl $1 $2 }

VarDecl : "var" VarSpec                                 { $2 }
VarSpec : IdentifierList Type '=' ExpressionList        { VarDecl $1 $2 $4 }

IdentifierList : NAME                                    { [(IdDecl $1)] }
               | IdentifierList ',' NAME                 { $1 ++ [(IdDecl $3)] }

ExpressionList : Expr                                   { [$1] }
               | ExpressionList ',' Expr                { $1 ++ [$3] } 

Type : TypeName                                         { TypeName $1 }
     | TypeLit                                          { $1 }
     | '(' Type ')'                                     { $2 }
     | NAME                                              { Type $1 }

TypeName : NAME                                          { TypNameIdentifier $1 } 
         | QualifiedIdent                               { TypeNameQualifiedIdent $1 }

QualifiedIdent : NAME '.' NAME                            { QualifiedIdent  $1 $3 }

TypeLit : ArrayType                                     { TypeLit $1 } 

ArrayType : '[' Expr ']' ElementType                    { ArrayType $2 $4 } 

ElementType : Type                                      { $1 } 


BinExpr : AritmExpr                                     { AritmExpr $1 } 
	| CondExpr                                      { CondExpr $1 } 


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

parseExpr :: String -> [Statement]
parseExpr = stmt . scanTokens
}
