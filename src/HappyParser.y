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
    '\\'    { TokenLambda }
    '->'    { TokenArrow }
    '='     { TokenEq }
    '+'     { TokenAdd }
    '-'     { TokenSub }
    '*'     { TokenMul }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '['     { TokenLBracket }
    ']'     { TokenRBracket }
    '.'     { TokenDot }

%left '+' '-'
%left '*'
%%


Statements : Statement                                  { [$1] }
            | Statements Statement                      { $2 : $1 }

Statement : Declaration                                 { Declaration $1 }
          | Expr                                        { Expr $1 }

Declaration : ConstDecl                                 { ConstDecl $1 }
            | TypeDecl                                  { TypeDecl $1 }

ConstDecl : "const" ConstSpec                           { $2 }
ConstSpec : IdentifierList Type '=' ExpressionList      { ConstSpec $1 $2 $4 }

TypeDecl : "type" TypeSpec                              { $2 }
TypeSpec : VAR Type                                     { TypeSpec $1 $2 }

VarDecl : "var" VarSpec                                 { $2 }
VarSpec : IdentifierList Type '=' ExpressionList        { VarSpec $1 $2 $4 }

IdentifierList : VAR                                    { $1 }
ExpressionList : Expr                                   { $1 }

Type : TypeName                                         { TypeName $1 }
     | TypeLit                                          { $1 }
     | '(' Type ')'                                     { $2 }
     | VAR                                              { Type $1 }

TypeName : VAR                                          {  TypNameIdentifier $1 } 
         | QualifiedIdent                               {  TypeNameQualifiedIdent $1 }

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
parseError _ = error "Parse error"

parseExpr :: String -> [Statement]
parseExpr = stmt . scanTokens
}
