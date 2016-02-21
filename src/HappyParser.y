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

%left '+' '-'
%left '*'
%%

--Declaration : "const" VAR                               {  Bajs $2 }


Stmt : Expr                                        { $1 }
--Type : VAR                                              { Var $1 }


--Statement : Declaration                                 { $1 }
--          | Expr                                        { $1 }
    


--Declaration : ConstDecl                                 { $1 }

--ConstDecl : "const" ConstSpec                           { $2 }

--ConstSpec : IdentifierList Type '=' ExpressionList      { Decl $1 $2 $4 }
--ConstSpec : IdentifierList VAR '=' Expr                 { Bajs $1 }

IdentifierList : VAR                                    { $1 }

ExpressionList : Expr                                   { $1 }

Type : VAR                                              { $1 }

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

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Stmt
parseExpr = stmt . scanTokens
}
