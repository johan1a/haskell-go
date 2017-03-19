{
{-# OPTIONS_GHC -w #-}
module AlexToken (Token(..),scanTokens) where
import AST
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-ZåäöÅÄÖ]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "#".*                         ;
  let                           { \s -> TokenLet }
  "]"                           { \s -> TokenRBracket }
  "["                           { \s -> TokenLBracket } 
  "."                           { \s -> TokenDot }
  in                            { \s -> TokenIn }
  $digit+                       { \s -> TokenNum (read s) }
  "->"                          { \s -> TokenArrow }
  "=="    			{ \s -> TokenEq2 }
  "!="    			{ \s -> TokenNeq }
  "<"     			{ \s -> TokenLess }
  "<="    			{ \s -> TokenLessEq }
  ">"     			{ \s -> TokenGreater }
  ">="    			{ \s -> TokenGreaterEq }
  "true"    			{ \s -> TokenTrue }
  "false"    			{ \s -> TokenFalse }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLambda }
  [\+]                          { \s -> TokenAdd }
  [\-]                          { \s -> TokenSub }
  [\*]                          { \s -> TokenMul }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  "print" 			{ \s -> TokenPrint }
  "const"                       { \s -> TokenConst }
  "type"                        { \s -> TokenType }
  "var"                         { \s -> TokenVar }
    "."                         { \s -> TokenDot }
    ","                         { \s -> TokenComma }
    "+"                         { \s -> TokenOpAdd }
    "-"                         { \s -> TokenOpSub }
    "|"                         { \s -> TokenOpPipe }
    "^"                         { \s -> TokenOpUpArrow }
    "*"                         { \s -> TokenOpMul }
    "/"                         { \s -> TokenOpSlash }
    "%"                         { \s -> TokenOpModulo }
    "<<"                        { \s -> TokenOpLeftStream }
    ">>"                        { \s -> TokenOpRightStream }
    "&"                         { \s -> TokenOpAnd }
    "&^"                        { \s -> TokenOpAndUp }
    ":="                        { \s -> TokenShortVarDecl }
    "++"                        { \s -> TokenInc }
    "--"                        { \s -> TokenDec }
    "{"                         { \s -> TokenLCParen }
    "}"                         { \s -> TokenRCParen }
    "if"                        { \s -> TokenIf }
    "else"                      { \s -> TokenElse }
    ";"                         { \s -> TokenSemiColon }
  $alpha [$alpha $digit \_ \"]* { \s -> TokenSym s }

{

data Token = TokenLet
           | TokenTrue
           | TokenFalse
           | TokenIn
	   | TokenEq2
	   | TokenNeq
	   | TokenLess
	   | TokenLessEq
	   | TokenGreater
	   | TokenGreaterEq
           | TokenConst
           | TokenType
           | TokenVar
           | TokenLambda
           | TokenNum Int
           | TokenSym String
           | TokenArrow
           | TokenEq
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenLParen
           | TokenRParen
           | TokenLBracket
           | TokenRBracket
           | TokenDot 
           | TokenComma
           | TokenOpAdd
           | TokenOpSub 
           | TokenOpPipe 
           | TokenOpUpArrow 
           | TokenOpMul 
           | TokenOpSlash 
           | TokenOpModulo 
           | TokenOpLeftStream 
           | TokenOpRightStream 
           | TokenOpAnd
           | TokenOpAndUp 
           | TokenPrint 
           | TokenShortVarDecl
           | TokenInc
           | TokenDec
           | TokenLCParen
           | TokenRCParen
           | TokenSemiColon
           | TokenIf
           | TokenElse
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
