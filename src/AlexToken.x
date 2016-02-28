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
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLambda }
  [\+]                          { \s -> TokenAdd }
  [\-]                          { \s -> TokenSub }
  [\*]                          { \s -> TokenMul }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
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
  $alpha [$alpha $digit \_ \"]* { \s -> TokenSym s }

{

data Token = TokenLet
           | TokenIn
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
           | TokenShortVarDecl
           | TokenInc
           | TokenDec
           | TokenLCParen
           | TokenRCParen
           | TokenIf
           | TokenElse
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
