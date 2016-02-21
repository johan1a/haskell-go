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
  "["                           { \s -> TokenLBracket }
  "]"                           { \s -> TokenRBracket }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

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
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
