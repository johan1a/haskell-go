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
  "<="    			            { \s -> TokenLessEq }
  ">"     			            { \s -> TokenGreater }
  ">="    			            { \s -> TokenGreaterEq }
  "true"    			        { \s -> TokenTrue }
  "false"    			        { \s -> TokenFalse }
  "func"    			        { \s -> TokenFunc }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLambda }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  "print" 			            { \s -> TokenPrint }
  "println" 			        { \s -> TokenPrintLn }
  "const"                       { \s -> TokenConst }
  "type"                        { \s -> TokenType }
  "var"                         { \s -> TokenVar }
  "..."                         { \s -> TokenDots }
    "."                         { \s -> TokenDot }
    ","                         { \s -> TokenComma }
    "+"                         { \s -> TokenAdd }
    "-"                         { \s -> TokenSub }
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
    "package"                   { \s -> TokenPackage }
    ";"                         { \s -> TokenSemiColon }
    \"$alpha [$white $alpha $digit \_ ]*\"             { \s -> TokenString (stripQuotes s) }
    $alpha [$alpha $digit \_ ]*   { \s -> TokenSym s }

{

stripQuotes :: String -> String
stripQuotes s@[c]                     = s 
stripQuotes ('"':s)  | last s == '"'  = init s
            | otherwise      	      = s
stripQuotes ('\'':s) | last s == '\'' = init s
            | otherwise               = s
stripQuotes s                         = s



data Token = TokenLet
       | TokenPrint 
       | TokenPrintLn
           | TokenTrue
           | TokenFalse
           | TokenIn
	   | TokenString String
	   | TokenEq2
	   | TokenNeq
	   | TokenLess
	   | TokenLessEq
	   | TokenGreater
	   | TokenGreaterEq
	   | TokenFunc
       | TokenConst
       | TokenType
       | TokenVar
       | TokenLambda
       | TokenNum Int
       | TokenSym String
       | TokenArrow
       | TokenEq
       | TokenLParen
       | TokenRParen
       | TokenLBracket
       | TokenRBracket
       | TokenDots
       | TokenDot 
       | TokenComma
       | TokenAdd
       | TokenSub
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
       | TokenSemiColon
       | TokenIf
       | TokenElse
       | TokenPackage
       deriving (Eq,Show)

scanTokens = alexScanTokens

}
