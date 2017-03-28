{
{-# OPTIONS_GHC -w #-}
module AlexToken (
    Token(..),
    Lexeme(..),
    alexMonadScan,
    runLexer
) where

import AST

}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-ZåäöÅÄÖ]
$eol   = [\n]

tokens :-

  $eol                          { skip }
  $white+                       { skip }
  "#".*                         { skip }
  let                           { alex(const TokenLet )}
  "]"                           { alex(const TokenRBracket )}
  "["                           { alex(const TokenLBracket )} 
  "."                           { alex(const TokenDot )}
  "->"                          { alex(const TokenArrow )}
  "=="    			            { alex(const TokenEq2 )}
  "!="    			            { alex(const TokenNeq )}
  "<"     			            { alex(const TokenLess )}
  "<="    			            { alex(const TokenLessEq) }
  ">"     			            { alex(const TokenGreater) }
  ">="    			            { alex(const TokenGreaterEq) }
  "true"    			        { alex(const TokenTrue )}
  "false"    			        { alex(const TokenFalse) }
  "func"    			        { alex(const TokenFunc )}
  "return"    			        { alex(const TokenReturn )}
  "struct"    			        { alex(const TokenStruct )}
  \=                            { alex(const TokenEq )}
  \\                            { alex(const TokenLambda) }
  \(                            { alex(const TokenLParen )}
  \)                            { alex(const TokenRParen )}
  "print" 			            { alex(const TokenPrint )}
  "println" 			        { alex(const TokenPrintLn) }
  "const"                       { alex(const TokenConst )}
  "type"                        { alex(const TokenType )}
  "var"                         { alex(const TokenVar )}
  "..."                         { alex(const TokenDots )}
    "."                         { alex(const TokenDot )}
    ","                         { alex(const TokenComma) }
    "+"                         { alex(const TokenAdd )}
    "-"                         { alex(const TokenSub )}
    "|"                         { alex(const TokenOpPipe )}
    "^"                         { alex(const TokenOpUpArrow) }
    "*"                         { alex(const TokenOpMul )}
    "/"                         { alex(const TokenOpSlash )}
    "%"                         { alex(const TokenOpModulo )}
    "<<"                        { alex(const TokenOpLeftStream) }
    ">>"                        { alex(const TokenOpRightStream) }
    "&"                         { alex(const TokenOpAnd )}
    "&^"                        { alex(const TokenOpAndUp )}
    ":="                        { alex(const TokenShortVarDecl) }
    "++"                        { alex(const TokenInc )}
    "--"                        { alex(const TokenDec )}
    "{"                         { alex(const TokenLCParen) }
    "}"                         { alex(const TokenRCParen) }
    "if"                        { alex(const TokenIf )}
    "else"                      { alex(const TokenElse) }
    "package"                   { alex(const TokenPackage) }
    ";"                         { alex(const TokenSemiColon) }
    $digit+                     { alex (TokenNum . read) }
    \"$alpha [$white $alpha $digit \_ ]*\"           { alex( TokenString . stripQuotes . read ) }
    $alpha [$alpha $digit \_ ]*                        { alex TokenSym  }
    .                           { alex TokenError}

{

stripQuotes :: String -> String
stripQuotes s@[c]                     = s 
stripQuotes ('"':s)  | last s == '"'  = init s
            | otherwise      	      = s
stripQuotes ('\'':s) | last s == '\'' = init s
            | otherwise               = s
stripQuotes s                         = s



data Token = TokenError {unknown :: String}
       | TokenLet
       | TokenPrint 
       | TokenPrintLn
       | TokenReturn
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
       | TokenStruct
       | TokenType
       | TokenVar
       | TokenLambda
       | TokenNum { tnNumber :: Int }
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
       | TokenEOF
       deriving (Eq,Show)

data Lexeme a = Lexeme { lData :: a, lPos  :: AlexPosn } 

alex :: (String -> Token) -> AlexInput -> Int -> Alex (Lexeme Token)
alex tokenFunc (pos, prevChar, byteRest, input) k = return (Lexeme (tokenFunc $ take k input) pos)

alexEOF :: Alex (Lexeme Token)
alexEOF = return $ Lexeme TokenEOF (AlexPn 0 0 0)

state :: String -> AlexState
state input = AlexState {
    alex_pos = alexStartPos, -- position at current input location
    alex_inp = input,        -- the current input
    alex_chr = '\n',          -- the character before the input
    alex_bytes = [],         -- 
    alex_scd = 0             -- the current startcode
}

runLexer :: String -> Alex a -> Either String a
runLexer input (Alex f) = case f (state input) of
    Left msg -> Left msg
    Right ( _, a ) -> Right a
}





