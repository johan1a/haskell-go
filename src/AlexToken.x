{
{-# OPTIONS_GHC -w #-}
module AlexToken (
    Token(..),
    Lexeme(..),
    alexMonadScan,
    runLexer,
    todoPos
) where

import AST

}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

-- newline is not whitespace because of semicolon insertion reasons.
$whitespace = [\ \t\r\f\v] 


@float_lit = $digit \. $digit+

tokens :-

  $eol                          { alex(const TokenNewLine) }
  $whitespace+                  { skip }
  "#".*                         { skip }
  let                           { alex(const TokenLet )}
  "]"                           { alex(const TokenRBracket )}
  "["                           { alex(const TokenLBracket )} 
  "."                           { alex(const TokenDot )}
  "->"                          { alex(const TokenArrow )}
  "<-"                          { alex(const TokenLeftArrow )}
  "=="                          { alex(const TokenEq2 )}
  "!="                          { alex(const TokenNeq )}
  "!"                           { alex(const TokenExclamation )}
  "<"                           { alex(const TokenLess )}
  "<="                          { alex(const TokenLessEq) }
  ">"                           { alex(const TokenGreater) }
  ">="                          { alex(const TokenGreaterEq) }
  "||"                          { alex(const TokenOr )                  }
  "func"                        { alex(const TokenFunc )}
  "return"                      { alex(const TokenReturn )}
  "struct"                      { alex(const TokenStruct )}
  "interface"                   { alex(const TokenInterface )}
  "import"                      { alex(const TokenImport               )}
  "map"                         { alex(const TokenMap )}
  \=                            { alex(const TokenEq )}
  \\                            { alex(const TokenLambda) }
  "("                           { alex(const TokenLParen )}
  ")"                           { alex(const TokenRParen )}
  "const"                       { alex(const TokenConst )}
  "type"                        { alex(const TokenType )}
  "var"                         { alex(const TokenVar )}
  "break"                       { alex(const TokenVar )}
  "continue"                    { alex(const TokenVar )}
  "fallthrough"                 { alex(const TokenVar )}
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
    "&&"                        { alex(const TokenAnd2 )}
    "<<"                        { alex(const TokenOpLeftStream) }
    ">>"                        { alex(const TokenOpRightStream) }
    "&"                         { alex(const TokenAnd1 )}
    "&^"                        { alex(const TokenOpAndUp )}
    ":="                        { alex(const TokenShortVarDecl) }
    "++"                        { alex(const TokenInc )}
    "--"                        { alex(const TokenDec )}
    ":"                         { alex(const TokenColon )}
    "{"                         { alex(const TokenLCParen) }
    "}"                         { alex(const TokenRCParen) }
    "if"                        { alex(const TokenIf )}
    "else"                      { alex(const TokenElse) }
    "package"                   { alex(const TokenPackage) }
    ";"                         { alex(const TokenSemicolon) }
    @float_lit                  { alex (TokenFloat . read) }
    $digit+                     { alex (TokenInt . read) }
    \"[$whitespace $alpha $digit \_]*\"           { alex( TokenString . stripQuotes . read ) }
    $alpha [$alpha $digit \_ ]*                        { alex TokenSym  }
    .                           { alex TokenError}

{

stripQuotes :: String -> String
stripQuotes s@[c]                     = s 
stripQuotes ('"':s)  | last s == '"'  = init s
            | otherwise               = s
stripQuotes ('\'':s) | last s == '\'' = init s
            | otherwise               = s
stripQuotes s                         = s



data Token = TokenError {unknown :: String}
       | TokenNewLine
       | TokenBreak
       | TokenContinue
       | TokenFallthrough
       | TokenInterface
       | TokenOr
       | TokenAnd2
       | TokenLet
       | TokenPrint 
       | TokenPrintLn
       | TokenReturn
       | TokenTrue
       | TokenFalse
       | TokenMap
       | TokenChan
       | TokenLeftArrow
       | TokenImport
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
       | TokenInt { tInt :: Int }
       | TokenFloat { tFloat :: Double }
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
       | TokenAnd1
       | TokenOpAndUp 
       | TokenShortVarDecl
       | TokenExclamation
       | TokenInc
       | TokenDec
       | TokenLCParen
       | TokenRCParen
       | TokenColon
       | TokenSemicolon
       | TokenIf
       | TokenElse
       | TokenPackage
       | TokenEOF
       deriving (Eq,Show)

data Lexeme a = Lexeme { lData :: a, lPos  :: AlexPosn } 

instance Show a => Show (Lexeme a) where
    show l = show (lPos l) ++ ": " ++ show (lData l)

alex :: (String -> Token) -> AlexInput -> Int -> Alex (Lexeme Token)
alex tokenFunc (pos, prevChar, byteRest, input) k = return (Lexeme (tokenFunc $ take k input) pos)

alexEOF :: Alex (Lexeme Token)
alexEOF = return $ Lexeme TokenEOF (AlexPn 0 0 0)

todoPos :: AlexPosn
todoPos = AlexPn 0 0 0

state :: String -> AlexState
state input = AlexState {
    alex_pos = alexStartPos, -- position at current input location
    alex_inp = input,        -- the current input
    alex_chr = ' ',          -- the character before the input
    alex_bytes = [],         -- 
    alex_scd = 0             -- the current startcode
}

runLexer :: String -> Alex a -> Either String a
runLexer input (Alex f) = case f (state input) of
    Left msg -> Left msg
    Right ( _, a ) -> Right a
}





