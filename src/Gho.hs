module Gho where


import HappyParser
import Data.Map
import System.Environment
import AST
import Test.HUnit
import Eval
import Debug.Trace

import AlexToken

import Control.Monad.Except
 
parseGho :: String -> Either String SourceFile
parseGho s = case scan2 s of
    (Right a) -> HappyParser.parse (reverse a)
    (Left x) -> error $ "scan error: " ++ x

scan2 :: String -> Either String [Lexeme Token]
scan2 s = runLexer s (process [])

scan :: String -> IO [Lexeme Token]
scan s = case runLexer s (process []) of
                (Right a) -> return $ reverse a
                (Left s) -> error $ "scan error: " ++ s

process ls = do
    lexeme@(Lexeme tok tp) <- alexMonadScan
    case tok of
        TokenEOF -> return ls
        TokenNewLine -> do
            process $! insertSemicolon ls --Throw away the newlines
        _ -> do
            process $! (lexeme:ls)

--When the input is broken into tokens, a semicolon is automatically inserted into the token stream immediately after a line's final token if that token is
-- an identifier
-- an integer, floating-point, imaginary, rune, or string literal
--one of the keywords break, continue, fallthrough, or return
--one of the operators and delimiters ++, --, ), ], or }

-- Inserts a semicolon based on the previous token
insertSemicolon :: [Lexeme Token] -> [Lexeme Token]
insertSemicolon [] = []
insertSemicolon ls@((Lexeme prev _):_) = case prev of
    (TokenSym _) -> semicolon:ls
    (TokenInt _) -> semicolon:ls 
    (TokenFloat _) -> semicolon:ls 
    (TokenString _) -> semicolon:ls 
    (TokenBreak) -> semicolon:ls
    (TokenContinue) -> semicolon:ls
    (TokenFallthrough) -> semicolon:ls
    (TokenReturn) -> semicolon:ls
    (TokenInc) -> semicolon:ls
    (TokenDec) -> semicolon:ls
    (TokenRParen) -> semicolon:ls
    (TokenRBracket) -> semicolon:ls
    (TokenRCParen) -> semicolon:ls
    _ -> ls


semicolon = Lexeme TokenSemicolon todoPos



