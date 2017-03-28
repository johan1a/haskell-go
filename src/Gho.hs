module Gho where


import HappyParser
import Data.Map
import System.Environment
import AST
import Test.HUnit
import Eval
import Debug.Trace

import AlexToken

run2 :: String -> IO ()
run2 fileName = do
    content <- readFile fileName
    state <- scan content
    putStrLn (show state)
    return ()

scan :: String -> IO [Lexeme Token]
scan s = case runLexer s (loop []) of
                Right a -> return a
                Left s -> error s

loop ls = do
    token@(Lexeme td tp) <- alexMonadScan
    case td of
        TokenEOF -> return ls
        otherwise -> do
            loop $! (token:ls)



