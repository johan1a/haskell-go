module Main where

import qualified HappyParser
import Data.Map
import System.Environment
import AST
import Test.HUnit
import Eval
import Debug.Trace

import AlexToken

runAndShow :: String -> IO ()
runAndShow input = do
  let ast = HappyParser.parseExpr input
  what <- runProgram ast
  putStrLn $ "AST: " ++ (show ast)

badRepl :: IO ()
badRepl = do
  putStrLn "Enter stuff:"
  input <- getLine  
  runAndShow input
  return ()

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ args !! 0
    state <- runProgram $ HappyParser.parseExpr content
    return ()

run :: String -> IO ()
run fileName = do
    content <- readFile fileName
    state <- runProgram $ HappyParser.parseExpr content
    return ()
    

parse :: String -> String
parse = show . HappyParser.parseExpr 

parseFile :: String -> IO ()
parseFile fileName = do 
    content <- readFile $ fileName
    let ast = HappyParser.parseExpr content
    putStrLn $ "AST: " ++ (show ast)
    return ()


run2 :: String -> IO ()
run2 fileName = do
    content <- readFile fileName
    state <- analyse content
    putStrLn (show state)
    return ()


analyse :: String -> IO [Lexeme Token]
analyse s = case runLexer s (loop []) of
                Right a -> return a
                Left s -> error s

loop ls = do
    token@(Lexeme td tp) <- alexMonadScan
    case td of
        TokenEOF -> return ls
        otherwise -> do
            loop $! (token:ls)



