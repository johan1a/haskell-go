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


