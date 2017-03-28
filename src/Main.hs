module Main where

import qualified HappyParser
import Data.Map
import System.Environment
import AST
import Test.HUnit
import Eval
import Debug.Trace

import AlexToken

import Gho

runAndShow :: String -> IO ()
runAndShow input = do
  ast <- Gho.mainParse input
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
    state <- Gho.mainParse content >>= runProgram
    return ()

run :: String -> IO ()
run fileName = do
    content <- readFile fileName
    state <- Gho.mainParse content >>= runProgram
    return ()
    
--parse :: String -> String
--parse = show . HappyParser.parse 

parseFile :: String -> IO ()
parseFile fileName = do 
    content <- readFile $ fileName
    ast <- Gho.mainParse content
    putStrLn $ "AST: " ++ (show ast)
    return ()


