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
  ast <- parseGho input 
  what <- runProgram ast
  putStrLn $ "AST: " ++ (show ast)

badRepl :: IO ()
badRepl = do
  putStrLn "Enter stuff:"
  getLine >>= runAndShow
  badRepl

main :: IO Eval.State
main = fmap head getArgs >>= runFile

runFile :: String -> IO Eval.State
runFile f = readFile f >>= parseGho >>= runProgram
    

