module Main where

import qualified HappyParser
import System.Environment
import AST
import Test.HUnit
import Verify
import Eval

runEvalWith :: (String -> [Statement]) -> String -> IO ()
runEvalWith parseExpr input = do
  let ast = parseExpr input
  putStrLn $ "AST: " ++ (show ast)

badRepl :: IO ()
badRepl = do
  putStrLn "Enter stuff:"
  input <- getLine  
  putStrLn "Input:"
  putStrLn "\nUsing Happy:"
  runEvalWith HappyParser.parseExpr input
  return ()

main :: IO ()
main = do 
    args <- getArgs
    content <- readFile $ args !! 0
    putStrLn content
    return ()

parse :: String -> String
parse = show . HappyParser.parseExpr 
