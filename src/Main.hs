module Main where

import qualified HappyParser
import AST
import Test.HUnit
import Verify
import Eval

runEvalWith :: (String -> [Statement]) -> String -> IO ()
runEvalWith parseExpr input = do
  let ast = parseExpr input
  putStrLn $ "AST: " ++ (show ast)

main :: IO ()
main = do
  putStrLn "Enter stuff:"
  input <- getLine  
  putStrLn "Input:"
  putStrLn "\nUsing Happy:"
  runEvalWith HappyParser.parseExpr input
  return ()


parse :: String -> String
parse = show . HappyParser.parseExpr 
