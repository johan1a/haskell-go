module Main where

import qualified HappyParser
import AST
import Test.HUnit
import Verify

runEvalWith :: (String -> [Statement]) -> String -> IO ()
runEvalWith parseExpr input = do
  let ast = parseExpr input
  putStrLn $ "AST: " ++ (show ast)

main :: IO ()
main = do
  putStrLn "Enter stuff:"
  input <- getContents
  putStrLn "Input:"
  putStrLn input
  putStrLn "\nUsing Happy:"
  runEvalWith HappyParser.parseExpr input


parse :: String -> String
parse = show . HappyParser.parseExpr 
