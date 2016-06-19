module Main where

import AST
import Test.HUnit
import System.Exit


main :: IO ()
main = do
  results <- runTestTT $ tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)

tests = TestList $ map TestCase testCases 

testCases = [
--    [assertEqual "Num" "Num 1" $ show $ num,
  --  assertEqual "Var" "Var \"s\"" $ show $ var,
   -- assertEqual "" "Type \"type\"" $ show $ Type "type"
      ]

