module Main where

import AST
import Test.HUnit

tests = TestList [TestLabel "test1" test1]
test1 = TestCase (assertEqual "for (foo 3)," (1 + 2) (3))

main :: IO ()
main = do _ <- runTestTT $ tests
          return ()

