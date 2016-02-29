module Main where

import qualified HappyParser
import AST
import Test.HUnit

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

parse = show . HappyParser.parseExpr 




testParse input = show $ HappyParser.parseExpr input

mapTuple f (a1, a2) = (f a1 a2)

inputPars = [("bla", "tmp")]

makeTests inputPairs = mapTuple doTest inputPairs

tests = do
    test1 <- (doTest "bla" "tmp")
    return $ TestList [TestLabel "test1" test1]

doTest expectedPath inputPath = do
    input <- readFile inputPath
    expected <- readFile expectedPath
    let a = TestCase (assertEqual inputPath expected $ testParse input)
    return a

verify :: IO ()
verify = do 
    tests1 <- tests
    _ <- runTestTT $ tests1
    return ()