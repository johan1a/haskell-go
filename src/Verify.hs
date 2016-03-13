module Verify where

import qualified HappyParser
import AST
import Test.HUnit


testFiles :: [String]
testFiles = ["var1",
            "varDecl1",
            "constDecl1",
            "typeDecl1",
            "ifStmt1",
            "block1"]
 
testList :: [String] -> IO Test
testList xs = do
    list <- mapM testLabel xs
    return $ TestList list
    
testLabel :: String -> IO Test
testLabel x = do
    test <- makeTest x
    return $ TestLabel x test

makeTest :: String -> IO Test
makeTest path = do
    x <- readFile $ "test/" ++ path ++ ".go"
    expected <- readFile $ "test/" ++ path ++ ".expected"
    let a = TestCase (assertEqual path expected $ testParse x)
    return a

tests :: IO Test
tests = do 
    test1 <- makeTest "var1"
    return $ TestList [TestLabel "test1" test1]

verify :: IO ()
verify = do 
    tests1 <- testList testFiles
    _ <- runTestTT $ tests1
    return ()

testParse :: String -> String
testParse = show . HappyParser.parseExpr




