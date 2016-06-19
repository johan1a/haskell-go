module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified HappyParser
import AST


main = parseTests >>= defaultMain

parseTests = do
    t <- testList testFiles
    return $ hUnitTestToTests t


testFiles :: [String]
testFiles = ["var1",
            "varDecl1",
            "constDecl1",
            "typeDecl1",
            "ifStmt1",
            "block1"]
 
testList xs = do
    list <- mapM testLabel xs
    return $ TestList list
    
testLabel x = do
    test <- makeTest x
    return $ TestLabel x test

makeTest path = do
    x <- readFile $ "test/" ++ path ++ ".go"
    expected <- readFile $ "test/" ++ path ++ ".expected"
    let a = TestCase (assertEqual path expected $ testParse x)
    return a


verify :: IO ()
verify = do 
    tests1 <- testList testFiles
    _ <- runTestTT $ tests1
    return ()

testParse :: String -> String
testParse = show . HappyParser.parseExpr




