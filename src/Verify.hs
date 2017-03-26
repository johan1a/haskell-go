module Main where

import Data.Char (isSpace)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified HappyParser
import AST



main = parseTests >>= defaultMain

parseTests :: IO [Test.Framework.Test]
parseTests = testList testFiles >>= return . hUnitTestToTests


testFiles :: [String]
testFiles = ["var1",
            "varDecl1",
            "varDecl2",
            "varDecl3",
            "constDecl1",
            "funcDecl1",
            "funcDecl2",
            "funcDecl3",
            "typeDecl1",
            "typeDecl2",
            "subExpr1",
            "string1",
            "stringDecl1",
            "ifStmt1",
            "ifStmt2",
            "ifStmt3",
            "ifStmt4",
            "ifStmt5",
            "ifStmt6",
            "block1"]
 
testList :: [String] -> IO Test.HUnit.Test
testList xs = do
    list <- mapM makeTestLabel xs
    return $ TestList list
    
makeTestLabel :: [Char] -> IO Test.HUnit.Test
makeTestLabel x = do
    test <- makeParsingTestCase x
    return $ TestLabel x test

makeParsingTestCase path = do
    x <- readFile $ "test/" ++ path ++ ".go"
    expected <- readFile $ "test/" ++ path ++ ".expected"
    let a = TestCase (assertEqual path (rstrip expected) $ testParse x)
    return a


verify :: IO ()
verify = do 
    tests1 <- testList testFiles
    _ <- runTestTT $ tests1
    return ()

testParse :: String -> String
testParse = show . HappyParser.parseExpr


rstrip :: String -> String 
rstrip = reverse . dropWhile isSpace . reverse
