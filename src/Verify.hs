module Main where

import Data.Char (isSpace)
import Data.String.Utils
import System.Directory 
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified HappyParser
import AST

main = parseTests >>= defaultMain

parseTests :: IO [Test.Framework.Test]
parseTests = parseTestFileNames >>= testList >>= return . hUnitTestToTests

parseTestFileNames :: IO [String]
parseTestFileNames = testFilesIn "test/parse/"

-- Returns a list of all test filenames in a dir, without their file extensions.
testFilesIn :: String -> IO [String]
testFilesIn dir = listDirectory dir >>= return . map removeExtension . filter (endswith ".go") 

removeExtension :: String -> String 
removeExtension string = replace ".go" "" string 

testList :: [String] -> IO Test.HUnit.Test
testList xs = do
    list <- mapM  (makeTest "test/parse/") xs
    return $ TestList list
    
makeTest :: [Char] -> [Char] -> IO Test.HUnit.Test
makeTest path x = do
    test <- makeParsingTestCase (path ++ x)
    return $ TestLabel x test

makeParsingTestCase :: String -> IO Test.HUnit.Test
makeParsingTestCase path = do
    x <- readFile $ path ++ ".go"
    expected <- readFile $ path ++ ".expected"
    let a = TestCase (assertEqual path (rstrip expected) $ testParse x)
    return a

testParse :: String -> String
testParse = show . HappyParser.parseExpr

