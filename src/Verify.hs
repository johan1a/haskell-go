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
parseTests = parseTestFileNames >>= (testList makeParsingTestCase)>>= return . hUnitTestToTests

--execTests :: IO [Test.Framework.Test]
--- execTests = execTestFileNames >>= testList >>= return . hUnitTestToTests

parseTestFileNames :: IO [String]
parseTestFileNames = testFilesIn "test/parse/"

execTestFileNames :: IO [String]
execTestFileNames = testFilesIn "test/execution/"

-- Returns a list of all test filenames in a dir, without their file extensions.
testFilesIn :: String -> IO [String]
testFilesIn dir = listDirectory dir >>= return . map (dir ++) . map removeExtension . filter (endswith ".go") 

removeExtension :: String -> String 
removeExtension string = replace ".go" "" string 

testList :: (String -> IO Test.HUnit.Test) -> [String] -> IO Test.HUnit.Test
testList testFunc xs = do
    list <- mapM (makeTest testFunc) xs
    return $ TestList list
    
makeTest :: (String -> IO Test.HUnit.Test) -> String -> IO Test.HUnit.Test
makeTest testFunc x = testFunc x >>= return . TestLabel x

makeParsingTestCase :: String -> IO Test.HUnit.Test
makeParsingTestCase path = do
    x <- readFile $ path ++ ".go"
    expected <- readFile $ path ++ ".expected"
    let a = TestCase (assertEqual path (rstrip expected) $ testParse x)
    return a

testParse :: String -> String
testParse = show . HappyParser.parseExpr

