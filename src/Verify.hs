module Main where

import Data.Char (isSpace)
import Data.String.Utils
import System.Directory 
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified HappyParser
import qualified Eval as E
import AST

main = parseTests >>= defaultMain

parseTests :: IO [Test.Framework.Test]
parseTests = parseTestFileNames >>= (testList makeParsingTestCase) >>= return . hUnitTestToTests

execTests :: IO [Test.Framework.Test]
execTests = execTestFileNames >>= (testList makeExecTestCase) >>= return . hUnitTestToTests

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
makeParsingTestCase path = compareExpected path testParse 

makeExecTestCase :: String -> IO Test.HUnit.Test
makeExecTestCase path = compareExpected path testRun

compareExpected :: String -> (String -> String -> IO String) -> IO Test.HUnit.Test
compareExpected path func = do
    src <- readFile $ path ++ ".go"
    expected <- readFile $ path ++ ".expected"
    output <- func path src
    let a = TestCase (assertEqual path (rstrip expected) $ output)
    return a

testParse :: String -> String -> IO String
testParse _ src = return $ show $ HappyParser.parseExpr src

testRun :: String -> String -> IO String
testRun path src = do
    st <- E.runTestProgram (path ++ ".out") $ HappyParser.parseExpr $ src ++ ".go"
    readFile (path ++ ".out")
















