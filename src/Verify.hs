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
import Debug.Trace

main = do
    p <- parseTests 
    exec <- execTests
    defaultMain (p ++ exec)

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
    expected <- fmap rstrip $ readFile $ path ++ ".expected"
    output <- func path src
    let a = TestCase (assertEqual path expected $ output)
    return a

myRstrip :: String -> String
myRstrip str = rstrip str

testParse :: String -> String -> IO String
testParse _ src = return $ show $ HappyParser.parseExpr src

testRun :: String -> String -> IO String
testRun path src = do
    writeFile (path ++ ".out") ""
    st <- E.runTestProgram (path ++ ".out") $ HappyParser.parseExpr src
    fmap rstrip $ readFile (path ++ ".out")
















