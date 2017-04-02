module Main where

import qualified HappyParser
import Data.Map
import System.Environment
import AST
import Test.HUnit
import Eval
import Debug.Trace

import AlexToken

import Control.Monad.Trans

import Gho

main :: IO Eval.State
main = fmap head getArgs >>= runFile

runFile :: String -> IO Eval.State
runFile f = do
    cont <- readFile f 
    case (parseGho cont) of 
        (Right a) -> runProgram a
        (Left b) -> error $ "Parse error: " ++ b

    

