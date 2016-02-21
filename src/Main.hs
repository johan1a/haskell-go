import qualified HappyParser
import Expr
import Pretty

runEvalWith :: (String -> Expr) -> String -> IO ()
runEvalWith parseExpr input = do
  let ast = parseExpr input
  putStrLn $ "AST: " ++ (show ast)
  putStrLn $ "Source: " ++ (pretty ast)

main :: IO ()
main = do
  input <- getContents
  putStrLn "Input:"
  putStrLn input
  putStrLn "\nUsing Happy:"
  runEvalWith HappyParser.parseExpr input
