import qualified HappyParser
import AST

runEvalWith :: (String -> Stmt) -> String -> IO ()
runEvalWith parseExpr input = do
  let ast = parseExpr input
  putStrLn $ "AST: " ++ (show ast)

main :: IO ()
main = do
  input <- getContents
  putStrLn "Input:"
  putStrLn input
  putStrLn "\nUsing Happy:"
  runEvalWith HappyParser.parseExpr input
