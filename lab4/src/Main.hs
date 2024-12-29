import Regex.Parser
import Regex.SyntaxChecker

main :: IO ()
main = do
  let input = "(a|b)(?1)"  -- простой пример
  case parseRegex input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right ast -> do
      putStrLn $ "Parsed AST: " ++ show ast
      case checkRegex ast of
        Left e    -> putStrLn $ "Check error: " ++ show e
        Right ast' -> do
          putStrLn "Regex is OK"
          putStrLn $ "Checked AST: " ++ show ast'
