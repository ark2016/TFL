{-# LANGUAGE OverloadedStrings #-}

module Main where

import Regex.Parser (parseRegex)
import Regex.SyntaxChecker (checkRegex, CheckedRegex)
import GrammarCFG.CFG (CFG(..), Production(..), Symbol(..), Nonterminal, Terminal)
import GrammarCFG.GrammarBuilder (buildFrameGrammar)
import Data.List (intercalate)

main :: IO ()
main = do
  -- Примеры входных регулярных выражений:
--  let input = "(a|b)(?2)(a(a(a(a(a(a(a(a(a(a))))))))))"
--  let input = "(a|(bb))(?2)"
--  let input = "((?:a(?2)|(bb))(?1))"
--  let input = "((?1)a|b)"
  let input = "(?1)(a|b)*(?1)"
--  let input = "(aa(?1))"
--  let input = "(a(?1)a|b)"  -- простой пример (регекс для языка {a^n c b^n | n ∈ N})
  putStrLn $ "Input regex: " ++ input

  -- Парсинг
  case parseRegex input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right ast -> do
      putStrLn $ "Parsed AST: " ++ show ast

      -- Проверка (присвоение группам номеров, проверка look-ahead и т.д.)
      case checkRegex ast of
        Left checkErr -> putStrLn $ "Check error: " ++ show checkErr
        Right goodAst -> do
          putStrLn "Regex is OK"
          putStrLn $ "Checked AST: " ++ show goodAst

          -- Построение КС-грамматики
          let cfg = buildFrameGrammar ast --goodAst
          putStrLn "Generated CFG:"
          printCFG cfg

--------------------------------------------------------------------------------
-- | Функция для отображения КС-грамматики в удобочитаемом виде
printCFG :: CFG -> IO ()
printCFG (CFG nonterms terms start prods) = do
  putStrLn $ "  Start symbol: " ++ start
  putStrLn $ "  Nonterminals: " ++ show nonterms
  putStrLn $ "  Terminals: " ++ show terms
  putStrLn "  Productions:"
  mapM_ printProd prods
  where
    printProd :: Production -> IO ()
    printProd (Production lhs rhs) =
      putStrLn $ "    " ++ lhs ++ " -> " ++
        (if null rhs
           then "Epsilon"
           else intercalate " " (map showSymbol rhs))
    
    showSymbol :: Symbol -> String
    showSymbol (N nt) = nt
    showSymbol (T c)  = "'" ++ [c] ++ "'"
