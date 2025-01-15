{-# LANGUAGE OverloadedStrings #-}

module Main where

import Regex.Parser (parseRegex)
import Regex.SyntaxChecker (checkRegex, CheckedRegex)
import GrammarCFG.CFG (CFG(..), Production(..), Symbol(..), Nonterminal, Terminal)
import GrammarCFG.GrammarBuilder (buildFrameGrammar)
import GrammarCFG.AttributedGrammar (buildAttributedGrammar, AttributedCFG(..), AttributedProduction(..), Attrib(..))
import Data.List (intercalate)
import Control.Monad (when)

main :: IO ()
main = do
  -- Примеры входных регулярных выражений:
  -- let input = "(a|b)(?2)(a(a(a(a(a(a(a(a(a(a))))))))))"
  -- let input = "(a|(bb))(?2)"
  -- let input = "((?:a(?2)|(bb))(?1))"
  -- let input = "((?1)a|b)"
  --let input = "(?1)(a|b)*(?1)"
  -- let input = "(aa(?1))"
  -- let input = "(a(?1)b|c)"  -- простой пример (регекс для языка {a^n c b^n | n ∈ N})
  --let input = "(?=a)(a|b)"
  input <- getLine
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
          let cfg = buildFrameGrammar goodAst
          putStrLn "Generated CFG:"
          printCFG cfg

          -- Построение атрибутной КС-грамматики
          let attCfg = buildAttributedGrammar goodAst
          putStrLn "Generated Attributed CFG:"
          printAttributedCFG attCfg

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

--------------------------------------------------------------------------------
-- | Функция для отображения атрибутной КС-грамматики в удобочитаемом виде
printAttributedCFG :: AttributedCFG -> IO ()
printAttributedCFG (AttributedCFG nts ts start prods) = do
  putStrLn $ "  Start symbol: " ++ start
  putStrLn $ "  Nonterminals: " ++ show nts
  putStrLn $ "  Terminals: " ++ show ts
  putStrLn "  Attributed Productions:"
  mapM_ printAttProd prods
  where
    printAttProd :: AttributedProduction -> IO ()
    printAttProd (AttributedProduction p inh syn) =
      putStrLn $ "    " ++ lhs p ++ " -> " ++ rhsStr p ++
                 " | Inherited: " ++ show inh ++
                 " | Synthesized: " ++ show syn
      where
        rhsStr :: Production -> String
        rhsStr (Production _ rhs) =
          if null rhs
            then "Epsilon"
            else intercalate " " (map showSymbol rhs)

        showSymbol :: Symbol -> String
        showSymbol (N nt) = nt
        showSymbol (T c)  = "'" ++ [c] ++ "'"
