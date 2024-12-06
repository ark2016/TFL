module Grammar (
    Grammar,
    Rule,
    Symbol(..),
    parseGrammar,
    printGrammar
) where

import Data.Char (isAlphaNum, isLower, isUpper)
import Data.List (intercalate)
import Text.Read (readMaybe)
import Control.Monad (guard)

-- Тип данных для символов грамматики
data Symbol = Terminal Char
            | NonTerminal String
            | Epsilon -- Добавляем символ для пустых правил
            deriving (Eq, Show)

-- Тип данных для правил грамматики
type Rule = (Symbol, [Symbol])

-- Тип данных для грамматики
newtype Grammar = Grammar [Rule]
    deriving (Show)

-- Добавляем инстанс Eq для Grammar
instance Eq Grammar where
  (Grammar g1) == (Grammar g2) = g1 == g2

-- Функция для парсинга строки в символ
parseSymbol :: String -> Maybe Symbol
parseSymbol s
    | all isLower s && length s == 1 = Just (Terminal (head s))
    | all isAlphaNum s && isUpper (head s) = Just (NonTerminal s)
    | s == "ε" = Just Epsilon -- Добавляем поддержку для epsilon
    | otherwise = Nothing

-- Функция для парсинга строки в правило
parseRule :: String -> Maybe Rule
parseRule s = do
    let parts = words s
    case parts of
        (left: "->": right) -> do
            leftSymbol <- parseNonTerminal left
            rightSymbols <- mapM parseSymbol right
            return (leftSymbol, rightSymbols)
        _ -> Nothing

parseNonTerminal :: String -> Maybe Symbol
parseNonTerminal s
  | all isAlphaNum s && isUpper (head s) = Just (NonTerminal s)
  | otherwise = Nothing

-- Функция для парсинга строки в грамматику
parseGrammar :: String -> Maybe Grammar
parseGrammar s = do
    let linesOfGrammar = lines s
    rules <- mapM parseRule linesOfGrammar
    return (Grammar rules)

-- Функция для печати грамматики
printGrammar :: Grammar -> String
printGrammar (Grammar rules) = intercalate "\n" (map printRule rules)
    where
        printRule (left, right) = show left ++ " -> " ++ intercalate " " (map printSymbol right)

        printSymbol :: Symbol -> String
        printSymbol (Terminal c) = [c]
        printSymbol (NonTerminal nt) = nt
        printSymbol Epsilon = "ε"

-- Пример использования
main :: IO ()
main = do
    let grammarStr = "S -> AB\nA -> a\nB -> b"
    case parseGrammar grammarStr of
        Just grammar -> putStrLn (printGrammar grammar)
        Nothing -> putStrLn "Failed to parse grammar"