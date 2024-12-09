module Grammar (
    Grammar(..),
    Rule,
    Symbol(..),
    parseGrammar,
    printGrammar,
    parseRule,
    parseSymbol
) where

import Data.Char (isAlphaNum, isLower, isUpper)
import Data.List (intercalate)
import Control.Monad (guard)

-- Тип данных для символов грамматики
data Symbol = Terminal Char
            | NonTerminal String
            | Epsilon
            deriving (Eq, Show, Ord)

-- Тип данных для правил грамматики
type Rule = (Symbol, [Symbol])

-- Тип данных для грамматики
newtype Grammar = Grammar [Rule]
    deriving (Eq, Show)

-- Функция для парсинга строки в символ
parseSymbol :: String -> Maybe Symbol
parseSymbol s
    | null s = Nothing -- Handle empty strings
    | s == "ε" = Just Epsilon -- Handle epsilon
    | length s == 1 && isLower (head s) = Just (Terminal (head s)) -- Single lowercase char is a Terminal
    | all isAlphaNum s && isUpper (head s) = Just (NonTerminal s) -- Alphanumeric starting with uppercase is a NonTerminal
    | otherwise = Nothing

-- Функция для парсинга строки в правило
parseRule :: String -> Maybe Rule
parseRule s = do
    let parts = words s -- Split the string into words
    case break (== "->") parts of
        (lhs, "->":rhs) -> do
            -- Ensure the left-hand side (lhs) has exactly one valid non-terminal
            guard (length lhs == 1)
            leftSymbol <- parseNonTerminal (head lhs) -- Parse the single non-terminal
            -- Ensure the right-hand side (rhs) is not empty and valid
            guard (not (null rhs))
            rightSymbols <- mapM parseSymbol rhs -- Parse the right-hand side symbols
            return (leftSymbol, rightSymbols)
        _ -> Nothing -- Invalid format

-- Helper function to parse a NonTerminal
parseNonTerminal :: String -> Maybe Symbol
parseNonTerminal s
  | all isAlphaNum s && isUpper (head s) = Just (NonTerminal s)
  | otherwise = Nothing

-- Функция для парсинга строки в грамматику
parseGrammar :: String -> Maybe Grammar
parseGrammar s = do
    let linesOfGrammar = lines s -- Split grammar into lines
    rules <- mapM parseRule linesOfGrammar -- Parse each line into a rule
    return (Grammar rules)

-- Функция для печати грамматики
printGrammar :: Grammar -> String
printGrammar (Grammar rules) = intercalate "\n" (map printRule rules)
  where
    printRule (left, right) = printSymbol left ++ " -> " ++ unwords (map printSymbol right)

    printSymbol :: Symbol -> String
    printSymbol (Terminal c) = [c]
    printSymbol (NonTerminal nt) = nt
    printSymbol Epsilon = "ε"