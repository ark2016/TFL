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

-- Тип данных для символов грамматики
data Symbol = Terminal Char
            | NonTerminal String
            deriving (Eq, Show)

-- Тип данных для правил грамматики
type Rule = (Symbol, [Symbol])

-- Тип данных для грамматики
newtype Grammar = Grammar [Rule]
    deriving (Show)

-- Функция для парсинга строки в символ
parseSymbol :: String -> Maybe Symbol
parseSymbol s
    | all isLower s && length s == 1 = Just (Terminal (head s))
    | all isAlphaNum s && isUpper (head s) = Just (NonTerminal s)
    | otherwise = Nothing

-- Функция для парсинга строки в правило
parseRule :: String -> Maybe Rule
parseRule s = do
    let parts = words s
    case parts of
        [left, "->", right] -> do
            let leftSymbol = NonTerminal left
            rightSymbols <- mapM parseSymbol (words right)
            return (leftSymbol, rightSymbols)
        _ -> Nothing

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
        printRule (left, right) = show left ++ " -> " ++ intercalate " " (map show right)

-- Пример использования
main :: IO ()
main = do
    let grammarStr = "S -> AB\nA -> a\nB -> b"
    case parseGrammar grammarStr of
        Just grammar -> putStrLn (printGrammar grammar)
        Nothing -> putStrLn "Failed to parse grammar"
