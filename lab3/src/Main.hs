{-# LANGUAGE OverloadedStrings #-}

module Main where

import TestGenerator
import Grammar
import CYKParser

import Control.Monad (forM_, forM, when)
import System.Environment (getArgs)
import System.IO
import Data.List (intercalate)
import System.Exit (exitSuccess)

-- | Структура для хранения информации о грамматике
data NamedGrammar = NamedGrammar {
    grammarName :: String,
    grammarDef :: Grammar,
    maxDepth :: Int,
    numNegatives :: Int
}

-- | Список доступных грамматик
grammars :: [NamedGrammar]
grammars = [
        --Грамматика S -> a
        NamedGrammar "S -> a" (Grammar [
                (NonTerminal "S", [Terminal 'a'])
            ]) 1 1,

        --Грамматика S -> A C | ε, C -> S B, A -> a, B -> b
        NamedGrammar "S -> A C | epsilon, C -> S B, A -> a, B -> b" (Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "C"]),
                (NonTerminal "S", [Epsilon]),
                (NonTerminal "C", [NonTerminal "S", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
            ]) 3 2,

        --Грамматика S -> A X | ε, X -> B Y, Y -> C, A -> a, B -> b, C -> c
        NamedGrammar "S -> A X | epsilon, X -> B Y, Y -> C, A -> a, B -> b, C -> c" (Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "X"]),
                (NonTerminal "S", [Epsilon]),
                (NonTerminal "X", [NonTerminal "B", NonTerminal "Y"]),
                (NonTerminal "Y", [NonTerminal "C"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "C", [Terminal 'c'])
            ]) 3 2,

        --Грамматика S -> A C | S S | ε, C -> S B, A -> a, B -> b
        NamedGrammar "S -> A C | S S | epsilon, C -> S B, A -> a, B -> b" (Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "C"]),
                (NonTerminal "S", [NonTerminal "S", NonTerminal "S"]),
                (NonTerminal "S", [Epsilon]),
                (NonTerminal "C", [NonTerminal "S", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
            ]) 3 2,

        --Грамматика сбалансированных скобок S -> O S D | S S | ε, O -> (, D -> )
        NamedGrammar "S -> O S D | S S | epsilon, O -> (, D -> )" (Grammar [
                (NonTerminal "S", [NonTerminal "O", NonTerminal "S", NonTerminal "D"]),
                (NonTerminal "S", [NonTerminal "S", NonTerminal "S"]),
                (NonTerminal "S", [Epsilon]),
                (NonTerminal "O", [Terminal '(']),
                (NonTerminal "D", [Terminal ')'])
            ]) 4 3
    ]

-- | Вывод списка грамматик
displayGrammars :: [NamedGrammar] -> IO ()
displayGrammars gs = do
    putStrLn "Доступные грамматики:"
    forM_ (zip [1..] gs) $ \(i, g) -> do
        putStrLn $ show i ++ ". " ++ grammarName g

-- | Чтение выбора грамматики от пользователя
chooseGrammar :: [NamedGrammar] -> IO NamedGrammar
chooseGrammar gs = do
    displayGrammars gs
    putStrLn "Выберите номер грамматики для тестирования:"
    input <- getLine
    case reads input :: [(Int, String)] of
        [(n, "")] | n >= 1 && n <= length gs -> return (gs !! (n - 1))
        _ -> do
            putStrLn "Некорректный ввод. Попробуйте снова."
            chooseGrammar gs

-- | Запрос параметров тестирования от пользователя
getTestParameters :: IO (Int, Int)
getTestParameters = do
    putStrLn "Введите максимальную глубину генерации положительных примеров (целое число):"
    depthInput <- getLine
    putStrLn "Введите количество отрицательных примеров (целое число):"
    negInput <- getLine
    case (reads depthInput, reads negInput) of
        ([(d, "")], [(n, "")]) | d >=0 && n >=0 -> return (d, n)
        _ -> do
            putStrLn "Некорректный ввод. Попробуйте снова."
            getTestParameters

-- | Запись результатов тестов в файл
writeResultsToFile :: String -> [(String, Bool, Bool)] -> IO ()
writeResultsToFile filename results = do
    handle <- openFile filename WriteMode
    hPutStrLn handle "Строка,Ожидалось,Получено,Результат"
    forM_ results $ \(s, expected, actual) -> do
        let status = if expected == actual then "PASS" else "FAIL"
        hPutStrLn handle $ "\"" ++ s ++ "\"," ++ show expected ++ "," ++ show actual ++ "," ++ status
    hClose handle

-- | Основная функция
main :: IO ()
main = do
    putStrLn "=== CYK Parser Test Runner ==="
    -- Выбор грамматики
    selectedGrammar <- chooseGrammar grammars
    putStrLn $ "Выбрана грамматика: " ++ grammarName selectedGrammar

    -- Ввод параметров тестирования
    (maxDepthVal, numNegativesVal) <- getTestParameters
    putStrLn $ "Генерация положительных примеров до глубины: " ++ show maxDepthVal
    putStrLn $ "Количество отрицательных примеров: " ++ show numNegativesVal

    -- Генерация тестов
    putStrLn "Генерация тестовых случаев..."
    testCases <- generateTestCases (grammarDef selectedGrammar) maxDepthVal numNegativesVal

    -- Запуск тестов и сбор результатов
    putStrLn "Выполнение тестов:"
    results <- forM testCases $ \(s, expected) -> do
        let actual = cykParse (grammarDef selectedGrammar) s
        let status = if actual == expected then "PASS" else "FAIL"
        putStrLn $ status ++ ": \"" ++ s ++ "\" | Expected: " ++ show expected ++ ", Got: " ++ show actual
        return (s, expected, actual)

    -- Подсчёт итогов
    let total = length results
        passed = length $ filter (\(_, e, a) -> e == a) results
        failed = total - passed
    putStrLn $ "Итого: " ++ show passed ++ "/" ++ show total ++ " пройдено, " ++ show failed ++ " не пройдено."

    -- Запись результатов в файл
    let filename = "test_results.txt"
    writeResultsToFile filename results
    putStrLn $ "Результаты тестов записаны в файл: " ++ filename

    putStrLn "=== Тестирование завершено ==="
    exitSuccess
