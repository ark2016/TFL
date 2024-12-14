module Main (
    main
) where

import Grammar (Grammar, parseGrammar)
import ChomskyNormalForm (toChomskyNormalForm)
import TestGenerator (generateTestCases)
import System.IO (hFlush, stdout)

-- | Функция `main` запускает программу, принимая входную грамматику, максимальную длину строки и количество отрицательных тестов.
main :: IO ()
main = do
    -- Получаем грамматику из стандартного ввода
    putStrLn "Введите грамматику (каждое правило на новой строке, окончание - пустая строка):"
    grammarInput <- getGrammarInput
    case parseGrammar grammarInput of
        Nothing -> putStrLn "Ошибка: неверный формат грамматики."
        Just grammar -> do
            -- Запрос параметров генерации
            putStrLn "Введите максимальную длину строки:"
            maxDepth <- readLn
            putStrLn "Введите количество отрицательных тестов:"
            numNegatives <- readLn

            -- Преобразуем грамматику в нормальную форму Хомского
            let cnfGrammar = toChomskyNormalForm grammar

            -- Генерируем тестовые случаи
            testCases <- generateTestCases cnfGrammar maxDepth numNegatives

            -- Выводим тесты в консоль
            mapM_ printTestCase testCases
  where
    -- Чтение грамматики из ввода
    getGrammarInput :: IO String
    getGrammarInput = do
        line <- getLine
        if null line
            then return ""
            else do
                rest <- getGrammarInput
                return $ line ++ "\n" ++ rest

    -- Печать одного тестового случая
    printTestCase :: (String, Bool) -> IO ()
    printTestCase (str, label) = do
        putStrLn $ str ++ " " ++ show (if label then 1 else 0)
        hFlush stdout
