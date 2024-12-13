{-# LANGUAGE OverloadedStrings #-}

module TestGenerator (
    generatePositiveExamples,
    generateNegativeExamples,
    generateTestCases
) where

import CYKParser
import Grammar
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Map.Strict (Map)
import Control.Monad (guard, replicateM)
import Control.Applicative ((<|>))
import System.Random
import System.Random.Shuffle (shuffleM)

-- | Генерирует положительные примеры строк, принадлежащих языку грамматики.
-- Параметры:
--   - grammar: Грамматика в CNF
--   - maxDepth: Максимальная глубина рекурсии для генерации строк
generatePositiveExamples :: Grammar -> Int -> [String]
generatePositiveExamples grammar maxDepth =
    concatMap (\d -> generateFromSymbol startSymbol d) [0..maxDepth]
  where
    startSymbol = getStartSymbol grammar

    getStartSymbol :: Grammar -> Symbol
    getStartSymbol (Grammar rs) = NonTerminal "S" -- Предполагается, что стартовый символ — "S"

    -- Генерация строк из заданного нетерминала с ограничением глубины
    generateFromSymbol :: Symbol -> Int -> [String]
    generateFromSymbol (Terminal c) _ = [[c]]
    generateFromSymbol Epsilon _ = [""] -- Пустая строка
    generateFromSymbol (NonTerminal nt) depth
        | depth <= 0 = []
        | otherwise =
            concatMap (generateFromRule depth) (getRules nt)

    -- Получение всех правил для заданного нетерминала
    getRules :: String -> [[Symbol]]
    getRules nt =
        [ rhs | (lhs, rhs) <- getRulesList grammar, lhs == NonTerminal nt ]

    getRulesList :: Grammar -> [(Symbol, [Symbol])]
    getRulesList (Grammar rs) = rs

    -- Генерация строк из одного правила
    generateFromRule :: Int -> [Symbol] -> [String]
    generateFromRule depth rhs = allCombinations rhs
      where
        allCombinations :: [Symbol] -> [String]
        allCombinations [] = [""]
        allCombinations (s:ss) =
            [ a ++ b | a <- generateFromSymbol s (depth - 1), b <- allCombinations ss ]

-- | Генерирует отрицательные примеры строк, не принадлежащих языку грамматики.
-- Параметры:
--   - grammar: Грамматика в CNF
--   - positiveExamples: Список положительных примеров для избежания
--   - numExamples: Количество отрицательных примеров
generateNegativeExamples :: Grammar -> [String] -> Int -> IO [String]
generateNegativeExamples grammar positiveExamples numExamples = do
    let alphabet = getAlphabet grammar
    randomStrings <- generateRandomStrings alphabet maxLength (numExamples * 5)
    let negatives = filter (\s -> not (s `elem` positiveExamples) && not (cykParse grammar s)) randomStrings
    return $ take numExamples negatives
  where
    maxLength = if null positiveExamples then 5 else maximum (map length positiveExamples) + 2
    getAlphabet :: Grammar -> [Char]
    getAlphabet (Grammar rs) = Set.toList $ Set.fromList [ c | (_, rhs) <- rs, Terminal c <- rhs ]

-- | Генерирует случайные строки из заданного алфавита и длины
generateRandomStrings :: [Char] -> Int -> Int -> IO [String]
generateRandomStrings alphabet maxLength num = do
    lengths <- replicateM num (randomRIO (1, maxLength))
    mapM (generateStringOfLength alphabet) lengths

-- | Генерирует строку заданной длины
generateStringOfLength :: [Char] -> Int -> IO String
generateStringOfLength alphabet len = replicateM len (randomChoice alphabet)
  where
    randomChoice :: [a] -> IO a
    randomChoice xs = do
        idx <- randomRIO (0, length xs - 1)
        return (xs !! idx)

-- | Генерирует тестовые случаи в виде списка пар (String, Bool)
--   - True: строка принадлежит языку
--   - False: строка не принадлежит языку
generateTestCases :: Grammar -> Int -> Int -> IO [(String, Bool)]
generateTestCases grammar maxDepth numNegatives = do
    let positiveExamples = generatePositiveExamples grammar maxDepth
    negatives <- generateNegativeExamples grammar positiveExamples numNegatives
    let testCases = map (\s -> (s, True)) positiveExamples ++ map (\s -> (s, False)) negatives
    shuffled <- shuffleM testCases -- Перемешивание для случайного порядка
    return shuffled
