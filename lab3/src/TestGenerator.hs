{-# LANGUAGE OverloadedStrings #-}
module TestGenerator (
    generatePositiveExamples,
    generateNegativeExamples,
    generateTestCases
) where

import CYKParser
import Grammar
import FirstFollow (computeFirst, computeFollow)
import LastPrecede (computeLast, computePrecede)
import BigramMatrix (buildBigramMatrix)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Map.Strict (Map)
import Control.Monad (replicateM)
import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)

-- | Генерирует положительные примеры строк, принадлежащих языку, с помощью матрицы биграмм.
-- Параметры:
--   - grammar: Грамматика в CNF
--   - maxDepth: Максимальная длина генерируемых строк
generatePositiveExamples :: Grammar -> Int -> IO [String]
generatePositiveExamples grammar maxDepth = do
    let firstMap = computeFirst grammar
        followMap = computeFollow grammar
        lastMap = computeLast grammar
        precedeMap = computePrecede grammar

        bigrams = buildBigramMatrix grammar firstMap followMap lastMap precedeMap

    let startTerminals = terminalsFromFirstS firstMap
    let endTerminals = terminalsFromLastS lastMap

    -- Генерируем строки путём случайного блуждания по биграммам
    replicateM 5 (randomWalk bigrams startTerminals endTerminals maxDepth)

-- | Случайное блуждание по матрице биграмм, чтобы сгенерировать одну строку.
randomWalk :: Set (Symbol, Symbol) -> [Symbol] -> [Symbol] -> Int -> IO String
randomWalk bigrams starts ends maxDepth = do
    if null starts || null ends
      then return ""
      else do
        start <- randomChoice starts
        walk [start]
  where
    walk current
      | length current >= maxDepth = return (toStr current)
      | last current `elem` ends = return (toStr current)
      | otherwise = do
          let possibleNext = [ gamma2 | (gamma1, gamma2) <- Set.toList bigrams, gamma1 == last current ]
          if null possibleNext
            then return (toStr current)
            else do
              nxt <- randomChoice possibleNext
              walk (current ++ [nxt])

    toStr syms = [ c | Terminal c <- syms ]
    randomChoice xs = do
      idx <- randomRIO (0, length xs - 1)
      return (xs !! idx)

-- Извлекает терминалы из First(S)
terminalsFromFirstS :: Map Symbol (Set Symbol) -> [Symbol]
terminalsFromFirstS firstMap =
    case Map.lookup (NonTerminal "S") firstMap of
        Nothing -> []
        Just s -> [ sym | sym@(Terminal _) <- Set.toList s ]

-- Извлекает терминалы из Last(S)
terminalsFromLastS :: Map Symbol (Set Symbol) -> [Symbol]
terminalsFromLastS lastMap =
    case Map.lookup (NonTerminal "S") lastMap of
        Nothing -> []
        Just s -> [ sym | sym@(Terminal _) <- Set.toList s ]

-- | Генерирует отрицательные примеры строк, не принадлежащих языку.
-- Параметры:
--   - grammar: Грамматика в CNF
--   - positiveExamples: Список положительных примеров
--   - numExamples: Количество отрицательных примеров
generateNegativeExamples :: Grammar -> [String] -> Int -> IO [String]
generateNegativeExamples grammar positiveExamples numExamples = do
    let alphabet = getAlphabet grammar
    randomStrings <- generateRandomStrings alphabet maxLength (numExamples * 10)
    let negatives = filter (\s -> not (s `elem` positiveExamples) && not (cykParse grammar s)) randomStrings
    return $ take numExamples negatives
  where
    maxLength = if null positiveExamples then 5 else maximum (map length positiveExamples) + 2
    getAlphabet (Grammar rs) = Set.toList $ Set.fromList [ c | (_, rhs) <- rs, Terminal c <- rhs ]

-- Генерирует случайные строки из заданного алфавита.
generateRandomStrings :: [Char] -> Int -> Int -> IO [String]
generateRandomStrings alphabet maxLength num = do
    lengths <- replicateM num (randomRIO (1, maxLength))
    mapM (generateStringOfLength alphabet) lengths

generateStringOfLength :: [Char] -> Int -> IO String
generateStringOfLength alphabet len = replicateM len (randomChoice alphabet)
  where
    randomChoice xs = do
      idx <- randomRIO (0, length xs - 1)
      return (xs !! idx)

-- | Генерирует тестовые случаи в виде списка пар (String, Bool)
-- True: строка принадлежит языку
-- False: не принадлежит
generateTestCases :: Grammar -> Int -> Int -> IO [(String, Bool)]
generateTestCases grammar maxDepth numNegatives = do
    positiveExamples <- generatePositiveExamples grammar maxDepth
    negatives <- generateNegativeExamples grammar positiveExamples numNegatives
    let testCases = map (\s -> (s, True)) positiveExamples ++ map (\s -> (s, False)) negatives
    shuffleM testCases
