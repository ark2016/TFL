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

-- Полиморфная функция для выбора случайного элемента из списка
randomChoice :: [a] -> IO a
randomChoice [] = fail "randomChoice: empty list"
randomChoice xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs !! idx)

-- Генерация строк, принадлежащих языку (positive examples)

-- | Случайное блуждание по матрице биграмм, чтобы сгенерировать одну строку.
randomWalk
    :: Set (Symbol, Symbol) -- ^ множество допустимых биграмм (Symbol, Symbol)
    -> [Symbol]             -- ^ возможные стартовые терминалы
    -> [Symbol]             -- ^ возможные конечные терминалы
    -> Int                  -- ^ максимальная длина строки
    -> IO String            -- ^ возвращаем сгенерированную строку
randomWalk bigrams starts ends maxDepth =
    if null starts || null ends
      then return ""
      else do
        start <- randomChoice starts
        walk [start]
  where
    walk current
      | length current >= maxDepth           = return (toStr current)
      | last current `elem` ends            = return (toStr current)
      | otherwise = do
          let possibleNext = [ gamma2
                             | (gamma1, gamma2) <- Set.toList bigrams
                             , gamma1 == last current
                             ]
          if null possibleNext
            then return (toStr current)
            else do
              nxt <- randomChoice possibleNext
              walk (current ++ [nxt])

    toStr syms = [ c | Terminal c <- syms ]

-- | Генерирует заданное количество уникальных положительных примеров.
--   Если при случайном блуждании возникают дубли, функция продолжает генерировать
--   новые строки, пока не получит нужное количество уникальных результатов.
generateUniquePositiveExamples
    :: Grammar   -- ^ грамматика в CNF
    -> Int       -- ^ максимальная длина строки
    -> Int       -- ^ сколько уникальных строк нужно
    -> IO [String]
generateUniquePositiveExamples grammar maxDepth numPos = do
    let firstMap   = computeFirst grammar
        followMap  = computeFollow grammar
        lastMap    = computeLast grammar
        precedeMap = computePrecede grammar

        bigrams = buildBigramMatrix grammar firstMap followMap lastMap precedeMap
        startTerminals = terminalsFromFirstS firstMap
        endTerminals   = terminalsFromLastS lastMap

    collectUnique bigrams startTerminals endTerminals Set.empty
  where
    collectUnique
      :: Set (Symbol, Symbol)  -- bigrams
      -> [Symbol]              -- startTerminals
      -> [Symbol]              -- endTerminals
      -> Set String            -- уже собранные строки
      -> IO [String]
    collectUnique _ _ _ acc
      | Set.size acc >= numPos = return (Set.toList acc)
    collectUnique b s e acc = do
      str <- randomWalk b s e maxDepth
      if Set.member str acc
         then collectUnique b s e acc
         else collectUnique b s e (Set.insert str acc)


-- | Если вам нужно оставить функцию с таким же названием и сигнатурой,
--   можете просто вызывать внутри неё новую функцию generateUniquePositiveExamples.
generatePositiveExamples :: Grammar -> Int -> IO [String]
generatePositiveExamples grammar maxDepth =
    -- Для примера порождаем 5 уникальных строк, как в исходном коде
    generateUniquePositiveExamples grammar maxDepth 1


-- Генерация строк, не принадлежащих языку (negative examples)

-- | Генерирует отрицательные примеры строк, не принадлежащих языку.
-- Параметры:
--   - grammar: Грамматика в CNF
--   - positiveExamples: Список положительных примеров (исключаем их из кандидатов)
--   - numExamples: Количество отрицательных примеров
generateNegativeExamples :: Grammar -> [String] -> Int -> IO [String]
generateNegativeExamples grammar positiveExamples numExamples = do
    let alphabet = getAlphabet grammar
    randomStrings <- generateRandomStrings alphabet maxLength (numExamples * 10)
    let negatives = filter (\s -> s `notElem` positiveExamples
                                && not (cykParse grammar s)
                         )
                     randomStrings
    return $ take numExamples negatives
  where
    -- Длина отрицательных строк не будет сильно отличаться от положительных
    maxLength =
      if null positiveExamples
        then 5
        else maximum (map length positiveExamples) + 2

    getAlphabet (Grammar rs) =
      Set.toList . Set.fromList $ [ c | (_, rhs) <- rs, Terminal c <- rhs ]

-- Генерирует случайные строки из заданного алфавита.
generateRandomStrings :: [Char] -> Int -> Int -> IO [String]
generateRandomStrings alphabet maxLength num = do
    lengths <- replicateM num (randomRIO (1, maxLength))
    mapM (generateStringOfLength alphabet) lengths

generateStringOfLength :: [Char] -> Int -> IO String
generateStringOfLength alphabet len =
    replicateM len (randomChoice alphabet)

-- Финальная функция для генерации тестовых случаев

-- | Генерирует тестовые случаи в виде списка пар (String, Bool)
-- True: строка принадлежит языку
-- False: не принадлежит
generateTestCases :: Grammar -> Int -> Int -> IO [(String, Bool)]
generateTestCases grammar maxDepth numNegatives = do
    -- Генерируем ровно numNegatives уникальных положительных примеров
    positiveExamples <- generateUniquePositiveExamples grammar maxDepth numNegatives

    -- Генерируем ровно numNegatives отрицательных примеров
    negatives <- generateNegativeExamples grammar positiveExamples numNegatives

    let testCases =
          map (\s -> (s, True))  positiveExamples
          ++
          map (\s -> (s, False)) negatives

    shuffleM testCases
