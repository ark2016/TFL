module AutomatonEquivalence
    ( Automaton(..)
    , areAutomataEquivalent
    ) where

import Automaton (Automaton(..))
import AutomatonMinimizer (minimizeAutomaton)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Проверка эквивалентности двух автоматов
areAutomataEquivalent :: Automaton -> Automaton -> Either String Bool
areAutomataEquivalent automaton1 automaton2 =
    let minimized1 = minimizeAutomaton automaton1
        minimized2 = minimizeAutomaton automaton2
    in if minimized1 == minimized2
       then Right True
       else Left (findCounterexample minimized1 minimized2)

-- Функция поиска контрпримера
findCounterexample :: Automaton -> Automaton -> String
findCounterexample automaton1 automaton2 =
    fromMaybe "No counterexample found" (findDifference automaton1 automaton2 "")

-- Рекурсивная функция для поиска различий в поведении автоматов
findDifference :: Automaton -> Automaton -> String -> Maybe String
findDifference automaton1 automaton2 prefix =
    let nextChars = alphabet automaton1
    in if runAutomaton automaton1 prefix /= runAutomaton automaton2 prefix
       then Just prefix
       else findInNextStates automaton1 automaton2 prefix nextChars

-- Проход по всем символам алфавита, чтобы найти различие
findInNextStates :: Automaton -> Automaton -> String -> [Char] -> Maybe String
findInNextStates _ _ _ [] = Nothing
findInNextStates automaton1 automaton2 prefix (c:cs) =
    let newPrefix = prefix ++ [c]
    in case findDifference automaton1 automaton2 newPrefix of
        Just counterexample -> Just counterexample
        Nothing -> findInNextStates automaton1 automaton2 prefix cs

-- Запуск автомата на строке
runAutomaton :: Automaton -> String -> Bool
runAutomaton automaton input =
    let finalState = foldl (stepAutomaton automaton) (initialState automaton) input
    in finalState `elem` acceptingStates automaton

-- Шаг автомата по одному символу
stepAutomaton :: Automaton -> Int -> Char -> Int
stepAutomaton automaton state char =
    fromMaybe state (Map.lookup (state, char) (transitions automaton))