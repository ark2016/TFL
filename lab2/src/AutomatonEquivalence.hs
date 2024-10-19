module AutomatonEquivalence
    ( Automaton(..)
    , areAutomataEquivalent
    ) where

import Automaton (Automaton(..))
import AutomatonMinimizer (minimizeAutomaton)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (|>))

-- Проверка эквивалентности двух автоматов
areAutomataEquivalent :: Automaton -> Automaton -> Either String Bool
areAutomataEquivalent automaton1 automaton2 =
    let minimized1 = minimizeAutomaton automaton1
        minimized2 = minimizeAutomaton automaton2
    in if minimized1 == minimized2
       then Right True
       else Left (findCounterexample minimized1 minimized2)

-- Функция поиска контрпримера с использованием BFS
findCounterexample :: Automaton -> Automaton -> String
findCounterexample automaton1 automaton2 =
    case bfs (Seq.singleton "") of
        Just ce -> ce
        Nothing  -> "No counterexample found"
  where
    bfs :: Seq String -> Maybe String
    bfs Seq.Empty = Nothing
    bfs (current Seq.:<| rest) =
        if runAutomaton automaton1 current /= runAutomaton automaton2 current
            then Just current
            else bfs (rest Seq.>< nextStrings current)

    nextStrings :: String -> Seq String
    nextStrings prefix = Seq.fromList [prefix ++ [c] | c <- alphabet automaton1]

-- Запуск автомата на строке
runAutomaton :: Automaton -> String -> Bool
runAutomaton automaton input =
    let finalState = foldl (stepAutomaton automaton) (initialState automaton) input
    in finalState `elem` acceptingStates automaton

-- Шаг автомата по одному символу
stepAutomaton :: Automaton -> Int -> Char -> Int
stepAutomaton automaton state char =
    fromMaybe state (Map.lookup (state, char) (transitions automaton))




--let dfa1 = Automaton {states = [0, 1],alphabet = ['a', 'b'],transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],initialState = 0,acceptingStates = [1]}
--let dfa2 = Automaton {states = [0, 1],alphabet = ['a', 'b'],transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 0)],initialState = 0,acceptingStates = [1]}





