{-|
Этот модуль предоставляет функциональность для проверки эквивалентности двух детерминированных
конечных автоматов (ДКА) и поиска контрпримера в случае их неэквивалентности.
-}
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

{-|
Проверяет эквивалентность двух автоматов.

@
areAutomataEquivalent :: Automaton -> Automaton -> Either String Bool
@

Возвращает:
- @Right True@, если автоматы эквивалентны
- @Left error@, если автоматы не эквивалентны или имеют разные алфавиты,
  где @error@ - строка с описанием ошибки или контрпримером

Алгоритм:
1. Проверяет, одинаковы ли алфавиты автоматов
2. Минимизирует оба автомата
3. Сравнивает минимизированные автоматы
4. Если автоматы не эквивалентны, ищет контрпример
-}
areAutomataEquivalent :: Automaton -> Automaton -> Either String Bool
areAutomataEquivalent automaton1 automaton2
    | alphabet automaton1 /= alphabet automaton2 = Left "Alphabets are different"
    | otherwise =
        let minimized1 = minimizeAutomaton automaton1
            minimized2 = minimizeAutomaton automaton2
        in if minimized1 == minimized2
           then Right True
           else Left (findCounterexample minimized1 minimized2)

{-|
Ищет контрпример для двух неэквивалентных автоматов.

@
findCounterexample :: Automaton -> Automaton -> String
@

Использует поиск в ширину (BFS) для нахождения кратчайшей строки,
на которой автоматы дают разные результаты.
-}
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

{-|
Запускает автомат на заданной входной строке.

@
runAutomaton :: Automaton -> String -> Bool
@

Возвращает @True@, если автомат принимает строку, и @False@ в противном случае.
-}
runAutomaton :: Automaton -> String -> Bool
runAutomaton automaton input =
    let finalState = foldl (stepAutomaton automaton) (initialState automaton) input
    in finalState `elem` acceptingStates automaton

{-|
Выполняет один шаг работы автомата.

@
stepAutomaton :: Automaton -> Int -> Char -> Int
@

Принимает текущее состояние и символ, возвращает следующее состояние автомата.
-}
stepAutomaton :: Automaton -> Int -> Char -> Int
stepAutomaton automaton state char =
    fromMaybe state (Map.lookup (state, char) (transitions automaton))




{-|
Примеры использования:

1. Проверка эквивалентности двух одинаковых автоматов:
@
let dfa = Automaton {states = [0, 1], alphabet = ['a', 'b'],
                     transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
                     initialState = 0, acceptingStates = [1]}
areAutomataEquivalent dfa dfa  -- Должно вернуть Right True
@

2. Проверка эквивалентности двух разных автоматов:
@
let dfa1 = Automaton {states = [0, 1], alphabet = ['a', 'b'],
                      transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
                      initialState = 0, acceptingStates = [1]}
let dfa2 = Automaton {states = [0, 1], alphabet = ['a', 'b'],
                      transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 0)],
                      initialState = 0, acceptingStates = [1]}
areAutomataEquivalent dfa1 dfa2  -- Должно вернуть Left с контрпримером
@

-}




