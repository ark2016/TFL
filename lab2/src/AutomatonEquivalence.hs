module AutomatonEquivalence
    ( Automaton(..)
    , areAutomataEquivalent
    ) where
{-|
Этот модуль предоставляет функциональность для проверки эквивалентности двух детерминированных
конечных автоматов (ДКА) и поиска контрпримера в случае их неэквивалентности.
-}
import Automaton (Automaton(..))
import AutomatonMinimizer (minimizeAutomaton)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (|>))
import qualified Data.Set as Set
import Data.Set (Set)

{-|
Проверяет эквивалентность двух автоматов.

Возвращает:
- @Right True@, если автоматы эквивалентны
- @Left error@, если автоматы не эквивалентны или имеют разные алфавиты,
  где @error@ - строка с описанием ошибки или контрпримером

Алгоритм:
1. Проверяет различия в алфавитах автоматов и обрабатывает два случая:
   - Если алфавит первого автомата содержит символы, отсутствующие во втором, возвращает контрпример.
   - Если алфавит второго автомата является супермножеством первого, возвращает сообщение об ошибке.
2. Если алфавиты идентичны, минимизирует оба автомата.
3. Сравнивает минимизированные автоматы.
4. Если автоматы не эквивалентны, ищет контрпример.
-}
areAutomataEquivalent :: Automaton -> Automaton -> Either String Bool
areAutomataEquivalent automaton1 automaton2 =
    let
        alphabet1 = Set.fromList (alphabet automaton1)
        alphabet2 = Set.fromList (alphabet automaton2)

        missingIn2 = Set.difference alphabet1 alphabet2
        extraIn2 = Set.difference alphabet2 alphabet1
    in
        if not (Set.null missingIn2)
        then
            -- Case 1: automaton1 has symbols not in automaton2
            case findCounterexampleWithMissingSymbol automaton1 missingIn2 of
                Just ce -> Left ce
                Nothing -> Right True  -- If no such string exists, automata are equivalent
        else if not (Set.null extraIn2)
        then
            -- Case 2: automaton2 has extra symbols
            Left "Error: Automaton2's alphabet is a superset of Automaton1's alphabet."
        else
            -- Alphabets are identical; proceed with minimization and equivalence checking
            let
                minimized1 = minimizeAutomaton automaton1
                minimized2 = minimizeAutomaton automaton2
            in
                if minimized1 == minimized2
                then Right True
                else Left (findCounterexample minimized1 minimized2)

{-|
Ищет контрпример для двух неэквивалентных автоматов, когда автомат1 содержит символы,
отсутствующие в автомат2. Контрпример должен быть строкой, принимаемой автоматом1 и содержащей
хотя бы один из отсутствующих символов.
-}
findCounterexampleWithMissingSymbol :: Automaton -> Set Char -> Maybe String
findCounterexampleWithMissingSymbol automaton missingSymbols =
    bfs (Seq.singleton "") Set.empty
  where
    -- BFS to find the shortest string accepted by automaton1 that includes at least one missing symbol
    bfs :: Seq String -> Set String -> Maybe String
    bfs Seq.Empty _ = Nothing
    bfs (current Seq.:<| rest) visited =
        if current `Set.member` visited
            then bfs rest visited
            else
                let
                    hasMissingSymbol = any (`Set.member` missingSymbols) current
                    isAccepted = runAutomaton automaton current
                in
                    if hasMissingSymbol && isAccepted
                        then Just current
                        else
                            let
                                newVisited = Set.insert current visited
                                newStrings = nextStrings current
                            in
                                bfs (rest Seq.>< newStrings) newVisited

    -- Generate next strings by appending each symbol from the alphabet
    nextStrings :: String -> Seq String
    nextStrings prefix = Seq.fromList [prefix ++ [c] | c <- alphabet automaton]

{-|
Ищет контрпример для двух неэквивалентных автоматов, когда автомат1 и автомат2 имеют одинаковые алфавиты.

Использует поиск в ширину (BFS) для нахождения кратчайшей строки,
на которой автоматы дают разные результаты.
-}
findCounterexample :: Automaton -> Automaton -> String
findCounterexample automaton1 automaton2 =
    case bfs (Seq.singleton "") of
        Just ce -> ce
        Nothing  -> "No counterexample found"
  where
    -- BFS to find the shortest string where the automata differ
    bfs :: Seq String -> Maybe String
    bfs Seq.Empty = Nothing
    bfs (current Seq.:<| rest) =
        if runAutomaton automaton1 current /= runAutomaton automaton2 current
            then Just current
            else bfs (rest Seq.>< nextStrings current)

    -- Generate next strings by appending each symbol from the alphabet
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




