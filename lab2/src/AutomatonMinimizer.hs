module AutomatonMinimizer
    ( Automaton(..)
    , minimizeAutomaton
    ) where
-- Этот модуль предоставляет функциональность для минимизации детерминированных конечных автоматов (ДКА)
--с использованием алгоритма Хопкрофта.
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Automaton
import AutomatonVisualization

{-|
Находит все достижимые состояния из начального состояния автомата.

@
reachableStates :: Automaton -> Set.Set Int
@
-}
reachableStates :: Automaton -> Set.Set Int
reachableStates automaton = explore (Set.singleton $ initialState automaton) [initialState automaton]
  where
    explore visited [] = visited
    explore visited (current:queue) =
      let
        nextStates = [ Map.findWithDefault (-1) (current, a) (transitions automaton)
                     | a <- alphabet automaton ]
        newStates = filter (\s -> s /= -1 && not (Set.member s visited)) nextStates
      in
        explore (Set.union visited (Set.fromList newStates)) (queue ++ newStates)

{-|
Минимизирует ДКА с использованием алгоритма Хопкрофта.

@
minimizeAutomaton :: Automaton -> Automaton
@

Этапы минимизации:
1. Находит достижимые состояния.
2. Фильтрует переходы и принимающие состояния, оставляя только достижимые.
3. Создает начальное разбиение состояний на принимающие и непринимающие.
4. Итеративно уточняет разбиение, пока оно не стабилизируется.
5. Создает новый минимизированный автомат на основе финального разбиения.

-}
minimizeAutomaton :: Automaton -> Automaton
minimizeAutomaton automaton =
    let
        -- Find reachable states
        reachable = reachableStates automaton

        -- Filter transitions to only include reachable states
        filteredTransitions = Map.filterWithKey (\(s, _) _ -> s `Set.member` reachable) (transitions automaton)

        -- Filter accepting states to only include reachable states
        filteredAcceptingStates = filter (`Set.member` reachable) (acceptingStates automaton)

        -- Initial partition of states: accepting and non-accepting
        initialPartition = [Set.fromList filteredAcceptingStates, reachable Set.\\ Set.fromList filteredAcceptingStates]

        -- Function to refine partitions
        refine partitions = foldl' refineStep partitions (alphabet automaton)

        -- Refine step for a given symbol
        refineStep partitions symbol =
            let
                -- Split each block using the current symbol
                splitBlock block =
                    let
                        -- Group states by the state they transition to on the given symbol
                        transitionsMap = Map.fromListWith Set.union
                            [ (Map.findWithDefault (-1) (s, symbol) filteredTransitions, Set.singleton s)
                            | s <- Set.toList block ]
                    in
                        Map.elems transitionsMap
            in
                concatMap splitBlock partitions

        -- Repeatedly refine the partition until it stabilizes
        stablePartition = until (\p -> refine p == p) refine initialPartition

        -- Map each state to its representative in the final partition
        stateToRepresentative = Map.fromList
            [ (s, Set.findMin block)
            | block <- stablePartition, s <- Set.toList block ]

        -- Create new minimized transitions
        minimizedTransitions = Map.fromList
            [ ((stateToRepresentative Map.! s, a), stateToRepresentative Map.! Map.findWithDefault (-1) (s, a) filteredTransitions)
            | (s, a) <- Map.keys filteredTransitions ]

        -- Determine new states and accepting states
        newStates = Set.toList $ Set.fromList $ Map.elems stateToRepresentative
        newAcceptingStates = Set.toList $ Set.fromList [stateToRepresentative Map.! s | s <- filteredAcceptingStates]
    in
        Automaton
            { states = newStates
            , alphabet = alphabet automaton
            , transitions = minimizedTransitions
            , initialState = stateToRepresentative Map.! initialState automaton
            , acceptingStates = newAcceptingStates
            }


{-|
Примеры использования:

1. Минимизация простого ДКА:
@
let dfa = Automaton {states = [0, 1, 2], alphabet = ['a', 'b'], transitions = Map.fromList [((0, 'a'), 1), ((1, 'b'), 0)], initialState = 0, acceptingStates = [1]}
let minimizedDfa = minimizeAutomaton dfa
@

2. Минимизация более сложного ДКА:
@
let dfa = Automaton {states = [0, 1, 2, 3, 4], alphabet = ['a', 'b'], transitions = Map.fromList [ ((0, 'a'), 1), ((0, 'b'), 2), ((1, 'a'), 1), ((1, 'b'), 3), ((2, 'a'), 1), ((2, 'b'), 2), ((3, 'a'), 1), ((3, 'b'), 4), ((4, 'a'), 1), ((4, 'b'), 2) ], initialState = 0, acceptingStates = [4]}
let minimizedDfa = minimizeAutomaton dfa
@

3. Минимизация ДКА с недостижимыми состояниями:
@
let dfa = Automaton {states = [0, 1, 2, 3], alphabet = ['a', 'b'], transitions = Map.fromList [ ((0, 'a'), 1), ((0, 'b'), 2), ((1, 'a'), 3), ((1, 'b'), 3), ((2, 'a'), 3), ((2, 'b'), 3), ((3, 'a'), 3), ((3, 'b'), 3) ], initialState = 0, acceptingStates = [3]}
let minimizedDfa = minimizeAutomaton dfa
@

-}


















