module AutomatonMinimizer
    ( Automaton(..)
    , minimizeAutomaton
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')

-- Automaton data structure
data Automaton = Automaton
    { states :: [Int]
    , alphabet :: [Char]
    , transitions :: Map.Map (Int, Char) Int
    , initialState :: Int
    , acceptingStates :: [Int]
    } deriving (Show, Eq)

-- Function to minimize a DFA using Hopcroft's algorithm
minimizeAutomaton :: Automaton -> Automaton
minimizeAutomaton automaton =
    let
        -- Initial partition of states: accepting and non-accepting
        initialPartition = [Set.fromList (acceptingStates automaton), Set.fromList (states automaton) Set.\\ Set.fromList (acceptingStates automaton)]

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
                            [ (Map.findWithDefault (-1) (s, symbol) (transitions automaton), Set.singleton s)
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
            [ ((stateToRepresentative Map.! s, a), stateToRepresentative Map.! Map.findWithDefault (-1) (s, a) (transitions automaton))
            | (s, a) <- Map.keys (transitions automaton) ]

        -- Determine new states and accepting states
        newStates = Set.toList $ Set.fromList $ Map.elems stateToRepresentative
        newAcceptingStates = Set.toList $ Set.fromList [stateToRepresentative Map.! s | s <- acceptingStates automaton]
    in
        Automaton
            { states = newStates
            , alphabet = alphabet automaton
            , transitions = minimizedTransitions
            , initialState = stateToRepresentative Map.! initialState automaton
            , acceptingStates = newAcceptingStates
            }

visualizeAutomaton :: Automaton -> IO ()
visualizeAutomaton (Automaton states alph transitions initial accepting) = do
    putStrLn "Automaton:"
    putStrLn $ "States: " ++ show states
    putStrLn $ "Alphabet: " ++ show alph
    putStrLn $ "Initial state: " ++ show initial
    putStrLn $ "Accepting states: " ++ show accepting
    putStrLn "Transitions:"
    putStrLn "digraph {"
    mapM_ (\((from, char), to) -> putStrLn $ "    " ++ show from ++  " -> " ++ show to ++ " [label = \"" ++ [char] ++ "\"]") (Map.toList transitions)
--    mapM_ (\((from, char), to) -> putStrLn $ show from ++ " --" ++ [char] ++ "--> " ++ show to) (Map.toList transitions)
    putStrLn "}"






--let dfa = Automaton {states = [0],alphabet = ['a', 'b'],transitions = Map.fromList [],initialState = 0,acceptingStates = [0]}
--let dfa = Automaton {states = [0, 1, 2, 3, 4],alphabet = ['a', 'b'],transitions = Map.fromList [ ((0, 'a'), 1), ((0, 'b'), 2),((1, 'a'), 1), ((1, 'b'), 3),((2, 'a'), 1), ((2, 'b'), 2),((3, 'a'), 1), ((3, 'b'), 4),((4, 'a'), 1), ((4, 'b'), 2) ],initialState = 0,acceptingStates = [4]}
--let dfa = Automaton {states = [0, 1, 2],alphabet = ['a', 'b'],transitions = Map.fromList [((0, 'a'), 1), ((1, 'b'), 0)],initialState = 0,acceptingStates = [1]}



















