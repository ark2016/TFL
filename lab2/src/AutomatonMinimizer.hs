module AutomatonMinimizer
    ( Automaton(..)
    , minimizeAutomaton
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub, (\\))
import Data.Maybe (fromMaybe)

-- Automaton data structure
data Automaton = Automaton
    { states :: [Int]
    , alphabet :: [Char]
    , transitions :: Map.Map (Int, Char) Int
    , initialState :: Int
    , acceptingStates :: [Int]
    } deriving (Show, Eq)

-- Main function to minimize the automaton
minimizeAutomaton :: Automaton -> Automaton
minimizeAutomaton aut =
    let eqClasses = computeEquivalenceClasses aut
        newStates = [0 .. (length eqClasses - 1)]
        stateMap = Map.fromList [(s, findClassIndex s eqClasses) | s <- states aut]
        newTransitions = Map.mapKeys (\(s, c) -> (stateMap Map.! s, c)) $
                         Map.map (\s -> stateMap Map.! s) (transitions aut)
        newInitialState = stateMap Map.! initialState aut
        newAcceptingStates = nub [stateMap Map.! s | s <- acceptingStates aut]
    in Automaton
        { states = newStates
        , alphabet = alphabet aut
        , transitions = newTransitions
        , initialState = newInitialState
        , acceptingStates = newAcceptingStates
        }

-- Compute equivalence classes for minimization
computeEquivalenceClasses :: Automaton -> [[Int]]
computeEquivalenceClasses aut =
    let initial = [acceptingStates aut, states aut \\ acceptingStates aut]
    in refinePartition aut initial

-- Refine partition until it stabilizes
refinePartition :: Automaton -> [[Int]] -> [[Int]]
refinePartition aut partition
    | partition == refined = partition
    | otherwise = refinePartition aut refined
  where
    refined = concatMap (refineClass aut partition) partition

-- Refine a single equivalence class
refineClass :: Automaton -> [[Int]] -> [Int] -> [[Int]]
refineClass aut partition cls =
    groupBy (\s1 s2 -> all (\a -> findClass (step aut s1 a) partition ==
                                  findClass (step aut s2 a) partition)
                           (alphabet aut))
            cls

-- Helper function to find the class of a state
findClass :: Int -> [[Int]] -> Maybe Int
findClass s = findIndex (elem s)

-- Helper function to find the index of a class containing a state
findClassIndex :: Int -> [[Int]] -> Int
findClassIndex s classes = fromMaybe (-1) (findIndex (elem s) classes)

-- Transition function
step :: Automaton -> Int -> Char -> Int
step aut s a = fromMaybe (-1) (Map.lookup (s, a) (transitions aut))

-- Helper function: group elements by a predicate
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x:xs) = let (ys, zs) = span (eq x) xs in (x:ys) : groupBy eq zs

-- Helper function: find index of first element satisfying a predicate
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p = loop 0
  where
    loop _ [] = Nothing
    loop i (x:xs)
        | p x = Just i
        | otherwise = loop (i+1) xs

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









