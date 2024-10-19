module AutomatonVisualization (visualizeAutomaton) where

import Automaton
import qualified Data.Map as Map
import qualified Data.Set as Set

visualizeAutomaton :: Automaton -> IO ()
visualizeAutomaton (Automaton states alph transitions initial accepting) = do
   putStrLn "Automaton:"
   putStrLn $ "States: " ++ show states
   putStrLn $ "Alphabet: " ++ show alph
   putStrLn $ "Initial state: " ++ show initial
   putStrLn $ "Accepting states: " ++ show accepting
   putStrLn "Transitions:"
   putStrLn "digraph {"
   putStrLn $ "  \"start\" [shape=point];"
   putStrLn $ "  \"start\" -> " ++ show initial ++ ";"
   mapM_ (\x -> putStrLn $ "  " ++ show x ++ " [shape=doublecircle];") accepting
   mapM_ (\((from, char), to) -> putStrLn $ "  " ++ show from ++ " -> " ++ show to ++ " [label = \"" ++ [char] ++ "\"]")
         (Map.toList transitions)
   putStrLn "}"