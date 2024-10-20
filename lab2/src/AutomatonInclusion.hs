module AutomatonInclusion where

import qualified Data.Map as Map
import Automaton (Automaton(..))

-- Simulate the automaton on the input string and return 1 if the string is accepted, 0 otherwise
isStringAccepted :: Automaton -> String -> Int
isStringAccepted automaton input =
    let finalState = foldl (transition automaton) (initialState automaton) input
    in if finalState `elem` (acceptingStates automaton)
       then 1
       else 0

-- Helper function to transition between states based on the current state and input character
transition :: Automaton -> Int -> Char -> Int
transition automaton currentState char =
    case Map.lookup (currentState, char) (transitions automaton) of
        Just nextState -> nextState
        Nothing -> currentState  -- If no valid transition, stay in the current state (or handle as needed)