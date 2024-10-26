module AutomatonFromEquivalenceTable (fromEquivalenceTable) where

import qualified Data.Map as Map
import Automaton (Automaton(..))
import Data.List (nub)
import Data.Maybe (maybeToList)
import AutomatonVisualization (visualizeAutomaton)

{-|
Converts an equivalence class table into an automaton.

The function takes a list of tuples, where each tuple contains:
1. A class name (String)
2. A list of transitions (List of tuples (Char, String)) representing transitions for each class
3. An integer value (0 or 1, where 1 indicates an accepting state)

The function performs the following steps:
1. Checks for duplicate class names
2. Ensures that the 'epsilon' class is present
3. Creates a mapping from class names to state IDs
4. Initializes states, alphabet, and transitions
5. Defines the initial state and accepting states
6. Constructs and returns the automaton

@param table An equivalence class table as a list of tuples (String, [(Char, String)], Int)
@return An automaton representation based on the equivalence class table
@throws Error if duplicate class names are found or if the 'epsilon' class is missing
-}
fromEquivalenceTable :: [(String, [(Char, String)], Int)] -> Automaton
fromEquivalenceTable table =
    let
        -- Extract class names, transitions, and values (accepting or not)
        classNames = map (\(name, _, _) -> name) table

        -- Check for duplicate class names
        uniqueClassNames = nub classNames
        _ = if length classNames /= length uniqueClassNames
            then error "Duplicate class names found"
            else ()

        -- Ensure 'epsilon' (or "e") is present
        _ = if "e" `elem` classNames
            then ()
            else error "No epsilon class found in the table"

        -- Create a mapping from class names to state IDs
        classToState = Map.fromList $ zip classNames [0..]

        -- Initialize states
        statesList = [0..(Map.size classToState - 1)]

        -- Extract and deduplicate the alphabet from all transitions
        alphabetList = nub [ c | (_, transitions, _) <- table, (c, _) <- transitions ]

        -- The initial state is 'epsilon' (represented by "e")
        initialStateId = classToState Map.! "e"

        -- Accepting states (states that have value 1 in the table)
        acceptingStatesList = [ stateId | (className, _, val) <- table
                                        , val == 1
                                        , stateId <- maybeToList (Map.lookup className classToState) ]

        -- Build transitions from the table
        transitionsList =
            [ ((classToState Map.! s, c), classToState Map.! s')
                | (s, transitions, _) <- table
                , (c, s') <- transitions
                , Map.member s' classToState  -- Only create a transition if the target state exists
            ]

        -- Construct the transition map
        transitions = Map.fromList transitionsList

    in Automaton
        { states = statesList
        , alphabet = alphabetList
        , transitions = transitions
        , initialState = initialStateId
        , acceptingStates = acceptingStatesList
        }
--table1 = [    ("e",    [('a', "a")],          0),   ("a",    [('a', "a"), ('b', "a")], 1)   ]
--[    ("e",    [('a', "saw_a")],     0), ("saw_a", [('b', "found_ab")], 0), ("found_ab", [('a', "found_ab"),                  ('b', "found_ab")], 1) ]