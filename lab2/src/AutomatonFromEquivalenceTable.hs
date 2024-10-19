module AutomatonFromEquivalenceTable (fromEquivalenceTable) where

import qualified Data.Map as Map
import Automaton (Automaton(..))

-- Assuming the table is passed as [(String, Int)] where
-- String is the equivalence class and Int is 0 or 1 (accepting state or not)
-- The epsilon class is the initial state and is denoted by "epsilon".
-- The transition between equivalence classes is not explicitly defined but is
-- inferred from the structure of the table.
--
-- Example input:
-- [("epsilon", 0), ("class_1", 1), ("class_2", 0), ...]

fromEquivalenceTable :: [(String, Int)] -> Automaton
fromEquivalenceTable table =
    let
        -- Create a mapping from class names to state IDs
        classToState = Map.fromList $ zip (map fst table) [0..]

        -- Initialize states
        statesList = [0..length table - 1]

        -- Assuming a basic alphabet for simplicity
        alphabetList = ['a']

        -- The initial state corresponds to the "epsilon" class
        initialStateId = case Map.lookup "epsilon" classToState of
            Just state -> state
            Nothing    -> error "No epsilon class found in the table"

        -- Accepting states are those with value 1
        acceptingStatesList = [state | (cls, val) <- table, val == 1, Just state <- [Map.lookup cls classToState]]

        -- Transitions: Create a cyclic transition pattern for demonstration purposes
        createTransitionIndex idx = (idx, alphabetList !! 0)
        cyclicTransitions = Map.fromList [((idx, 'a'), (idx + 1) `mod` length statesList) | idx <- statesList]

    in Automaton
        { states = statesList
        , alphabet = alphabetList
        , transitions = cyclicTransitions
        , initialState = initialStateId
        , acceptingStates = acceptingStatesList
        }
--let eqTable = [("epsilon", 0), ("class_1", 1), ("class_2", 0)]
--let automaton = fromEquivalenceTable(eqTable)
--print automaton