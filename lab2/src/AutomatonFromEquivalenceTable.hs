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
        statesList = Map.elems classToState

        -- The alphabet is simple, for this example we'll assume it's just ['a'] for simplicity
        alphabetList = ['a']

        -- The initial state corresponds to the "epsilon" class
        initialStateId = case Map.lookup "epsilon" classToState of
            Just state -> state
            Nothing    -> error "No epsilon class found in the table"

        -- Accepting states are those with value 1
        acceptingStatesList = [state | (cls, val) <- table, val == 1, Just state <- [Map.lookup cls classToState]]

        -- Transitions: This part is somewhat arbitrary without more information,
        -- but we'll assume each class transitions to the next class in the list
        -- under the alphabet 'a'.
        transitionsMap = Map.fromList $ concatMap createTransitions (zip (map fst table) [0..])

        createTransitions (cls, stateId) =
            case Map.lookup cls classToState of
                Just nextStateId -> [((stateId, 'a'), nextStateId)]
                Nothing -> []

    in Automaton
        { states = statesList
        , alphabet = alphabetList
        , transitions = transitionsMap
        , initialState = initialStateId
        , acceptingStates = acceptingStatesList
        }
--let eqTable = [("epsilon", 0), ("class_1", 1), ("class_2", 0)]
--let automaton = fromEquivalenceTable(eqTable)
--print automaton