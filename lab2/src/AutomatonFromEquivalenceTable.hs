module AutomatonFromEquivalenceTable (fromEquivalenceTable) where

import qualified Data.Map as Map
import Automaton (Automaton(..))
import Data.List (nub)

-- | Converts an equivalence class table into an Automaton
fromEquivalenceTable :: [(String, Int)] -> Automaton
fromEquivalenceTable table =
    let
        -- Check for duplicate class names
        classNames = map fst table
        uniqueClassNames = nub classNames
        _ = if length classNames /= length uniqueClassNames
            then error "Duplicate class names found"
            else ()

        -- Ordered table ensures 'epsilon' is at the first if present
        orderedTable = case lookup "epsilon" table of
            Just _  -> ("epsilon", fromMaybe 0 (lookup "epsilon" table)) :
                       filter ((/= "epsilon") . fst) table
            Nothing -> error "No epsilon class found in the table"

        -- Create a mapping from class names to state IDs
        classToState = Map.fromList $ zip (map fst orderedTable) [0..]

        -- Initialize states
        statesList = [0..length orderedTable - 1]

        -- Assuming a basic alphabet for simplicity
        alphabetList = ['a']

        -- The initial state is always 0 as we ensured 'epsilon' is first
        initialStateId = 0

        -- Accepting states are those with value 1
        acceptingStatesList = [stateId | (className, val) <- orderedTable, val == 1, Just stateId <- [Map.lookup className classToState]]

        -- Transitions: Create a cyclic transition pattern if more than one state
        transitions = if length orderedTable > 1
                      then Map.fromList [((stateId, 'a'), (stateId + 1) `mod` length orderedTable) | stateId <- statesList]
                      else Map.empty

    in Automaton
        { states = statesList
        , alphabet = alphabetList
        , transitions = transitions
        , initialState = initialStateId
        , acceptingStates = acceptingStatesList
        }

-- Helper function to use `fromMaybe`
fromMaybe :: a -> Maybe a -> a
fromMaybe defval wrapped =
    case wrapped of
        Nothing -> defval
        Just value -> value