module AutomatonFromEquivalenceTable (fromEquivalenceTable) where
{-|
This module provides functionality to convert an equivalence class table into an automaton representation.
-}
import qualified Data.Map as Map
import Automaton (Automaton(..))
import Data.List (nub)
import Data.Maybe (fromMaybe, maybeToList)


{-|
Converts an equivalence class table into an automaton.

The function takes a list of tuples, where each tuple contains a class name (String) and an integer value (0 or 1, where 1 indicates an accepting state).

The function performs the following steps:
1. Checks for duplicate class names
2. Ensures that the 'epsilon' class is present
3. Creates a mapping from class names to state IDs
4. Initializes states, alphabet, and transitions
5. Defines the initial state and accepting states
6. Constructs and returns the automaton

Note: This function extracts the alphabet from the class names provided in the table.

@param table An equivalence class table as a list of tuples (String, Int)
@return An automaton representation based on the equivalence class table
@throws Error if duplicate class names are found or if the 'epsilon' class is missing
-}
fromEquivalenceTable :: [(String, Int)] -> Automaton
fromEquivalenceTable table =
    let
        -- Check for duplicate class names
        classNames = map fst table
        uniqueClassNames = nub classNames
        _ = if length classNames /= length uniqueClassNames
            then error "Duplicate class names found"
            else ()

        -- Ensure 'epsilon' is present
        _ = if "e" `elem` classNames
            then ()
            else error "No epsilon class found in the table"

        -- Create a mapping from class names to state IDs
        classToState = Map.fromList $ zip classNames [0..]
        -- Add an error state
        errorStateId = Map.size classToState

        -- Initialize states
        statesList = [0..errorStateId]

        -- Extract alphabet from class names
        alphabetList = nub $ concat classNames

        -- The initial state is 'epsilon'
        initialStateId = classToState Map.! "e"

        -- Accepting states
        acceptingStatesList = [ stateId | (className, val) <- table
                                        , val == 1
                                        , stateId <- maybeToList (Map.lookup className classToState) ]

        -- Build transitions
        transitionsList =
            [ ((classToState Map.! s, c), classToState Map.! s')
                | s <- classNames
                , c <- alphabetList
                , let s' = s ++ [c]
                , Map.member s' classToState
            ] ++
            -- Transitions to errorState for missing s'
            [ ((classToState Map.! s, c), errorStateId)
                | s <- classNames
                , c <- alphabetList
                , let s' = s ++ [c]
                , not (Map.member s' classToState)
            ] ++
            -- Transitions for errorStateId (stay in error state)
            [ ((errorStateId, c), errorStateId)
                | c <- alphabetList
            ]
        transitions = Map.fromList transitionsList

    in Automaton
        { states = statesList
        , alphabet = alphabetList
        , transitions = transitions
        , initialState = initialStateId
        , acceptingStates = acceptingStatesList
        }