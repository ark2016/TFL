module AutomatonFromEquivalenceTable (fromEquivalenceTable) where

import qualified Data.Map as Map
import Automaton (Automaton(..))
import Data.List (nub)
import Data.Maybe (maybeToList)
import AutomatonVisualization (visualizeAutomaton)

-- The equivalence table is in the form: [(String, String, Int)]
fromEquivalenceTable :: [(String, String, Int)] -> Automaton
fromEquivalenceTable table =
    let
        -- Extract the class names (first element in each row)
        classNames = map (\(name, _, _) -> name) table

        -- Ensure class names are unique
        uniqueClassNames = nub classNames
        _ = if length classNames /= length uniqueClassNames
            then error "Duplicate class names found"
            else ()

        -- Ensure the epsilon class (represented by "") is present
        _ = if "" `elem` classNames
            then ()
            else error "No epsilon class found in the table"

        -- Create a mapping from class names to state IDs
        classToState = Map.fromList $ zip classNames [0..]

        -- Initialize states: list of state IDs
        statesList = [0..(Map.size classToState - 1)]

        -- Extract and deduplicate the alphabet from all transitions
        alphabetList = nub [ head c | (_, c, _) <- table, not (null c) ] -- First character of each non-empty string

        -- The initial state is the one corresponding to the empty string (epsilon)
        initialStateId = classToState Map.! ""

        -- Accepting states: states with value 1 in the third column of the table
        acceptingStatesList = [ stateId | (className, _, val) <- table
                                        , val == 1
                                        , stateId <- maybeToList (Map.lookup className classToState) ]

        -- Build transitions from the table
        transitionsList =
            [ ((classToState Map.! s, head t), classToState Map.! s')
                | (s, t, _) <- table  -- (s: current state, t: transition string, _: accepting flag)
                , not (null t)        -- Only consider non-epsilon transitions
                , let s' = findNextState s t
                , Map.member s' classToState -- Ensure the target state exists
            ]

        -- Function to find the next state based on the current state and transition string
        findNextState :: String -> String -> String
        findNextState s t = -- u concatenated with γ should map to the correct state
            let nextPrefix = s ++ [head t]  -- Simulating a DFA transition
            in head [u' | (u', _, _) <- table, u' == nextPrefix]

        -- Construct the transition map
        transitions = Map.fromList transitionsList

    in Automaton
        { states = statesList
        , alphabet = alphabetList
        , transitions = transitions
        , initialState = initialStateId
        , acceptingStates = acceptingStatesList
        }

--[("", "a", 0), ("a", "", 1)]
--[("", "a", 0), ("a", "b", 1), ("ab", "", 0)]