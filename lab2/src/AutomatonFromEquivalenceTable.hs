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
        -- Define the epsilon symbol as 'E'
        epsilonSymbol = 'E'

        -- Extract the class names (first element in each row)
        classNames = map (\(name, _, _) -> name) table

        -- Ensure class names are unique
        uniqueClassNames = nub classNames
        _ = if length classNames /= length uniqueClassNames
            then error "Duplicate class names found"
            else ()

        -- Ensure the epsilon class (represented by the symbol 'E') is present
        _ = if any (== [epsilonSymbol]) classNames
            then ()
            else error "No epsilon class found in the table"

        -- Create a mapping from class names to state IDs
        classToState = Map.fromList $ zip classNames [0..]

        -- Initialize states: list of state IDs
        statesList = [0..(Map.size classToState - 1)]

        -- Extract and deduplicate the alphabet from all transitions
        alphabetList = nub [ head c | (_, c, _) <- table, not (null c), head c /= epsilonSymbol ] -- Only non-epsilon symbols

        -- The initial state is the one corresponding to the epsilon symbol ('E')
        initialStateId = classToState Map.! [epsilonSymbol]

        -- Accepting states: states with value 1 in the third column of the table
        acceptingStatesList = [ stateId | (className, _, val) <- table
                                        , val == 1
                                        , stateId <- maybeToList (Map.lookup className classToState) ]

        -- Build transitions from the table
        transitionsList =
            [ ((classToState Map.! s, head t), classToState Map.! s')
                | (s, t, _) <- table  -- (s: current state, t: transition string, _: accepting flag)
                , not (null t)        -- Only consider non-epsilon transitions
                , t /= [epsilonSymbol] -- Exclude self-loops on epsilon
                , let s' = findNextState s t
                , Map.member s' classToState -- Ensure the target state exists
            ]

        -- Function to find the next state based on the current state and transition string
        findNextState :: String -> String -> String
        findNextState s t =
            case [s'' | (s'', t', _) <- table, s == s'' && t == t'] of
                (s':_) -> s'  -- Return the first matching next state
                _ -> error $ "No matching state found for state " ++ s ++ " and transition " ++ t

        -- Construct the transition map
        transitions = Map.fromList transitionsList

    in Automaton
        { states = statesList
        , alphabet = alphabetList
        , transitions = transitions
        , initialState = initialStateId
        , acceptingStates = acceptingStatesList
        }
--[("E", "a", 0), ("a", "E", 1)]
--[("E", "a", 0), ("a", "b", 1), ("ab", "E", 0)]