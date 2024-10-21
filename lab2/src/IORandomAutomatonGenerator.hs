module IORandomAutomatonGenerator
    ( generateRandomAutomaton
    ) where

import Automaton

import qualified Data.Map as Map
import System.Random (randomRIO)
import Control.Monad (filterM)

-- Generates a random automaton according to the constraints
generateRandomAutomaton :: IO Automaton
generateRandomAutomaton = do
    -- Randomly choose which lexeme automaton to generate
    lexemeType <- randomRIO (1, 7) :: IO Int
    case lexemeType of
        1 -> generateVariableAutomaton
        2 -> generateConstantAutomaton
        3 -> generateEolAutomaton
        4 -> generateBlankAutomaton
        5 -> generateEqualAutomaton
        6 -> generateSepAutomaton
        7 -> generateBracketAutomaton
        _ -> generateVariableAutomaton  -- Default case

-- Generates a random automaton for variables
-- Variables: Infinite language over {'a','b','c'}, disjoint from constants
generateVariableAutomaton :: IO Automaton
generateVariableAutomaton = generateRandomAutomatonOverAlphabet ['a','b','c']

-- Generates a random automaton for constants
-- Constants: Infinite language over {'0','1','2'}, disjoint from variables
generateConstantAutomaton :: IO Automaton
generateConstantAutomaton = generateRandomAutomatonOverAlphabet ['0','1','2']

-- Helper function to generate a random automaton over a given alphabet
generateRandomAutomatonOverAlphabet :: [Char] -> IO Automaton
generateRandomAutomatonOverAlphabet alphabet = do
    numStates <- randomRIO (2, 5) :: IO Int
    let states = [0..(numStates - 1)]
    let initialState = 0
    -- Exclude initial state from accepting states to ensure non-empty strings
    let otherStates = tail states
    acceptingStates <- randomNonEmptySubset otherStates
    transitions <- generateRandomTransitions states alphabet
    let automaton = Automaton { states = states
                              , alphabet = alphabet
                              , transitions = transitions
                              , initialState = initialState
                              , acceptingStates = acceptingStates }
    return automaton

-- Generates a random automaton for [eol]
-- [eol]: Alphabet different from all other lexemes
generateEolAutomaton :: IO Automaton
generateEolAutomaton = do
    let states = [0, 1]
    let alphabet = ['\n']  -- Using newline character for [eol]
    let transitions = Map.fromList [ ((0, '\n'), 1) ]
    let initialState = 0
    let acceptingStates = [1]
    return Automaton { states = states
                     , alphabet = alphabet
                     , transitions = transitions
                     , initialState = initialState
                     , acceptingStates = acceptingStates }

-- Generates a random automaton for [blank]
-- [blank]: Alphabet different from all other lexemes and from [eol]
generateBlankAutomaton :: IO Automaton
generateBlankAutomaton = do
    let states = [0, 1]
    let alphabet = [' ']  -- Using space character for [blank]
    let transitions = Map.fromList [ ((0, ' '), 1) ]
    let initialState = 0
    let acceptingStates = [1]
    return Automaton { states = states
                     , alphabet = alphabet
                     , transitions = transitions
                     , initialState = initialState
                     , acceptingStates = acceptingStates }

-- Generates a random automaton for [equal]
-- [equal]: Finite language, first and last letters in a different alphabet
generateEqualAutomaton :: IO Automaton
generateEqualAutomaton = do
    let states = [0,1]
    let alphabet = ['=']  -- Using '=' character for [equal]
    let transitions = Map.fromList [ ((0,'='),1) ]
    let initialState = 0
    let acceptingStates = [1]
    return Automaton { states = states
                     , alphabet = alphabet
                     , transitions = transitions
                     , initialState = initialState
                     , acceptingStates = acceptingStates }

-- Generates a random automaton for [sep]
-- [sep]: Finite language, first and last letters in a different alphabet
generateSepAutomaton :: IO Automaton
generateSepAutomaton = do
    let states = [0,1]
    let alphabet = [';']  -- Using ';' character for [sep]
    let transitions = Map.fromList [ ((0,';'),1) ]
    let initialState = 0
    let acceptingStates = [1]
    return Automaton { states = states
                     , alphabet = alphabet
                     , transitions = transitions
                     , initialState = initialState
                     , acceptingStates = acceptingStates }

-- Generates a random automaton for brackets
-- Brackets have pairwise disjoint languages disjoint from variables/constants
generateBracketAutomaton :: IO Automaton
generateBracketAutomaton = do
    -- Randomly choose a bracket type
    bracketType <- randomRIO (1, 6) :: IO Int
    let bracketChar = case bracketType of
            1 -> '{'  -- [lbr-1]
            2 -> '}'  -- [rbr-1]
            3 -> '['  -- [lbr-2]
            4 -> ']'  -- [rbr-2]
            5 -> '('  -- [lbr-3]
            6 -> ')'  -- [rbr-3]
            _ -> '{'
    let states = [0,1]
    let alphabet = [bracketChar]
    let transitions = Map.fromList [ ((0, bracketChar),1) ]
    let initialState = 0
    let acceptingStates = [1]
    return Automaton { states = states
                     , alphabet = alphabet
                     , transitions = transitions
                     , initialState = initialState
                     , acceptingStates = acceptingStates }

-- Helper function to generate random transitions for an automaton
generateRandomTransitions :: [Int] -> [Char] -> IO (Map.Map (Int, Char) Int)
generateRandomTransitions states alphabet = do
    let statePairs = [(s, c) | s <- states, c <- alphabet]
    transitionsList <- mapM (\(s, c) -> do
        nextState <- randomChoice states
        return ((s, c), nextState)
        ) statePairs
    return $ Map.fromList transitionsList

-- Helper function to select a random element from a list
randomChoice :: [a] -> IO a
randomChoice xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs !! idx)

-- Helper function to generate a random non-empty subset of a list
randomNonEmptySubset :: [a] -> IO [a]
randomNonEmptySubset xs = do
    subset <- filterM (\_ -> randomRIO (False, True)) xs
    if null subset
        then randomNonEmptySubset xs
        else return subset