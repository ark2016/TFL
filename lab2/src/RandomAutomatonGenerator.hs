-- RandomAutomatonGenerator.hs
module RandomAutomatonGenerator
    ( generateRandomAutomaton
    , generateVariableAutomaton
    , generateConstantAutomaton
    , generateEolAutomaton
    , generateBlankAutomaton
    , generateEqualAutomaton
    , generateSepAutomaton
    , generateBracketAutomaton
    ) where

import Automaton

import qualified Data.Map as Map

-- Generates a random automaton according to a given type
generateRandomAutomaton :: Int -> Automaton
generateRandomAutomaton lexemeType = case lexemeType of
    1 -> generateVariableAutomaton
    2 -> generateConstantAutomaton
    3 -> generateEolAutomaton
    4 -> generateBlankAutomaton
    5 -> generateEqualAutomaton
    6 -> generateSepAutomaton
    7 -> generateBracketAutomaton 1
    _ -> generateVariableAutomaton  -- Default case

-- Generates a simple automaton for variables
-- Variables: Infinite language over {'a','b','c'}, disjoint from constants
generateVariableAutomaton :: Automaton
generateVariableAutomaton = generateAutomatonOverAlphabet ['a','b','c']

-- Generates a simple automaton for constants
-- Constants: Infinite language over {'0','1','2'}, disjoint from variables
generateConstantAutomaton :: Automaton
generateConstantAutomaton = generateAutomatonOverAlphabet ['0','1','2']

-- Helper function to create an automaton over a given alphabet
generateAutomatonOverAlphabet :: [Char] -> Automaton
generateAutomatonOverAlphabet alphabet =
    let states = [0..2]
        initialState = 0
        acceptingStates = [1, 2]
        transitions = Map.fromList
            [ ((0, head alphabet), 1)
            , ((1, last alphabet), 2)
            , ((2, head alphabet), 0)
            ]
    in Automaton { states = states
                 , alphabet = alphabet
                 , transitions = transitions
                 , initialState = initialState
                 , acceptingStates = acceptingStates }

-- Generates a simple automaton for [eol]
-- [eol]: Alphabet different from all other lexemes
generateEolAutomaton :: Automaton
generateEolAutomaton = 
    let states = [0, 1]
        alphabet = ['\n']  -- Using newline character for [eol]
        transitions = Map.fromList [ ((0, '\n'), 1) ]
        initialState = 0
        acceptingStates = [1]
    in Automaton { states = states
                 , alphabet = alphabet
                 , transitions = transitions
                 , initialState = initialState
                 , acceptingStates = acceptingStates }

-- Generates a simple automaton for [blank]
-- [blank]: Alphabet different from all other lexemes and from [eol]
generateBlankAutomaton :: Automaton
generateBlankAutomaton = 
    let states = [0, 1]
        alphabet = [' ']  -- Using space character for [blank]
        transitions = Map.fromList [ ((0, ' '), 1) ]
        initialState = 0
        acceptingStates = [1]
    in Automaton { states = states
                 , alphabet = alphabet
                 , transitions = transitions
                 , initialState = initialState
                 , acceptingStates = acceptingStates }

-- Generates a simple automaton for [equal]
-- [equal]: Finite language, first and last letters in a different alphabet
generateEqualAutomaton :: Automaton
generateEqualAutomaton = 
    let states = [0, 1]
        alphabet = ['=']  -- Using '=' character for [equal]
        transitions = Map.fromList [ ((0, '='), 1) ]
        initialState = 0
        acceptingStates = [1]
    in Automaton { states = states
                 , alphabet = alphabet
                 , transitions = transitions
                 , initialState = initialState
                 , acceptingStates = acceptingStates }

-- Generates a simple automaton for [sep]
-- [sep]: Finite language, first and last letters in a different alphabet
generateSepAutomaton :: Automaton
generateSepAutomaton = 
    let states = [0, 1]
        alphabet = [';']  -- Using ';' character for [sep]
        transitions = Map.fromList [ ((0, ';'), 1) ]
        initialState = 0
        acceptingStates = [1]
    in Automaton { states = states
                 , alphabet = alphabet
                 , transitions = transitions
                 , initialState = initialState
                 , acceptingStates = acceptingStates }

-- Generates a simple automaton for brackets
-- Brackets have pairwise disjoint languages disjoint from variables/constants
generateBracketAutomaton :: Int -> Automaton
generateBracketAutomaton bracketType = 
    let bracketChar = case bracketType of
            1 -> '{'  -- [lbr-1]
            2 -> '}'  -- [rbr-1]
            3 -> '['  -- [lbr-2]
            4 -> ']'  -- [rbr-2]
            5 -> '('  -- [lbr-3]
            6 -> ')'  -- [rbr-3]
            _ -> '{'
        states = [0, 1]
        alphabet = [bracketChar]
        transitions = Map.fromList [ ((0, bracketChar), 1) ]
        initialState = 0
        acceptingStates = [1]
    in Automaton { states = states
                 , alphabet = alphabet
                 , transitions = transitions
                 , initialState = initialState
                 , acceptingStates = acceptingStates }