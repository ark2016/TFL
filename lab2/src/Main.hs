-- Main module that ties together the functionalities
module Main where

import Automaton (Automaton(..))
import AutomatonInclusion (isStringAccepted)
import AutomatonVisualization (visualizeAutomaton)
import AutomatonFromEquivalenceTable (fromEquivalenceTable)
import AutomatonEquivalence (areAutomataEquivalent)
import System.IO (hFlush, stdout, readFile)
import qualified Data.Map as Map

-- Main function
main :: IO ()
main = do
    -- Read the automaton from the file
    automatonText <- readFile "Automaton.txt"
    let automaton = read automatonText :: Automaton
    --let automaton = generateRandomAutomaton 1
    mainLoop automaton

-- Main loop to handle user interactions
mainLoop :: Automaton -> IO ()
mainLoop automaton = do
    hFlush stdout
    option <- getLine
    case option of
        "1" -> do
            checkStringInclusion automaton
            mainLoop automaton
        "2" -> do
            checkAutomatonEquivalence automaton
            mainLoop automaton
        "3" -> do
            visualizeAutomaton automaton
            mainLoop automaton
        "4" -> putStrLn "Exiting."
        _   -> do
            putStrLn "Invalid option. Please try again."
            mainLoop automaton

-- Function to check if a string is accepted by the automaton
checkStringInclusion :: Automaton -> IO ()
checkStringInclusion automaton = do
    hFlush stdout
    inputStr <- getLine
    let result = isStringAccepted automaton inputStr
    if result == 1
        then putStrLn "1"
        else putStrLn "0"

-- Function to check if another automaton is equivalent to the generated one
checkAutomatonEquivalence :: Automaton -> IO ()
checkAutomatonEquivalence automaton = do
    hFlush stdout
    eqTableInput <- getLine
    let maybeEqTable = reads eqTableInput :: [([(String, [(Char, String)], Int)], String)]
    case maybeEqTable of
        [(eqTable, "")] -> do
            let enteredAutomaton = fromEquivalenceTable eqTable
            let equivalenceResult = areAutomataEquivalent automaton enteredAutomaton
            case equivalenceResult of
                Right True -> putStrLn "1"
                Left msg -> putStrLn msg
        _ -> putStrLn "Invalid input. Please enter a valid equivalence table."