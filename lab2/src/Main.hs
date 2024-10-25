-- Main module that ties together the functionalities
module Main where

import Automaton (Automaton(..))
import RandomAutomatonGenerator (generateRandomAutomaton)
import AutomatonInclusion (isStringAccepted)
import AutomatonVisualization (visualizeAutomaton)
import AutomatonFromEquivalenceTable (fromEquivalenceTable)
import AutomatonEquivalence (areAutomataEquivalent)
import System.IO (hFlush, stdout)

-- Main function
main :: IO ()
main = do
--    putStrLn "Generating a random automaton..."
    let automaton = generateRandomAutomaton 1  -- You can change the size parameter as needed
--    putStrLn "Automaton generated."
    mainLoop automaton

-- Main loop to handle user interactions
mainLoop :: Automaton -> IO ()
mainLoop automaton = do
--    putStrLn "\nSelect an option:"
--    putStrLn "1. Check if a string is accepted by the automaton"
--    putStrLn "2. Check if another automaton (from equivalence table) is equivalent to the generated automaton"
--    putStrLn "3. Visualize the automaton"
--    putStrLn "4. Exit"
--    putStr "Enter your choice: "
    hFlush stdout
    option <- getLine
    case option of
        "1" -> do
--            inputStr <- getLine
--            isStringAccepted automaton inputStr
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
--    putStr "Enter the string to check: "
    hFlush stdout
    inputStr <- getLine
    let result = isStringAccepted automaton inputStr
    if result == 1
        then putStrLn "1"
        else putStrLn "0"
--        then putStrLn "String is accepted by the automaton."
--        else putStrLn "String is NOT accepted by the automaton."

-- Function to check if another automaton is equivalent to the generated one
checkAutomatonEquivalence :: Automaton -> IO ()
checkAutomatonEquivalence automaton = do
--    putStrLn "Enter the equivalence table as a list of (String, Int) pairs. For example: [(\"a\",1),(\"b\",2)]"
--    putStr "Equivalence table: "
    hFlush stdout
    eqTableInput <- getLine
    let maybeEqTable = reads eqTableInput :: [([(String, Int)], String)]
    case maybeEqTable of
        [(eqTable, "")] -> do
            let enteredAutomaton = fromEquivalenceTable eqTable
            let equivalenceResult = areAutomataEquivalent automaton enteredAutomaton
            case equivalenceResult of
--                Right True  -> putStrLn "The automata are equivalent."
                Right True -> putStrLn "1"
                Left msg -> putStrLn msg
--                Right False -> putStrLn "The automata are NOT equivalent."
--                Left errMsg -> putStrLn $ "Error during equivalence checking: " ++ errMsg
        _ -> putStrLn "Invalid input. Please enter a valid equivalence table."