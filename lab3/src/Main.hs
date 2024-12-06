module Main where

import Grammar

main :: IO ()
main = do
    let grammarStr = "S -> AB\nA -> a\nB -> b"
    case parseGrammar grammarStr of
        Just grammar -> putStrLn (printGrammar grammar)
        Nothing -> putStrLn "Failed to parse grammar"