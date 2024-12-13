{-# LANGUAGE OverloadedStrings #-}

module CYKParser (
    cykParse
) where

import Grammar
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

-- | Проверяет, принадлежит ли строка языку, заданному грамматикой в CNF, используя алгоритм CYK
cykParse :: Grammar -> String -> Bool
cykParse grammar input =
    if null input
    then canGenerateEpsilon
    else parseString table n
  where
    rules = case grammar of
        Grammar rs -> rs

    startSymbol = NonTerminal "S"
    n = length input

    -- Разделение правил на терминальные и нетерминальные
    terminalRules = [ (a, lhs) | (lhs, [Terminal a]) <- rules ]
    nonTerminalRules = [ ((NonTerminal lhs1, NonTerminal lhs2), lhs)
                       | (lhs, [NonTerminal lhs1, NonTerminal lhs2]) <- rules ]

    termToNonTermMap :: Map Char (Set Symbol)
    termToNonTermMap = Map.fromListWith Set.union [ (a, Set.singleton lhs) | (a, lhs) <- terminalRules ]

    pairToNonTermMap :: Map (Symbol, Symbol) (Set Symbol)
    pairToNonTermMap = Map.fromListWith Set.union
        [ ((NonTerminal lhs1, NonTerminal lhs2), Set.singleton lhs)
        | ((NonTerminal lhs1, NonTerminal lhs2), lhs) <- nonTerminalRules ]

    canGenerateEpsilon :: Bool
    canGenerateEpsilon =
        any (\lhs -> any (== [Epsilon]) [ rhs | (l, rhs) <- rules, l == lhs])
            [ lhs | (lhs, rhs) <- rules, rhs == [Epsilon]]

    -- Построение таблицы CYK:
    -- table[j][i] — множество нетерминалов, порождающих подстроку длиной j, начинающуюся с i
    table :: [[Set Symbol]]
    table =
        -- j — длина подстроки от 1 до n
        -- для каждого j — список из (n-j+1) элементов (подстрок такой длины в строке)
        [ [ cell i j | i <- [1..(n-j+1)] ] | j <- [1..n] ]
      where
        cell i 1 = terminalsAt i
        cell i j = combineCells i j

        terminalsAt i =
            let char = input !! (i - 1)
            in Map.findWithDefault Set.empty char termToNonTermMap

        combineCells i j =
            Set.unions
                [ Map.findWithDefault Set.empty (b, c) pairToNonTermMap
                | p <- [1..j-1]
                , let leftSet = (table !! (p-1)) !! (i-1)
                , let rightSet = (table !! (j-p-1)) !! (i+p-1)
                , b <- Set.toList leftSet
                , c <- Set.toList rightSet
                ]

    parseString tbl n =
        let finalSet = (tbl !! (n-1)) !! 0 -- подстрока длиной n, начинающаяся с 1
        in Set.member startSymbol finalSet
