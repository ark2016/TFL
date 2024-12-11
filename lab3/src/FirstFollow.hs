{-# LANGUAGE OverloadedStrings #-}

module FirstFollow (
    computeFirst,
    computeFollow
) where

import Grammar
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.List (foldl')

-- Фиксированная точка для итеративного вычисления множеств
fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x
    | x == x'   = x
    | otherwise = fixPoint f x'
  where x' = f x

computeFirst :: Grammar -> Map Symbol (Set Symbol)
computeFirst (Grammar rules) = fixPoint updateFirst initialFirst
  where
    initialFirst :: Map Symbol (Set Symbol)
    initialFirst = Map.fromList [(lhs, Set.empty) | (lhs, _) <- rules]

    updateFirst :: Map Symbol (Set Symbol) -> Map Symbol (Set Symbol)
    updateFirst acc = foldl' (updateRule acc) acc rules

    updateRule :: Map Symbol (Set Symbol) -> Map Symbol (Set Symbol) -> Rule -> Map Symbol (Set Symbol)
    updateRule oldAcc acc (lhs, rhs) =
        let firstSet = calcFirstFromRhs rhs oldAcc
        in Map.insertWith Set.union lhs firstSet acc

    -- Вычисляет First для цепочки символов rhs, используя уже вычисленные множества First из acc
    calcFirstFromRhs :: [Symbol] -> Map Symbol (Set Symbol) -> Set Symbol
    calcFirstFromRhs [] _ = Set.singleton Epsilon
    calcFirstFromRhs (x:xs) acc =
        case x of
            Terminal t -> Set.singleton (Terminal t)
            NonTerminal nt ->
                let firstNt = Map.findWithDefault Set.empty (NonTerminal nt) acc
                    -- Если в firstNt есть ε, объединяем (firstNt без ε) с First оставшейся части
                in if Set.member Epsilon firstNt
                   then Set.union (Set.delete Epsilon firstNt) (calcFirstFromRhs xs acc)
                   else firstNt
            Epsilon -> Set.insert Epsilon (calcFirstFromRhs xs acc)

computeFollow :: Grammar -> Map Symbol (Set Symbol)
computeFollow grammar@(Grammar rules) = fixPoint updateFollow initialFollow
  where
    startSymbol :: Symbol
    startSymbol = getStartSymbol grammar

    initialFollow :: Map Symbol (Set Symbol)
    initialFollow = Map.fromList [ (lhs, if lhs == startSymbol then Set.singleton (Terminal '$') else Set.empty) | (lhs, _) <- rules ]

    updateFollow :: Map Symbol (Set Symbol) -> Map Symbol (Set Symbol)
    updateFollow acc = foldl' updateRule acc rules

    updateRule :: Map Symbol (Set Symbol) -> Rule -> Map Symbol (Set Symbol)
    updateRule acc (lhs, rhs) =
        foldl' (processSymbol lhs rhs) acc [0 .. length rhs - 1]

    processSymbol :: Symbol -> [Symbol] -> Map Symbol (Set Symbol) -> Int -> Map Symbol (Set Symbol)
    processSymbol lhs rhs acc i =
        case rhs !! i of
            NonTerminal nt ->
                let following = drop (i + 1) rhs
                    firstOfFollowing = calcFirstFromRhs following acc
                    withoutEpsilon = Set.delete Epsilon firstOfFollowing
                    acc' = Map.insertWith Set.union (NonTerminal nt) withoutEpsilon acc
                    acc'' = if Set.member Epsilon firstOfFollowing
                            then Map.insertWith Set.union (NonTerminal nt) (Map.findWithDefault Set.empty lhs acc) acc'
                            else acc'
                in acc''

            _ -> acc

    -- Используем ту же функцию, что и для computeFirst, чтобы получить First оставшейся цепочки
    calcFirstFromRhs :: [Symbol] -> Map Symbol (Set Symbol) -> Set Symbol
    calcFirstFromRhs [] _ = Set.singleton Epsilon
    calcFirstFromRhs (x:xs) acc =
        case x of
            Terminal t -> Set.singleton (Terminal t)
            NonTerminal nt ->
                let firstNt = Map.findWithDefault Set.empty (NonTerminal nt) acc
                in if Set.member Epsilon firstNt
                   then Set.union (Set.delete Epsilon firstNt) (calcFirstFromRhs xs acc)
                   else firstNt
            Epsilon -> Set.insert Epsilon (calcFirstFromRhs xs acc)

    getStartSymbol :: Grammar -> Symbol
    getStartSymbol (Grammar []) = error "Empty grammar"
    getStartSymbol (Grammar ((NonTerminal s, _):_)) = NonTerminal s
    getStartSymbol _ = error "Invalid grammar format"
