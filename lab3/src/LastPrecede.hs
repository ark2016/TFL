{-# LANGUAGE OverloadedStrings #-}

module LastPrecede (
    computeLast,
    computePrecede
) where

import Grammar
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.List (foldl')

-- Функция фиксированной точки
fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x
    | x == x'   = x
    | otherwise = fixPoint f x'
  where x' = f x

-- | Вычисляет множество Last для каждого нетерминала
computeLast :: Grammar -> Map Symbol (Set Symbol)
computeLast (Grammar rules) = fixPoint updateLast initialLast
  where
    initialLast :: Map Symbol (Set Symbol)
    initialLast = Map.fromList [ (lhs, Set.empty) | (lhs, _) <- rules ]

    updateLast :: Map Symbol (Set Symbol) -> Map Symbol (Set Symbol)
    updateLast acc = foldl' updateRule acc rules

    updateRule :: Map Symbol (Set Symbol) -> Rule -> Map Symbol (Set Symbol)
    updateRule acc (lhs, rhs) =
        let
            lastSet = computeLastFromRhs (reverse rhs) acc
        in Map.insertWith Set.union lhs lastSet acc

    -- Вычисляет Last для цепочки символов rhs, используя уже вычисленные множества Last из acc
    computeLastFromRhs :: [Symbol] -> Map Symbol (Set Symbol) -> Set Symbol
    computeLastFromRhs [] _ = Set.empty
    computeLastFromRhs (x:xs) acc =
        case x of
            Terminal t -> Set.singleton (Terminal t)
            NonTerminal nt ->
                let lastNt = Map.findWithDefault Set.empty (NonTerminal nt) acc
                in if Set.member Epsilon lastNt
                   then Set.union (Set.delete Epsilon lastNt) (computeLastFromRhs xs acc)
                   else lastNt
            Epsilon -> Set.singleton Epsilon

-- | Вычисляет множество Precede для каждого нетерминала
computePrecede :: Grammar -> Map Symbol (Set Symbol)
computePrecede (Grammar rules) = fixPoint updatePrecede initialPrecede
  where
    initialPrecede :: Map Symbol (Set Symbol)
    initialPrecede = Map.fromList [(lhs, Set.empty) | (lhs, _) <- rules]

    updatePrecede :: Map Symbol (Set Symbol) -> Map Symbol (Set Symbol)
    updatePrecede acc = foldl' updateRule acc rules

    updateRule :: Map Symbol (Set Symbol) -> Rule -> Map Symbol (Set Symbol)
    updateRule acc (lhs, rhs) =
        foldl' processPrecede acc [1 .. length rhs - 1]
      where
        processPrecede :: Map Symbol (Set Symbol) -> Int -> Map Symbol (Set Symbol)
        processPrecede a i =
            let
                currentSymbol = rhs !! i
                precedingSymbol = rhs !! (i -1)
            in case currentSymbol of
                NonTerminal _ ->
                    Map.insertWith Set.union currentSymbol (Set.singleton precedingSymbol) a
                _ -> a
