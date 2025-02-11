{-# LANGUAGE OverloadedStrings #-}

module BigramMatrix (
    buildBigramMatrix
) where

import Grammar
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Map.Strict (Map)

-- | Строит матрицу биграмм терминалов на основе грамматики и множеств
buildBigramMatrix :: Grammar
                  -> Map Symbol (Set Symbol) -- First
                  -> Map Symbol (Set Symbol) -- Follow
                  -> Map Symbol (Set Symbol) -- Last
                  -> Map Symbol (Set Symbol) -- Precede
                  -> Set (Symbol, Symbol)
buildBigramMatrix grammar firstMap followMap lastMap precedeMap =
    Set.fromList existingBigrams `Set.union` additionalBigrams
  where
    (Grammar rules) = grammar

    -- 1. Биграммы, которые встречаются в правой части правил
    existingBigrams = [ (gamma1, gamma2) |
        (NonTerminal lhs, rhs) <- rules,
        let bigrams = zip rhs (tail rhs),
        (gamma1, gamma2) <- bigrams
      ]

    -- 2. Биграммы на основе Last и Follow
    condition2 = [ (gamma1, gamma2) |
        (NonTerminal nt1, lastSet) <- Map.toList lastMap,
        gamma1 <- Set.toList lastSet,
        gamma2 <- Set.toList (Map.findWithDefault Set.empty (NonTerminal nt1) followMap)
      ]

    -- 3. Биграммы на основе Precede и First
    condition3 = [ (gamma1, gamma2) |
        (NonTerminal nt2, precedeSet) <- Map.toList precedeMap,
        gamma1 <- Set.toList precedeSet,
        gamma2 <- Set.toList (Map.findWithDefault Set.empty (NonTerminal nt2) firstMap)
      ]

    -- 4. Биграммы на основе Last, First и Follow
    condition4 = [ (gamma1, gamma2) |
        (NonTerminal nt1, lastSet) <- Map.toList lastMap,
        gamma1 <- Set.toList lastSet,
        (NonTerminal nt2, firstSetA) <- Map.toList firstMap,
        gamma2 <- Set.toList firstSetA,
        (NonTerminal nt2) `Set.member` Map.findWithDefault Set.empty (NonTerminal nt1) followMap
      ]

    additionalBigrams = Set.fromList (condition2 ++ condition3 ++ condition4)
