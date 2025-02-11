{-# LANGUAGE OverloadedStrings #-}

module BigramMatrixSpec where

import Test.Hspec
import BigramMatrix
import Grammar
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "BigramMatrix.buildBigramMatrix" $ do
    it "строит правильную матрицу биграмм для простой грамматики в CNF" $ do
        -- Определение простой грамматики в CNF:
        -- S -> A X
        -- X -> B C
        -- A -> a
        -- B -> b
        -- C -> c

        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "X"]),
                (NonTerminal "X", [NonTerminal "B", NonTerminal "C"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "C", [Terminal 'c'])
              ]

        -- Определение множеств First
        let firstMap = Map.fromList [
                (NonTerminal "S", Set.fromList [Terminal 'a']),
                (NonTerminal "A", Set.fromList [Terminal 'a']),
                (NonTerminal "X", Set.fromList [Terminal 'b']),
                (NonTerminal "B", Set.fromList [Terminal 'b']),
                (NonTerminal "C", Set.fromList [Terminal 'c'])
              ]

        -- Определение множеств Follow
        let followMap = Map.fromList [
                (NonTerminal "S", Set.fromList [Terminal '$']),
                (NonTerminal "A", Set.fromList [Terminal 'b']),
                (NonTerminal "X", Set.fromList [Terminal '$']),
                (NonTerminal "B", Set.fromList [Terminal 'c']),
                (NonTerminal "C", Set.fromList [Terminal '$'])
              ]

        -- Определение множеств Last
        let lastMap = Map.fromList [
                (NonTerminal "S", Set.fromList [Terminal 'c']),
                (NonTerminal "X", Set.fromList [Terminal 'c']),
                (NonTerminal "A", Set.fromList [Terminal 'a']),
                (NonTerminal "B", Set.fromList [Terminal 'b']),
                (NonTerminal "C", Set.fromList [Terminal 'c'])
              ]

        -- Определение множеств Precede
        let precedeMap = Map.fromList [
                (NonTerminal "A", Set.empty),
                (NonTerminal "X", Set.fromList [NonTerminal "S"]),
                (NonTerminal "B", Set.fromList [NonTerminal "X"]),
                (NonTerminal "C", Set.fromList [NonTerminal "X"]),
                (NonTerminal "S", Set.empty)
              ]

        -- Ожидаемые существующие биграммы
        let existingBigrams = Set.fromList [
                (NonTerminal "A", NonTerminal "X"),
                (NonTerminal "B", NonTerminal "C")
              ]

        -- Ожидаемые дополнительные биграммы из условий 2-4
        let expectedAdditionalBigrams = Set.fromList [
                -- Condition2: Last и Follow
                (Terminal 'c', Terminal '$'),
                (Terminal 'c', Terminal '$'),
                (Terminal 'a', Terminal 'b'),
                (Terminal 'b', Terminal 'c'),
                (Terminal 'c', Terminal '$'),

                -- Condition3: Precede и First
                (NonTerminal "S", Terminal 'b'),
                (NonTerminal "X", Terminal 'b'),
                (NonTerminal "X", Terminal 'c')
              ]

        -- Ожидаемый итоговый набор биграмм
        let expectedBigrams = Set.union existingBigrams expectedAdditionalBigrams

        -- Построение матрицы биграмм
        let bigramMatrix = buildBigramMatrix grammar firstMap followMap lastMap precedeMap

        -- Проверка, что построенная матрица совпадает с ожидаемой
        bigramMatrix `shouldBe` expectedBigrams
