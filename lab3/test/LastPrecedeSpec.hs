{-# LANGUAGE OverloadedStrings #-}

module LastPrecedeSpec where

import Test.Hspec
import LastPrecede
import Grammar
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "LastPrecede.computeLast and computePrecede" $ do
    it "вычисляет множества Last и Precede для простой грамматики в CNF" $ do
        -- Пример грамматики в CNF:
        -- S -> A B
        -- A -> a
        -- B -> b
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let lastSets = computeLast grammar
        let precedeSets = computePrecede grammar

        -- Проверка множества Last:
        -- Last(S) = Last(B), так как S -> A B, а B -> b, Last(B) = {b}, так Last(S) = {b}
        lastSets Map.! NonTerminal "S" `shouldBe` Set.fromList [Terminal 'b']
        lastSets Map.! NonTerminal "A" `shouldBe` Set.fromList [Terminal 'a']
        lastSets Map.! NonTerminal "B" `shouldBe` Set.fromList [Terminal 'b']

        -- Проверка множества Precede:
        -- Precede(B) должен содержать символы, которые могут предшествовать B
        -- В S -> A B, A может предшествовать B, но A -> a => следовательно, Precede(B) = {A}, но по определению Precede добавляет предшествующие символы (которые могут быть терминалами или нетерминалами)
        -- Здесь мы помещаем непосредственно символ, предшествующий B. Предшествующий символ - это A (нетерминал).
        precedeSets Map.! NonTerminal "B" `shouldBe` Set.fromList [NonTerminal "A"]
