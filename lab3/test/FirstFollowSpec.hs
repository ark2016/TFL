{-# LANGUAGE OverloadedStrings #-}

module FirstFollowSpec where

import Test.Hspec
import FirstFollow
import Grammar
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "FirstFollow.computeFirst and computeFollow" $ do
    it "корректно вычисляет множества First и Follow для простой грамматики в CNF" $ do
        -- Грамматика в CNF:
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
        let first = computeFirst grammar
        let follow = computeFollow grammar
        -- Проверка множеств First
        first Map.! NonTerminal "S" `shouldBe` Set.fromList [Terminal 'a']
        first Map.! NonTerminal "A" `shouldBe` Set.fromList [Terminal 'a']
        first Map.! NonTerminal "X" `shouldBe` Set.fromList [Terminal 'b']
        first Map.! NonTerminal "B" `shouldBe` Set.fromList [Terminal 'b']
        first Map.! NonTerminal "C" `shouldBe` Set.fromList [Terminal 'c']
        -- Проверка множеств Follow
        follow Map.! NonTerminal "S" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "A" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "X" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "B" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "C" `shouldBe` Set.fromList [Terminal '$']

    it "корректно вычисляет множества First и Follow для грамматики с epsilon-правилами в CNF" $ do
        -- Грамматика в CNF:
        -- S -> A X | ε
        -- X -> B
        -- A -> a
        -- B -> b
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "X"]),
                (NonTerminal "S", [Epsilon]),
                (NonTerminal "X", [NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let first = computeFirst grammar
        let follow = computeFollow grammar
        -- Проверка множеств First
        first Map.! NonTerminal "S" `shouldBe` Set.fromList [Terminal 'a', Epsilon]
        first Map.! NonTerminal "A" `shouldBe` Set.fromList [Terminal 'a']
        first Map.! NonTerminal "X" `shouldBe` Set.fromList [Terminal 'b']
        first Map.! NonTerminal "B" `shouldBe` Set.fromList [Terminal 'b']
        -- Проверка множеств Follow
        follow Map.! NonTerminal "S" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "A" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "X" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "B" `shouldBe` Set.fromList [Terminal '$']

    it "корректно вычисляет множества First и Follow для грамматики с цепными правилами в CNF" $ do
        -- Грамматика в CNF:
        -- S -> A X
        -- X -> B Y
        -- Y -> C
        -- A -> a
        -- B -> b
        -- C -> c
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "X"]),
                (NonTerminal "X", [NonTerminal "B", NonTerminal "Y"]),
                (NonTerminal "Y", [NonTerminal "C"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "C", [Terminal 'c'])
              ]
        let first = computeFirst grammar
        let follow = computeFollow grammar
        -- Проверка множеств First
        first Map.! NonTerminal "S" `shouldBe` Set.fromList [Terminal 'a']
        first Map.! NonTerminal "A" `shouldBe` Set.fromList [Terminal 'a']
        first Map.! NonTerminal "X" `shouldBe` Set.fromList [Terminal 'b']
        first Map.! NonTerminal "B" `shouldBe` Set.fromList [Terminal 'b']
        first Map.! NonTerminal "Y" `shouldBe` Set.fromList [Terminal 'c']
        first Map.! NonTerminal "C" `shouldBe` Set.fromList [Terminal 'c']
        -- Проверка множеств Follow
        follow Map.! NonTerminal "S" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "A" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "X" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "B" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "Y" `shouldBe` Set.fromList [Terminal '$']
        follow Map.! NonTerminal "C" `shouldBe` Set.fromList [Terminal '$']
