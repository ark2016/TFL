{-# LANGUAGE OverloadedStrings #-}

module AutomatonFromEquivalenceTableSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Automaton (Automaton(..))
import AutomatonFromEquivalenceTable (fromEquivalenceTable)

spec :: Spec
spec = do
  describe "fromEquivalenceTable" $ do

    it "creates a simple automaton from a basic equivalence table with epsilon represented as 'E'" $ do
      let table = [("E", "a", 0), ("a", "E", 1)]
          automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1]
      alphabet automaton `shouldBe` ['a']
      transitions automaton `shouldBe` Map.fromList [((0,'a'),0)]
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [1]

    it "creates a more complex automaton from a larger equivalence table with epsilon represented as 'E'" $ do
      let table = [("E", "a", 0), ("a", "b", 0), ("ab", "a", 0), ("aba", "E", 1)]
          automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2, 3]
      alphabet automaton `shouldBe` ['a', 'b']
      transitions automaton `shouldBe` Map.fromList [((0,'a'),0),((1,'b'),1),((2,'a'),2)]
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [3]

    it "handles an equivalence table with multiple accepting states and epsilon represented as 'E'" $ do
      let table = [("E", "a", 1), ("a", "b", 1), ("ab", "E", 0)]
          automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2]
      alphabet automaton `shouldBe` ['a', 'b']
      transitions automaton `shouldBe` Map.fromList  [((0,'a'),0),((1,'b'),1)]
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [0, 1]

    it "creates an automaton from an equivalence table representing transitions with the same symbol" $ do
      let table = [("E", "a", 0), ("a", "a", 1), ("aa", "E", 0)]
          automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2]
      alphabet automaton `shouldBe` ['a']
      transitions automaton `shouldBe` Map.fromList [((0,'a'),0),((1,'a'),1)]
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [1]

    it "creates an automaton with a single state when given only an epsilon class" $ do
      let table = [("E", "E", 1)]
          automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0]
      alphabet automaton `shouldBe` []
      transitions automaton `shouldBe` Map.empty
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [0]

    it "creates an automaton with complex transitions and multiple accepting states" $ do
      let table = [("E", "a", 0), ("a", "b", 0), ("b", "c", 1), ("c", "a", 1), ("a", "E", 1)]
          automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2, 3]
      alphabet automaton `shouldBe` ['a', 'b', 'c']
      transitions automaton `shouldBe` Map.fromList  [((0,'a'),0),((2,'c'),2),((3,'a'),3),((4,'b'),4)]
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe`  [2, 3, 4]