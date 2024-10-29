{-# LANGUAGE OverloadedStrings #-}

module AutomatonFromEquivalenceTableSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Automaton (Automaton(..))
import AutomatonFromEquivalenceTable (fromEquivalenceTable)

spec :: Spec
spec = do
  describe "fromEquivalenceTable" $ do
    it "creates a simple automaton from a basic equivalence table" $ do
      let table = [("", "a", 0), ("a", "", 1)]
          automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1]
      alphabet automaton `shouldBe` ['a']
      transitions automaton `shouldBe` Map.fromList [((0, 'a'), 1)]
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [1]

    it "creates a more complex automaton from a larger equivalence table" $ do
      let table = [("", "a", 0), ("a", "b", 0), ("ab", "a", 0), ("aba", "", 1)]
          automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2, 3]
      alphabet automaton `shouldBe` ['a', 'b']
      transitions automaton `shouldBe` Map.fromList [((0, 'a'), 1), ((1, 'b'), 2), ((2, 'a'), 3)]
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [3]

    it "handles an equivalence table with multiple accepting states" $ do
      let table = [("", "a", 1), ("a", "b", 1), ("ab", "", 0)]
          automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2]
      alphabet automaton `shouldBe` ['a', 'b']
      transitions automaton `shouldBe` Map.fromList [((0, 'a'), 1), ((1, 'b'), 2)]
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [0, 1]

    it "creates an automaton with a single state when given only an epsilon class" $ do
      let table = [("", "", 1)]
          automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0]
      alphabet automaton `shouldBe` []
      transitions automaton `shouldBe` Map.empty
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [0]