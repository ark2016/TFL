{-# LANGUAGE OverloadedStrings #-}

module AutomatonFromEquivalenceTableSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Automaton (Automaton(..))
import AutomatonFromEquivalenceTable (fromEquivalenceTable)

spec :: Spec
spec = do
  describe "fromEquivalenceTable" $ do
    it "creates a basic automaton from a simple table" $ do
      let table = [("epsilon", 0), ("class_1", 1), ("class_2", 0)]
      let automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2]
      alphabet automaton `shouldBe` ['a']
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [1]
      Map.keys (transitions automaton) `shouldMatchList` [(0,'a'), (1,'a'), (2,'a')]

    it "handles a table with only epsilon" $ do
      let table = [("epsilon", 1)]
      let automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0]
      alphabet automaton `shouldBe` ['a']
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [0]
      transitions automaton `shouldBe` Map.empty

    it "creates correct transitions for a larger table" $ do
      let table = [("epsilon", 0), ("class_1", 1), ("class_2", 0), ("class_3", 1)]
      let automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2, 3]
      alphabet automaton `shouldBe` ['a']
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [1, 3]
      Map.keys (transitions automaton) `shouldMatchList` [(0,'a'), (1,'a'), (2,'a'), (3,'a')]

    it "handles a table with all accepting states" $ do
      let table = [("epsilon", 1), ("class_1", 1), ("class_2", 1)]
      let automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2]
      alphabet automaton `shouldBe` ['a']
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [0, 1, 2]

    it "handles a table with no accepting states" $ do
      let table = [("epsilon", 0), ("class_1", 0), ("class_2", 0)]
      let automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2]
      alphabet automaton `shouldBe` ['a']
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` []

    it "creates correct transitions for a cyclic table" $ do
      let table = [("epsilon", 0), ("class_1", 1), ("class_2", 0)]
      let automaton = fromEquivalenceTable table
      transitions automaton `shouldBe` Map.fromList [((0,'a'),1), ((1,'a'),2), ((2,'a'),0)]

    it "handles a table with duplicate class names" $ do
      let table = [("epsilon", 0), ("class_1", 1), ("class_1", 0)]
      let automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2]
      acceptingStates automaton `shouldBe` [1]

    it "creates an automaton with correct structure for a complex table" $ do
      let table = [("epsilon", 0), ("a", 0), ("aa", 1), ("aaa", 0), ("aaaa", 1)]
      let automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2, 3, 4]
      alphabet automaton `shouldBe` ['a']
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [2, 4]
      Map.keys (transitions automaton) `shouldMatchList` [(0,'a'), (1,'a'), (2,'a'), (3,'a'), (4,'a')]

    it "handles a table with non-standard class names" $ do
      let table = [("epsilon", 0), ("state_1", 1), ("state_2", 0), ("final_state", 1)]
      let automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2, 3]
      acceptingStates automaton `shouldBe` [1, 3]

    it "creates an automaton with a single state when given only epsilon" $ do
      let table = [("epsilon", 1)]
      let automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0]
      alphabet automaton `shouldBe` ['a']
      initialState automaton `shouldBe` 0
      acceptingStates automaton `shouldBe` [0]
      transitions automaton `shouldBe` Map.empty

    it "preserves the order of states based on the input table" $ do
      let table = [("epsilon", 0), ("class_2", 1), ("class_1", 0)]
      let automaton = fromEquivalenceTable table
      states automaton `shouldBe` [0, 1, 2]
      Map.keys (transitions automaton) `shouldMatchList` [(0,'a'), (1,'a'), (2,'a')]

    it "handles a table with all classes as accepting states except epsilon" $ do
      let table = [("epsilon", 0), ("class_1", 1), ("class_2", 1), ("class_3", 1)]
      let automaton = fromEquivalenceTable table
      acceptingStates automaton `shouldBe` [1, 2, 3]

    it "creates correct transitions for a larger cyclic table" $ do
      let table = [("epsilon", 0), ("a", 1), ("aa", 0), ("aaa", 1), ("aaaa", 0)]
      let automaton = fromEquivalenceTable table
      transitions automaton `shouldBe` Map.fromList [((0,'a'),1), ((1,'a'),2), ((2,'a'),3), ((3,'a'),4), ((4,'a'),0)]

