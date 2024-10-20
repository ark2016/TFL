{-# LANGUAGE OverloadedStrings #-}

module AutomatonInclusionSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Automaton (Automaton(..))
import AutomatonInclusion (isStringAccepted)

spec :: Spec
spec = do
  describe "isStringAccepted" $ do
    it "returns 1 for a string accepted by the automaton" $ do
      let dfa = Automaton {
            states = [0, 1],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
            initialState = 0,
            acceptingStates = [1]
          }
      isStringAccepted dfa "a" `shouldBe` 1

    it "returns 0 for a string not accepted by the automaton" $ do
      let dfa = Automaton {
            states = [0, 1],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
            initialState = 0,
            acceptingStates = [1]
          }
      isStringAccepted dfa "b" `shouldBe` 0

    it "returns 1 for a longer string accepted by the automaton" $ do
      let dfa = Automaton {
            states = [0, 1, 2],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 2), ((1, 'b'), 1), ((2, 'a'), 2), ((2, 'b'), 2)],
            initialState = 0,
            acceptingStates = [2]
          }
      isStringAccepted dfa "aab" `shouldBe` 1

--    it "returns 0 for an invalid string (missing transitions)" $ do
--      let dfa = Automaton {
--            states = [0, 1],
--            alphabet = ['a'],
--            transitions = Map.fromList [((0, 'a'), 1)],
--            initialState = 0,
--            acceptingStates = [1]
--          }
--      isStringAccepted dfa "ab" `shouldBe` 0  -- No transition for 'b'

    it "returns 0 for an empty string when the initial state is not accepting" $ do
      let dfa = Automaton {
            states = [0, 1],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
            initialState = 0,
            acceptingStates = [1]
          }
      isStringAccepted dfa "" `shouldBe` 0

    it "returns 1 for an empty string when the initial state is accepting" $ do
      let dfa = Automaton {
            states = [0, 1],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
            initialState = 0,
            acceptingStates = [0]
          }
      isStringAccepted dfa "" `shouldBe` 1