{-# LANGUAGE OverloadedStrings #-}

module AutomatonEquivalenceSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Automaton (Automaton(..))
import AutomatonEquivalence (areAutomataEquivalent)

spec :: Spec
spec = do
  describe "areAutomataEquivalent" $ do
    it "returns True for identical automata" $ do
      let dfa = Automaton {
            states = [0, 1],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
            initialState = 0,
            acceptingStates = [1]
          }
      areAutomataEquivalent dfa dfa `shouldBe` Right True

    it "returns True for equivalent automata" $ do
      let dfa1 = Automaton {
            states = [0, 1],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
            initialState = 0,
            acceptingStates = [1]
          }
          dfa2 = Automaton {
            states = [0, 1, 2],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 2), ((2, 'a'), 1), ((2, 'b'), 2)],
            initialState = 0,
            acceptingStates = [1, 2]
          }
      areAutomataEquivalent dfa1 dfa2 `shouldBe` Right True

    it "returns False with a counterexample for non-equivalent automata" $ do
      let dfa1 = Automaton {
            states = [0, 1],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
            initialState = 0,
            acceptingStates = [1]
          }
          dfa2 = Automaton {
            states = [0, 1],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 0)],
            initialState = 0,
            acceptingStates = [1]
          }
      case areAutomataEquivalent dfa1 dfa2 of
        Left counterexample -> counterexample `shouldBe` "ab"
        Right _ -> expectationFailure "Expected Left, but got Right"

    it "handles automata with different alphabets" $ do
      let dfa1 = Automaton {
            states = [0, 1],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
            initialState = 0,
            acceptingStates = [1]
          }
          dfa2 = Automaton {
            states = [0, 1],
            alphabet = ['a', 'c'],
            transitions = Map.fromList [((0, 'a'), 1), ((0, 'c'), 0), ((1, 'a'), 1), ((1, 'c'), 1)],
            initialState = 0,
            acceptingStates = [1]
          }
      areAutomataEquivalent dfa1 dfa2 `shouldBe` Left "ab"