{-# LANGUAGE OverloadedStrings #-}

module AutomatonMinimizerSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set
import AutomatonMinimizer

-- Test suite for AutomatonMinimizer
spec :: Spec
spec = do
  describe "minimizeAutomaton" $ do
    it "minimizes a trivial DFA with one state" $ do
      let dfa = Automaton {
            states = [0],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [],
            initialState = 0,
            acceptingStates = [0]
          }
          expected = dfa  -- The DFA is already minimal
      minimizeAutomaton dfa `shouldBe` expected

    it "minimizes 1 https://www.gatevidyalay.com/minimization-of-dfa-minimize-dfa-example/" $ do
      let dfa = Automaton {
            states = [0, 1, 2, 3, 4],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [ ((0, 'a'), 1), ((0, 'b'), 2),
                                         ((1, 'a'), 1), ((1, 'b'), 3),
                                         ((2, 'a'), 1), ((2, 'b'), 2),
                                         ((3, 'a'), 1), ((3, 'b'), 4),
                                         ((4, 'a'), 1), ((4, 'b'), 2) ],
            initialState = 0,
            acceptingStates = [4]
          }
          expected = Automaton {
            states = [0,1,3,4],
            alphabet = "ab",
            transitions = Map.fromList [((0,'a'),1),((0,'b'),0),
                                        ((1,'a'),1),((1,'b'),3),
                                        ((3,'a'),1),((3,'b'),4),
                                        ((4,'a'),1),((4,'b'),0) ],
            initialState = 0,
            acceptingStates = [4]
          }
      minimizeAutomaton dfa `shouldBe` expected

    it "minimizes a DFA with unreachable states" $ do
      let dfa = Automaton {
            states = [0, 1, 2],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((1, 'b'), 0)],  -- No transition to state 2
            initialState = 0,
            acceptingStates = [1]
          }
          expected = Automaton {
            states = [0, 1],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [((0, 'a'), 1), ((1, 'b'), 0)],
            initialState = 0,
            acceptingStates = [1]
          }
      minimizeAutomaton dfa `shouldBe` expected
