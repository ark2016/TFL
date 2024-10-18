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
            states = [0, 1, 3, 4],
            alphabet = ['a', 'b'],
            transitions = Map.fromList [ ((0, 'a'), 1), ((0, 'b'), 0),
                                         ((1, 'a'), 1), ((1, 'b'), 3),
                                         ((3, 'a'), 1), ((3, 'b'), 4),
                                         ((4, 'a'), 1), ((4, 'b'), 0) ],
            initialState = 0,
            acceptingStates = [4]
          }
      minimizeAutomaton dfa `shouldBe` expected

--    it "minimizes a DFA with unreachable states" $ do
--      let dfa = Automaton {
--            states = [0, 1, 2],
--            alphabet = ['a', 'b'],
--            transitions = Map.fromList [((0, 'a'), 1), ((1, 'b'), 0)],  -- No transition to state 2
--            initialState = 0,
--            acceptingStates = [1]
--          }
--          expected = Automaton {
--            states = [0, 1],
--            alphabet = ['a', 'b'],
--            transitions = Map.fromList [((0, 'a'), 1), ((1, 'b'), 0)],
--            initialState = 0,
--            acceptingStates = [1]
--          }
--      minimizeAutomaton dfa `shouldBe` expected
--
--    it "minimizes a DFA with equivalent states" $ do
--      let dfa = Automaton {
--            states = [0, 1, 2, 3],
--            alphabet = ['a', 'b'],
--            transitions = Map.fromList [ ((0, 'a'), 1), ((0, 'b'), 2),
--                                         ((1, 'a'), 3), ((1, 'b'), 3),
--                                         ((2, 'a'), 3), ((2, 'b'), 3),
--                                         ((3, 'a'), 3), ((3, 'b'), 3) ],
--            initialState = 0,
--            acceptingStates = [3]
--          }
--          expected = Automaton {
--            states = [0, 1, 2],
--            alphabet = ['a', 'b'],
--            transitions = Map.fromList [ ((0, 'a'), 1), ((0, 'b'), 2),
--                                         ((1, 'a'), 2), ((1, 'b'), 2),
--                                         ((2, 'a'), 2), ((2, 'b'), 2) ],
--            initialState = 0,
--            acceptingStates = [2]
--          }
--      minimizeAutomaton dfa `shouldBe` expected
--
--    it "minimizes a DFA where no states are equivalent" $ do
--      let dfa = Automaton {
--            states = [0, 1, 2],
--            alphabet = ['a', 'b'],
--            transitions = Map.fromList [ ((0, 'a'), 1), ((0, 'b'), 2),
--                                         ((1, 'a'), 1), ((1, 'b'), 2),
--                                         ((2, 'a'), 1), ((2, 'b'), 2) ],
--            initialState = 0,
--            acceptingStates = [1]
--          }
--          expected = dfa  -- The DFA is already minimal
--      minimizeAutomaton dfa `shouldBe` expected
--
--    it "minimizes a complex DFA with multiple equivalent states" $ do
--      let dfa = Automaton {
--            states = [0, 1, 2, 3, 4, 5],
--            alphabet = ['a', 'b'],
--            transitions = Map.fromList [ ((0, 'a'), 1), ((0, 'b'), 2),
--                                         ((1, 'a'), 3), ((1, 'b'), 4),
--                                         ((2, 'a'), 3), ((2, 'b'), 4),
--                                         ((3, 'a'), 5), ((3, 'b'), 5),
--                                         ((4, 'a'), 5), ((4, 'b'), 5),
--                                         ((5, 'a'), 5), ((5, 'b'), 5) ],
--            initialState = 0,
--            acceptingStates = [5]
--          }
--          expected = Automaton {
--            states = [0, 1, 2, 3],
--            alphabet = ['a', 'b'],
--            transitions = Map.fromList [ ((0, 'a'), 1), ((0, 'b'), 2),
--                                         ((1, 'a'), 3), ((1, 'b'), 3),
--                                         ((2, 'a'), 3), ((2, 'b'), 3),
--                                         ((3, 'a'), 3), ((3, 'b'), 3) ],
--            initialState = 0,
--            acceptingStates = [3]
--          }
--      minimizeAutomaton dfa `shouldBe` expected