-- test/RemoveLongRulesSpec.hs
module RemoveLongRulesSpec where

import Test.Hspec
import Grammar (Grammar(..), Symbol(..))
import RemoveLongRules (removeLongRules)

spec :: Spec
spec = do
  describe "RemoveLongRules" $ do
    it "removes long rules from a grammar with no long rules" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let newGrammar = removeLongRules grammar
        newGrammar `shouldBe` grammar

    it "removes a single long rule from the grammar" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a', NonTerminal "B", Terminal 'c', NonTerminal "B"]),
                (NonTerminal "B", [Terminal 'd', Terminal 'e']) -- Короткое правило
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a', NonTerminal "AB1"]),
                (NonTerminal "AB1", [NonTerminal "B", NonTerminal "AB2"]),
                (NonTerminal "AB2", [Terminal 'c', NonTerminal "B"]),
                (NonTerminal "B", [Terminal 'd', Terminal 'e'])
              ]
        let newGrammar = removeLongRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "removes multiple long rules from the grammar" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a', NonTerminal "B", Terminal 'c', NonTerminal "B"]),
                (NonTerminal "B", [Terminal 'd', Terminal 'e', Terminal 'f'])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a', NonTerminal "AB1"]),
                (NonTerminal "AB1", [NonTerminal "B", NonTerminal "AB2"]),
                (NonTerminal "AB2", [Terminal 'c', NonTerminal "B"]),
                (NonTerminal "B", [Terminal 'd', NonTerminal "BB1"]),
                (NonTerminal "BB1", [Terminal 'e', Terminal 'f'])
              ]
        let newGrammar = removeLongRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "handles rules with exactly two symbols correctly" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a', NonTerminal "C"]),
                (NonTerminal "B", [Terminal 'b', Terminal 'c']),
                (NonTerminal "C", [Terminal 'c'])
              ]
        let newGrammar = removeLongRules grammar
        newGrammar `shouldBe` grammar

    it "handles rules with one symbol correctly" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b', Terminal 'c'])
              ]
        let newGrammar = removeLongRules grammar
        newGrammar `shouldBe` grammar
