-- test/RemoveChainRulesSpec.hs
module RemoveChainRulesSpec where

import Test.Hspec
import Grammar (Grammar(..), Symbol(..))
import RemoveChainRules (removeChainRules)

spec :: Spec
spec = do
  describe "RemoveChainRules" $ do
    it "удаляет цепные правила и добавляет соответствующие нецепные правила" $ do
        let grammar = Grammar [
                (NonTerminal "A", [NonTerminal "B"]),
                (NonTerminal "B", [NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c']),
                (NonTerminal "C", [Terminal 'd']),
                (NonTerminal "D", [Terminal 'e'])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "C", [Terminal 'c']),
                (NonTerminal "C", [Terminal 'd']),
                (NonTerminal "D", [Terminal 'e']),
                (NonTerminal "A", [Terminal 'd']),
                (NonTerminal "A", [Terminal 'c']),
                (NonTerminal "B", [Terminal 'd']),
                (NonTerminal "B", [Terminal 'c'])
              ]
        let newGrammar = removeChainRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "корректно обрабатывает грамматику без цепных правил" $ do
        let grammar = Grammar [
                (NonTerminal "S", [Terminal 'a']),
                (NonTerminal "A", [Terminal 'b']),
                (NonTerminal "B", [Terminal 'c'])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [Terminal 'a']),
                (NonTerminal "A", [Terminal 'b']),
                (NonTerminal "B", [Terminal 'c'])
              ]
        let newGrammar = removeChainRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "удаляет все цепные правила и добавляет соответствующие нецепные правила" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A"]),
                (NonTerminal "A", [NonTerminal "B"]),
                (NonTerminal "B", [NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c'])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "C", [Terminal 'c']),
                (NonTerminal "A", [Terminal 'c']),
                (NonTerminal "B", [Terminal 'c']),
                (NonTerminal "S", [Terminal 'c'])
              ]
        let newGrammar = removeChainRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "не добавляет дублирующиеся правила" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A"]),
                (NonTerminal "A", [NonTerminal "B"]),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "A", [Terminal 'b']),
                (NonTerminal "S", [Terminal 'b'])
              ]
        let newGrammar = removeChainRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "обрабатывает цепные правила с циклическими зависимостями" $ do
        let grammar = Grammar [
                (NonTerminal "A", [NonTerminal "B"]),
                (NonTerminal "B", [NonTerminal "A"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "A", [Terminal 'b']),
                (NonTerminal "B", [Terminal 'a'])
              ]
        let newGrammar = removeChainRules grammar
        newGrammar `shouldBe` expectedGrammar
