-- test/RemoveEpsilonRulesSpec.hs
module RemoveEpsilonRulesSpec where

import Test.Hspec
import Grammar (Grammar(..), Symbol(..))
import RemoveEpsilonRules (removeEpsilonRules)

spec :: Spec
spec = do
  describe "RemoveEpsilonRules" $ do
    it "removes epsilon-правила из грамматики без epsilon-правил" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let newGrammar = removeEpsilonRules grammar
        newGrammar `shouldBe` grammar

    it "удаляет одно epsilon-правило из грамматики" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "A", [Epsilon]),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "S", [NonTerminal "B"]), -- Новое правило, удаляющее nullable A
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let newGrammar = removeEpsilonRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "удаляет несколько epsilon-правил из грамматики и не добавляет новое стартовое правило, если S не epsilon-порождающий" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", NonTerminal "C", Terminal 'd']),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "A", [Epsilon]),
                (NonTerminal "B", [NonTerminal "A", NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c']),
                (NonTerminal "C", [Epsilon])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", NonTerminal "C", Terminal 'd']),
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", Terminal 'd']),
                (NonTerminal "S", [NonTerminal "A", NonTerminal "C", Terminal 'd']),
                (NonTerminal "S", [NonTerminal "A", Terminal 'd']),
                (NonTerminal "S", [NonTerminal "B", NonTerminal "C", Terminal 'd']),
                (NonTerminal "S", [NonTerminal "B", Terminal 'd']),
                (NonTerminal "S", [NonTerminal "C", Terminal 'd']),
                (NonTerminal "S", [Terminal 'd']),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [NonTerminal "A", NonTerminal "C"]),
                (NonTerminal "B", [NonTerminal "A"]),
                (NonTerminal "B", [NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c'])
              ]
        let newGrammar = removeEpsilonRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "обрабатывает правило с множеством epsilon-порождающих нетерминалов" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", Terminal 'c']),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "A", [Epsilon]),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "B", [Epsilon])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", Terminal 'c']),
                (NonTerminal "S", [NonTerminal "A", Terminal 'c']),
                (NonTerminal "S", [NonTerminal "B", Terminal 'c']),
                (NonTerminal "S", [Terminal 'c']),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let newGrammar = removeEpsilonRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "обрабатывает грамматику, допускающую только epsilon" $ do
        let grammar = Grammar [
                (NonTerminal "S", [Epsilon])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [Epsilon])
              ] -- В этом случае новое стартовое правило не добавляется
        let newGrammar = removeEpsilonRules grammar
        newGrammar `shouldBe` expectedGrammar

--    it "добавляет новое стартовое правило, если исходная грамматика допускает epsilon" $ do
--        let grammar = Grammar [
--                (NonTerminal "S", [NonTerminal "A"]),
--                (NonTerminal "A", [Epsilon])
--              ]
--        let expectedGrammar = Grammar [
--                (NonTerminal "S", [NonTerminal "A"]),
--                (NonTerminal "A", [Epsilon]),
--                (NonTerminal "S", [Epsilon]) -- Добавление S → ε, так как S может выводить ε через A
--              ]
--        let newGrammar = removeEpsilonRules grammar
--        newGrammar `shouldBe` expectedGrammar
