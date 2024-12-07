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
                (NonTerminal "A", [Terminal 'a', Epsilon]),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b']) -- Новое стартовое правило не добавляется, так как S не epsilon-порождающий
              ]
        let newGrammar = removeEpsilonRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "удаляет несколько epsilon-правил из грамматики и не добавляет новое стартовое правило, если S не epsilon-порождающий" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", NonTerminal "C", Terminal 'd']),
                (NonTerminal "A", [Terminal 'a', Epsilon]),
                (NonTerminal "B", [NonTerminal "A", NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c', Epsilon])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", NonTerminal "C", Terminal 'd']),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [NonTerminal "A", NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c']),
                -- Генерация новых правил без epsilon-порождающих нетерминалов
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", Terminal 'd']),
                (NonTerminal "S", [NonTerminal "B", NonTerminal "C", Terminal 'd']),
                (NonTerminal "S", [NonTerminal "A", Terminal 'd']),
                (NonTerminal "S", [NonTerminal "B", Terminal 'd']),
                (NonTerminal "S", [NonTerminal "C", Terminal 'd']),
                (NonTerminal "S", [Terminal 'd'])
              ]
        let newGrammar = removeEpsilonRules grammar
        newGrammar `shouldBe` expectedGrammar

    it "обрабатывает правило с множеством epsilon-порождающих нетерминалов" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", Terminal 'c']),
                (NonTerminal "A", [Terminal 'a', Epsilon]),
                (NonTerminal "B", [Terminal 'b', Epsilon])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", Terminal 'c']),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b']),
                -- Генерация новых правил без epsilon-порождающих нетерминалов
                (NonTerminal "S", [NonTerminal "A", Terminal 'c']),
                (NonTerminal "S", [NonTerminal "B", Terminal 'c']),
                (NonTerminal "S", [Terminal 'c'])
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

    it "добавляет новое стартовое правило, если исходная грамматика допускает epsilon" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A"]),
                (NonTerminal "A", [Epsilon])
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A"]),
                (NonTerminal "A", [Epsilon]),
                -- Добавление нового стартового правила, так как S может выводить epsilon через A
                (NonTerminal "S'", [NonTerminal "S"]),
                (NonTerminal "S'", [Epsilon])
              ]
        let newGrammar = removeEpsilonRules grammar
        newGrammar `shouldBe` expectedGrammar
