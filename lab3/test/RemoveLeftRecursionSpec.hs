-- test/RemoveLeftRecursionSpec.hs
module RemoveLeftRecursionSpec where

import Test.Hspec
import Grammar (Grammar(..), Symbol(..))
import RemoveLeftRecursion (removeLeftRecursion)

spec :: Spec
spec = do
  describe "RemoveLeftRecursion" $ do

    it "удаляет непосредственную левую рекурсию для одного нетерминала" $ do
      let grammar = Grammar [
              (NonTerminal "S", [NonTerminal "S", Terminal 'a']),
              (NonTerminal "S", [Terminal 'b'])
            ]
      let expectedGrammar = Grammar [
              (NonTerminal "S", [Terminal 'b', NonTerminal "S'"]),
              (NonTerminal "S'", [Terminal 'a', NonTerminal "S'"]),
              (NonTerminal "S'", [Terminal 'a'])
            ]
      let newGrammar = removeLeftRecursion grammar
      newGrammar `shouldBe` expectedGrammar

--    it "удаляет непосредственную левую рекурсию для нескольких нетерминалов" $ do
--      let grammar = Grammar [
--              (NonTerminal "A", [NonTerminal "A", Terminal 'a']),
--              (NonTerminal "A", [Terminal 'b']),
--              (NonTerminal "B", [NonTerminal "B", Terminal 'c']),
--              (NonTerminal "B", [Terminal 'd'])
--            ]
--      let expectedGrammar = Grammar [
--              (NonTerminal "A", [Terminal 'b', NonTerminal "A'"]),
--              (NonTerminal "A'", [Terminal 'a', NonTerminal "A'"]),
--              (NonTerminal "A'", [Terminal 'a']),
--              (NonTerminal "B", [Terminal 'd', NonTerminal "B'"]),
--              (NonTerminal "B'", [Terminal 'c', NonTerminal "B'"]),
--              (NonTerminal "B'", [Terminal 'c'])
--            ]
--      let newGrammar = removeLeftRecursion grammar
--      newGrammar `shouldBe` expectedGrammar

--    it "удаляет косвенную левую рекурсию" $ do
--      let grammar = Grammar [
--              (NonTerminal "S", [NonTerminal "A", Terminal 'a']),
--              (NonTerminal "A", [NonTerminal "S", Terminal 'b']),
--              (NonTerminal "A", [Terminal 'c'])
--            ]
--      let expectedGrammar = Grammar [
--              (NonTerminal "S", [NonTerminal "A", Terminal 'a']),
--              (NonTerminal "A", [Terminal 'c', NonTerminal "A'"]),
--              (NonTerminal "A'", [Terminal 'b', NonTerminal "A'"]),
--              (NonTerminal "A'", [Terminal 'b'])
--            ]
--      let newGrammar = removeLeftRecursion grammar
--      newGrammar `shouldBe` expectedGrammar

    it "не изменяет грамматику без левой рекурсии" $ do
      let grammar = Grammar [
              (NonTerminal "S", [Terminal 'a']),
              (NonTerminal "A", [NonTerminal "B", Terminal 'b']),
              (NonTerminal "B", [Terminal 'c'])
            ]
      let expectedGrammar = Grammar [
              (NonTerminal "S", [Terminal 'a']),
              (NonTerminal "A", [NonTerminal "B", Terminal 'b']),
              (NonTerminal "B", [Terminal 'c'])
            ]
      let newGrammar = removeLeftRecursion grammar
      newGrammar `shouldBe` expectedGrammar

--    it "удаляет левую рекурсию в грамматике с несколькими леворекурсивными нетерминалами" $ do
--      let grammar = Grammar [
--              (NonTerminal "S", [NonTerminal "S", Terminal 'a']),
--              (NonTerminal "S", [NonTerminal "A", Terminal 'b']),
--              (NonTerminal "A", [NonTerminal "A", Terminal 'c']),
--              (NonTerminal "A", [Terminal 'd'])
--            ]
--      let expectedGrammar = Grammar [
--              (NonTerminal "S", [NonTerminal "A", Terminal 'b', NonTerminal "S'"]),
--              (NonTerminal "S'", [Terminal 'a', NonTerminal "S'"]),
--              (NonTerminal "S'", [Terminal 'a']),
--              (NonTerminal "A", [Terminal 'd', NonTerminal "A'"]),
--              (NonTerminal "A'", [Terminal 'c', NonTerminal "A'"]),
--              (NonTerminal "A'", [Terminal 'c'])
--            ]
--      let newGrammar = removeLeftRecursion grammar
--      newGrammar `shouldBe` expectedGrammar

--    it "удаляет левую рекурсию при наличии циклических зависимостей" $ do
--      let grammar = Grammar [
--              (NonTerminal "A", [NonTerminal "B", Terminal 'a']),
--              (NonTerminal "B", [NonTerminal "C", Terminal 'b']),
--              (NonTerminal "C", [NonTerminal "A", Terminal 'c']),
--              (NonTerminal "C", [Terminal 'd'])
--            ]
--      let expectedGrammar = Grammar [
--              (NonTerminal "A", [NonTerminal "B", Terminal 'a']),
--              (NonTerminal "B", [NonTerminal "C", Terminal 'b']),
--              (NonTerminal "C", [Terminal 'd', NonTerminal "C'"]),
--              (NonTerminal "C'", [Terminal 'c', NonTerminal "C'"]),
--              (NonTerminal "C'", [Terminal 'c'])
--            ]
--      let newGrammar = removeLeftRecursion grammar
--      newGrammar `shouldBe` expectedGrammar

--    it "обрабатывает грамматику с единственным нетерминалом, производящим только себя" $ do
--      let grammar = Grammar [
--              (NonTerminal "S", [NonTerminal "S"])
--            ]
--      let expectedGrammar = Grammar [
--              (NonTerminal "S", [NonTerminal "S'"]),
--              (NonTerminal "S'", [NonTerminal "S'"])
--            ]
--      let newGrammar = removeLeftRecursion grammar
--      newGrammar `shouldBe` expectedGrammar

    it "удаляет левую рекурсию и добавляет новые нетерминалы без дубликатов" $ do
      let grammar = Grammar [
              (NonTerminal "S", [NonTerminal "S", Terminal 'a']),
              (NonTerminal "S", [NonTerminal "S", Terminal 'b']),
              (NonTerminal "S", [Terminal 'c'])
            ]
      let expectedGrammar = Grammar [
              (NonTerminal "S", [Terminal 'c', NonTerminal "S'"]),
              (NonTerminal "S'", [Terminal 'a', NonTerminal "S'"]),
              (NonTerminal "S'", [Terminal 'b', NonTerminal "S'"]),
              (NonTerminal "S'", [Terminal 'a']),
              (NonTerminal "S'", [Terminal 'b'])
            ]
      let newGrammar = removeLeftRecursion grammar
      newGrammar `shouldBe` expectedGrammar

--    it "корректно обрабатывает грамматику с несколькими цепочками вывода" $ do
--      let grammar = Grammar [
--              (NonTerminal "S", [NonTerminal "A", Terminal 'a']),
--              (NonTerminal "A", [NonTerminal "B", Terminal 'b']),
--              (NonTerminal "B", [NonTerminal "C", Terminal 'c']),
--              (NonTerminal "C", [NonTerminal "S", Terminal 'd']),
--              (NonTerminal "C", [Terminal 'e'])
--            ]
--      let expectedGrammar = Grammar [
--              (NonTerminal "S", [NonTerminal "A", Terminal 'a']),
--              (NonTerminal "A", [NonTerminal "B", Terminal 'b']),
--              (NonTerminal "B", [NonTerminal "C", Terminal 'c']),
--              (NonTerminal "C", [Terminal 'e', NonTerminal "C'"]),
--              (NonTerminal "C'", [Terminal 'd', NonTerminal "C'"]),
--              (NonTerminal "C'", [Terminal 'd'])
--            ]
--      let newGrammar = removeLeftRecursion grammar
--      newGrammar `shouldBe` expectedGrammar

    it "обрабатывает грамматику с правой рекурсией и не изменяет её" $ do
      let grammar = Grammar [
              (NonTerminal "S", [Terminal 'a', NonTerminal "S"]),
              (NonTerminal "S", [Terminal 'b'])
            ]
      let expectedGrammar = Grammar [
              (NonTerminal "S", [Terminal 'a', NonTerminal "S"]),
              (NonTerminal "S", [Terminal 'b'])
            ]
      let newGrammar = removeLeftRecursion grammar
      newGrammar `shouldBe` expectedGrammar
