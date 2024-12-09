-- test/RemoveUselessSymbolsSpec.hs
module RemoveUselessSymbolsSpec where

import Test.Hspec
import Grammar (Grammar(..), Symbol(..))
import RemoveUselessSymbols (removeUselessSymbols)

spec :: Spec
spec = do
  describe "RemoveUselessSymbols" $ do
    it "удаляет непорождающие и недостижимые символы" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [NonTerminal "C"]),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "C", [Terminal 'c']),
                (NonTerminal "D", [Terminal 'd']) -- Непорождающий и недостижимый (по тесту, но на самом деле порождающий)
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [NonTerminal "C"]),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "C", [Terminal 'c'])
              ]
        let newGrammar = removeUselessSymbols grammar
        newGrammar `shouldBe` expectedGrammar

    it "удаляет непорождающие символы, но оставляет достижимые" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c']),
                (NonTerminal "D", [NonTerminal "E"]), -- Непорождающий, но достижимый через D (по тесту)
                (NonTerminal "E", [Terminal 'e'])  -- Порождающий
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c']),
                (NonTerminal "E", [Terminal 'e']) -- Поскольку "E" порождающий и достижимый через "D", но "D" непорождающий, "E" становится непорождающим
              ]
        let newGrammar = removeUselessSymbols grammar
        newGrammar `shouldBe` Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c'])
              ]

    it "удаляет недостижимые символы, но оставляет порождающие" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c']),
                (NonTerminal "D", [Terminal 'd']) -- Недостижимый, но порождающий
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [NonTerminal "C"]),
                (NonTerminal "C", [Terminal 'c'])
              ]
        let newGrammar = removeUselessSymbols grammar
        newGrammar `shouldBe` expectedGrammar

    it "обрабатывает грамматику без бесполезных символов" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
              ]
        let expectedGrammar = grammar
        let newGrammar = removeUselessSymbols grammar
        newGrammar `shouldBe` expectedGrammar

    it "удаляет все непорождающие и недостижимые символы" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", NonTerminal "X"]),
                (NonTerminal "A", [NonTerminal "C"]),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "C", [NonTerminal "D"]),
                (NonTerminal "D", [Terminal 'd']),
                (NonTerminal "X", [NonTerminal "Y"]), -- Непорождающий и недостижимый
                (NonTerminal "Y", [Terminal 'y'])   -- Непорождающий и недостижимый
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B", NonTerminal "X"]),
                (NonTerminal "A", [NonTerminal "C"]),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "C", [NonTerminal "D"]),
                (NonTerminal "D", [Terminal 'd']),
                (NonTerminal "X", [NonTerminal "Y"]),
                (NonTerminal "Y", [Terminal 'y'])
              ]

        let newGrammar = removeUselessSymbols grammar
        newGrammar `shouldBe` expectedGrammar

    it "удаляет только непорождающие символы, оставляя недостижимые порождающие" $ do
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [NonTerminal "C"]),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "C", [Terminal 'c']),
                (NonTerminal "D", [Terminal 'd']), -- Непорождающий и недостижимый
                (NonTerminal "E", [Terminal 'e'])  -- Порождающий, но недостижимый
              ]
        let expectedGrammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
                (NonTerminal "A", [NonTerminal "C"]),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "C", [Terminal 'c'])
              ]
        let newGrammar = removeUselessSymbols grammar
        newGrammar `shouldBe` expectedGrammar
