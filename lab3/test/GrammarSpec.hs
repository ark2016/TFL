-- test/GrammarSpec.hs
module GrammarSpec where

import Test.Hspec
import Grammar (Grammar(..), Symbol(..), parseSymbol, parseRule, parseGrammar, printGrammar)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Grammar" $ do
    describe "parseSymbol" $ do
      it "parses a terminal symbol" $ do
        parseSymbol "a" `shouldBe` Just (Terminal 'a')

      it "parses a non-terminal symbol" $ do
        parseSymbol "Abc" `shouldBe` Just (NonTerminal "Abc")

      it "parses epsilon" $ do
        parseSymbol "ε" `shouldBe` Just Epsilon

      it "fails to parse an invalid symbol" $ do
        parseSymbol "123" `shouldBe` Nothing
        parseSymbol "ab" `shouldBe` Nothing
        parseSymbol "" `shouldBe` Nothing

    describe "parseRule" $ do
      it "parses a simple rule" $ do
        parseRule "S -> a" `shouldBe` Just (NonTerminal "S", [Terminal 'a'])

      it "parses a rule with multiple symbols" $ do
        parseRule "S -> A B" `shouldBe` Just (NonTerminal "S", [NonTerminal "A", NonTerminal "B"])

      it "parses a rule with epsilon" $ do
        parseRule "A -> ε" `shouldBe` Just (NonTerminal "A", [Epsilon])

      it "parses a complex rule" $ do
        parseRule "Expr -> Term AddOp Term" `shouldBe` Just (NonTerminal "Expr", [NonTerminal "Term", NonTerminal "AddOp", NonTerminal "Term"])

      it "fails to parse an invalid rule" $ do
        parseRule "S a" `shouldBe` Nothing
        parseRule "S ->" `shouldBe` Nothing
        parseRule "-> a" `shouldBe` Nothing
        parseRule "" `shouldBe` Nothing

    describe "parseGrammar" $ do
      it "parses a simple grammar" $ do
        parseGrammar "S -> a\nA -> b" `shouldBe` Just (Grammar [(NonTerminal "S", [Terminal 'a']), (NonTerminal "A", [Terminal 'b'])])

      it "parses a grammar with multiple rules" $ do
        parseGrammar "S -> A B\nA -> a\nB -> b" `shouldBe` Just (Grammar [(NonTerminal "S", [NonTerminal "A", NonTerminal "B"]), (NonTerminal "A", [Terminal 'a']), (NonTerminal "B", [Terminal 'b'])])

      it "parses a grammar with epsilon rules" $ do
        parseGrammar "S -> A\nA -> ε" `shouldBe` Just (Grammar [(NonTerminal "S", [NonTerminal "A"]), (NonTerminal "A", [Epsilon])])

      it "fails to parse an invalid grammar" $ do
        parseGrammar "S -> a\nA b" `shouldBe` Nothing
        parseGrammar "S -> a\n->" `shouldBe` Nothing

    describe "printGrammar" $ do
      it "prints a simple grammar" $ do
        printGrammar (Grammar [(NonTerminal "S", [Terminal 'a']), (NonTerminal "A", [Terminal 'b'])]) `shouldBe` "S -> a\nA -> b"

      it "prints a grammar with multiple rules" $ do
        printGrammar (Grammar [(NonTerminal "S", [NonTerminal "A", NonTerminal "B"]), (NonTerminal "A", [Terminal 'a']), (NonTerminal "B", [Terminal 'b'])]) `shouldBe` "S -> A B\nA -> a\nB -> b"

      it "prints a grammar with epsilon rules" $ do
        printGrammar (Grammar [(NonTerminal "S", [NonTerminal "A"]), (NonTerminal "A", [Epsilon])]) `shouldBe` "S -> A\nA -> ε"

    describe "Eq instance for Grammar" $ do
      it "correctly compares equal grammars" $ do
        let grammar1 = Grammar [(NonTerminal "S", [Terminal 'a']), (NonTerminal "A", [Terminal 'b'])]
        let grammar2 = Grammar [(NonTerminal "S", [Terminal 'a']), (NonTerminal "A", [Terminal 'b'])]
        grammar1 `shouldBe` grammar2

      it "correctly compares unequal grammars (different rules)" $ do
        let grammar1 = Grammar [(NonTerminal "S", [Terminal 'a']), (NonTerminal "A", [Terminal 'b'])]
        let grammar2 = Grammar [(NonTerminal "S", [Terminal 'b']), (NonTerminal "A", [Terminal 'a'])]
        grammar1 `shouldNotBe` grammar2

      it "correctly compares unequal grammars (different number of rules)" $ do
        let grammar1 = Grammar [(NonTerminal "S", [Terminal 'a']), (NonTerminal "A", [Terminal 'b'])]
        let grammar2 = Grammar [(NonTerminal "S", [Terminal 'a'])]
        grammar1 `shouldNotBe` grammar2