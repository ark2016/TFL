-- test/GrammarSpec.hs
{-# LANGUAGE OverloadedStrings #-}

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
        parseSymbol "A1" `shouldBe` Just (NonTerminal "A1")
        parseSymbol "ABC123" `shouldBe` Just (NonTerminal "ABC123")

      it "parses epsilon" $ do
        parseSymbol "ε" `shouldBe` Just Epsilon

      it "fails to parse an invalid symbol" $ do
        parseSymbol "123" `shouldBe` Nothing
        parseSymbol "ab" `shouldBe` Nothing
        parseSymbol "" `shouldBe` Nothing
        parseSymbol "A_b" `shouldBe` Nothing -- Символ '_' теперь запрещён
        parseSymbol "A-B" `shouldBe` Nothing -- Неправильный символ '-'
        parseSymbol "_A" `shouldBe` Nothing -- Начало с '_'
        parseSymbol "1A" `shouldBe` Nothing -- Начало с цифры
        parseSymbol "aB1" `shouldBe` Nothing -- Начало с маленькой буквы

    describe "parseRule" $ do
      it "parses a simple rule with spaces" $ do
        parseRule "S -> a" `shouldBe` Just (NonTerminal "S", [Terminal 'a'])

      it "parses a simple rule without spaces" $ do
        parseRule "S->a" `shouldBe` Just (NonTerminal "S", [Terminal 'a'])

      it "parses a rule with multiple symbols and spaces" $ do
        parseRule "S -> A B" `shouldBe` Just (NonTerminal "S", [NonTerminal "A", NonTerminal "B"])

      it "parses a rule with multiple symbols without spaces" $ do
        parseRule "S->AB" `shouldBe` Just (NonTerminal "S", [NonTerminal "AB"])

      it "parses a rule with epsilon and spaces" $ do
        parseRule "A -> ε" `shouldBe` Just (NonTerminal "A", [Epsilon])

      it "parses a rule with epsilon without spaces" $ do
        parseRule "A->ε" `shouldBe` Just (NonTerminal "A", [Epsilon])

      it "parses a complex rule with spaces" $ do
        parseRule "Expr -> Term AddOp Term" `shouldBe` Just (NonTerminal "Expr", [NonTerminal "Term", NonTerminal "AddOp", NonTerminal "Term"])

      it "parses a complex rule without spaces" $ do
        parseRule "Expr->TermAddOpTerm" `shouldBe` Just (NonTerminal "Expr", [NonTerminal "TermAddOpTerm"])

      it "fails to parse an invalid rule with missing '->'" $ do
        parseRule "S a" `shouldBe` Nothing

      it "fails to parse a rule with missing RHS" $ do
        parseRule "S ->" `shouldBe` Nothing

      it "fails to parse a rule with missing LHS" $ do
        parseRule "-> a" `shouldBe` Nothing

      it "fails to parse an empty rule" $ do
        parseRule "" `shouldBe` Nothing

      it "fails to parse a rule with multiple LHS symbols" $ do
        parseRule "S A -> a" `shouldBe` Nothing

      it "fails to parse a rule where LHS starts with a digit" $ do
        parseRule "1S -> a" `shouldBe` Nothing

      it "fails to parse a rule where LHS starts with a lowercase letter" $ do
        parseRule "s -> a" `shouldBe` Nothing

      it "fails to parse a rule where LHS contains '_' " $ do
        parseRule "A_B -> a" `shouldBe` Nothing

    describe "parseGrammar" $ do
      it "parses a simple grammar with spaces" $ do
        parseGrammar "S -> a\nA -> b" `shouldBe` Just (Grammar [(NonTerminal "S", [Terminal 'a']), (NonTerminal "A", [Terminal 'b'])])

      it "parses a simple grammar without spaces" $ do
        parseGrammar "S->a\nA->b" `shouldBe` Just (Grammar [(NonTerminal "S", [Terminal 'a']), (NonTerminal "A", [Terminal 'b'])])

      it "parses a grammar with multiple rules and spaces" $ do
        parseGrammar "S -> A B\nA -> a\nB -> b" `shouldBe` Just (Grammar [(NonTerminal "S", [NonTerminal "A", NonTerminal "B"]), (NonTerminal "A", [Terminal 'a']), (NonTerminal "B", [Terminal 'b'])])

      it "parses a grammar with multiple rules without spaces" $ do
        parseGrammar "S->AB\nA->a\nB->b" `shouldBe` Just (Grammar [(NonTerminal "S", [NonTerminal "AB"]), (NonTerminal "A", [Terminal 'a']), (NonTerminal "B", [Terminal 'b'])])

      it "parses a grammar with epsilon rules and spaces" $ do
        parseGrammar "S -> A\nA -> ε" `shouldBe` Just (Grammar [(NonTerminal "S", [NonTerminal "A"]), (NonTerminal "A", [Epsilon])])

      it "parses a grammar with epsilon rules without spaces" $ do
        parseGrammar "S->A\nA->ε" `shouldBe` Just (Grammar [(NonTerminal "S", [NonTerminal "A"]), (NonTerminal "A", [Epsilon])])

      it "parses a complex grammar with mixed spaces" $ do
        parseGrammar "Expr -> Term AddOp Term\nTerm->Factor\nFactor -> a" `shouldBe` Just (Grammar
          [ (NonTerminal "Expr", [NonTerminal "Term", NonTerminal "AddOp", NonTerminal "Term"])
          , (NonTerminal "Term", [NonTerminal "Factor"])
          , (NonTerminal "Factor", [Terminal 'a'])
          ])

      it "fails to parse a grammar with invalid rules" $ do
        parseGrammar "S -> a\nA b" `shouldBe` Nothing
        parseGrammar "S -> a\n->" `shouldBe` Nothing
        parseGrammar "S -> a\nA->" `shouldBe` Nothing
        parseGrammar "S a\nA->b" `shouldBe` Nothing
        parseGrammar "-> a\nA->b" `shouldBe` Nothing

      it "fails to parse a grammar with non-terminal starting with digit" $ do
        parseGrammar "S -> a\n1A -> b" `shouldBe` Nothing

      it "fails to parse a grammar with non-terminal starting with lowercase letter" $ do
        parseGrammar "S -> a\nsA -> b" `shouldBe` Nothing

      it "fails to parse a grammar with non-terminal containing '_'" $ do
        parseGrammar "S -> a\nA_B -> b" `shouldBe` Nothing

    describe "printGrammar" $ do
      it "prints a simple grammar" $ do
        printGrammar (Grammar [(NonTerminal "S", [Terminal 'a']), (NonTerminal "A", [Terminal 'b'])]) `shouldBe` "S -> a\nA -> b"

      it "prints a grammar with multiple rules" $ do
        printGrammar (Grammar
          [ (NonTerminal "S", [NonTerminal "A", NonTerminal "B"])
          , (NonTerminal "A", [Terminal 'a'])
          , (NonTerminal "B", [Terminal 'b'])
          ]) `shouldBe` "S -> A B\nA -> a\nB -> b"

      it "prints a grammar with epsilon rules" $ do
        printGrammar (Grammar [(NonTerminal "S", [NonTerminal "A"]), (NonTerminal "A", [Epsilon])]) `shouldBe` "S -> A\nA -> ε"

      it "prints a complex grammar correctly" $ do
        printGrammar (Grammar
          [ (NonTerminal "Expr", [NonTerminal "Term", NonTerminal "AddOp", NonTerminal "Term"])
          , (NonTerminal "Term", [NonTerminal "Factor"])
          , (NonTerminal "Factor", [Terminal 'a'])
          ]) `shouldBe` "Expr -> Term AddOp Term\nTerm -> Factor\nFactor -> a"

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
