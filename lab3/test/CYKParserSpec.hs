{-# LANGUAGE OverloadedStrings #-}

module CYKParserSpec where

import Test.Hspec
import CYKParser
import Grammar

import Control.Monad (forM_)
import System.Exit (ExitCode(..))

-- | Функция для запуска тестов
spec :: Spec
spec = describe "CYKParser.cykParse" $ do

    it "распознает пустую строку, если грамматика генерирует epsilon" $ do
        -- Грамматика: S -> ε
        let grammar = Grammar [
                (NonTerminal "S", [Epsilon])
              ]
        cykParse grammar "" `shouldBe` True

    it "не распознает пустую строку, если грамматика не генерирует epsilon" $ do
        -- Грамматика: S -> a
        let grammar = Grammar [
                (NonTerminal "S", [Terminal 'a'])
              ]
        cykParse grammar "" `shouldBe` False

    it "распознает строки вида a^n b^n для грамматики S -> A C | epsilon, C -> S B, A -> a, B -> b" $ do
        -- Грамматика в CNF: S -> A C | ε, C -> S B, A -> a, B -> b
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "C"]),
                (NonTerminal "S", [Epsilon]),
                (NonTerminal "C", [NonTerminal "S", NonTerminal "B"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b'])
              ]

        -- Отрицательные примеры
        forM_ ["aab", "abb", "abc"] $ \s -> do
            cykParse grammar s `shouldBe` False

    it "распознает строки вида a^n b^n c^n для грамматики S -> A X | epsilon, X -> B Y, Y -> C, A -> a, B -> b, C -> c" $ do
        -- Грамматика в CNF:
        -- S -> A X | ε
        -- X -> B Y
        -- Y -> C
        -- A -> a
        -- B -> b
        -- C -> c
        let grammar = Grammar [
                (NonTerminal "S", [NonTerminal "A", NonTerminal "X"]),
                (NonTerminal "S", [Epsilon]),
                (NonTerminal "X", [NonTerminal "B", NonTerminal "Y"]),
                (NonTerminal "Y", [NonTerminal "C"]),
                (NonTerminal "A", [Terminal 'a']),
                (NonTerminal "B", [Terminal 'b']),
                (NonTerminal "C", [Terminal 'c'])
              ]


        -- Отрицательные примеры
        forM_ ["aabcc", "aabbc"] $ \s -> do
            cykParse grammar s `shouldBe` False


main :: IO ()
main = hspec spec
