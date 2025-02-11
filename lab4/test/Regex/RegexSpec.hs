{-# LANGUAGE OverloadedStrings #-}

module Regex.RegexSpec where

import Test.Hspec
import Regex.AST

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Regex.AST" $ do
    describe "RChar" $ do
      it "parses a single character" $ do
        let regex = RChar 'a'
        show regex `shouldBe` "RChar 'a'"
        regex `shouldBe` RChar 'a'

    describe "RConcat" $ do
      it "represents concatenation of multiple regexes" $ do
        let regex = RConcat [RChar 'a', RChar 'b', RChar 'c']
        show regex `shouldBe` "RConcat [RChar 'a',RChar 'b',RChar 'c']"

    describe "RAlt" $ do
      it "represents alternation between two regexes" $ do
        let regex = RAlt (RChar 'a') (RChar 'b')
        show regex `shouldBe` "RAlt (RChar 'a') (RChar 'b')"

    describe "RGroup" $ do
      it "represents a capturing group with an ID" $ do
        let regex = RGroup 1 (RChar 'a')
        show regex `shouldBe` "RGroup 1 (RChar 'a')"

    describe "RRef" $ do
      it "represents a reference to a group by ID" $ do
        let regex = RRef 1
        show regex `shouldBe` "RRef 1"

    describe "RLookAhead" $ do
      it "represents a look-ahead assertion" $ do
        let regex = RLookAhead (RChar 'a')
        show regex `shouldBe` "RLookAhead (RChar 'a')"

    describe "RNonCapGroup" $ do
      it "represents a non-capturing group" $ do
        let regex = RNonCapGroup (RChar 'a')
        show regex `shouldBe` "RNonCapGroup (RChar 'a')"

    describe "RStar" $ do
      it "represents a repetition of a regex" $ do
        let regex = RStar (RChar 'a')
        show regex `shouldBe` "RStar (RChar 'a')"
