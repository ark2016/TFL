{-# LANGUAGE OverloadedStrings #-}

module Regex.SyntaxCheckerSpec where

import Test.Hspec
import Regex.SyntaxChecker
import Regex.AST

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Regex.SyntaxChecker.checkRegex" $ do
    it "accepts a valid regex with no groups" $ do
      let input = RConcat [RChar 'a', RChar 'b']
      let expected = Right (CRConcat [CRChar 'a', CRChar 'b'])
      checkRegex input `shouldBe` expected

    it "accepts a valid regex with groups and references" $ do
      let input = RConcat [RGroup 0 (RChar 'a'), RRef 1]
      let expected = Right (CRConcat [CRGroup 1 (CRChar 'a'), CRRef 1])
      checkRegex input `shouldBe` expected

    it "rejects regex with too many groups" $ do
      let input = RConcat (replicate 10 (RGroup 0 (RChar 'a')))
      checkRegex input `shouldBe` Left TooManyGroups

    it "rejects regex with invalid group reference" $ do
      let input = RRef 1
      checkRegex input `shouldBe` Left (InvalidGroupRef 1)

    it "rejects regex with future group reference" $ do
      let input = RConcat [RRef 2, RGroup 0 (RChar 'a')]
      checkRegex input `shouldBe` Left (InvalidGroupRef  2)

    it "rejects nested look-ahead" $ do
      let input = RLookAhead (RLookAhead (RChar 'a'))
      checkRegex input `shouldBe` Left NestedLookAhead

    it "rejects group inside look-ahead" $ do
      let input = RLookAhead (RGroup 0 (RChar 'a'))
      checkRegex input `shouldBe` Left NestedLookAhead

    it "accepts non-capturing groups inside look-ahead" $ do
      let input = RLookAhead (RNonCapGroup (RChar 'a'))
      let expected = Right (CRLookAhead (CRNonCapGroup (CRChar 'a')))
      checkRegex input `shouldBe` expected

    it "accepts valid regex with alternation and repetition" $ do
      let input = RAlt (RChar 'a') (RStar (RChar 'b'))
      let expected = Right (CRAlt (CRChar 'a') (CRStar (CRChar 'b')))
      checkRegex input `shouldBe` expected

    it "accepts valid regex with nested concatenation" $ do
      let input = RConcat [RConcat [RChar 'a', RChar 'b'], RChar 'c']
      let expected = Right (CRConcat [CRConcat [CRChar 'a', CRChar 'b'], CRChar 'c'])
      checkRegex input `shouldBe` expected

    it "rejects look-ahead with invalid references inside" $ do
      let input = RLookAhead (RRef 1)
      checkRegex input `shouldBe` Left (InvalidGroupRef 1)
