{-# LANGUAGE OverloadedStrings #-}

module Regex.ParserSpec where

import Test.Hspec
import Text.Parsec (ParseError)
import Regex.Parser (parseRegex)
import Regex.AST

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Regex.Parser.parseRegex" $ do
    it "parses a single character" $ do
      parseRegex "a" `shouldBe` Right (RChar 'a')

    it "parses a concatenation of characters" $ do
      parseRegex "ab" `shouldBe` Right (RConcat [RChar 'a', RChar 'b'])

    it "parses an alternation of characters" $ do
      parseRegex "a|b" `shouldBe` Right (RAlt (RChar 'a') (RChar 'b'))

    it "parses a capturing group" $ do
      parseRegex "(a)" `shouldBe` Right (RGroup 0 (RChar 'a'))

    it "parses a reference to a capturing group" $ do
      parseRegex "(?1)" `shouldBe` Right (RRef 1)

    it "parses a look-ahead assertion" $ do
      parseRegex "(?=a)" `shouldBe` Right (RLookAhead (RChar 'a'))

    it "parses a non-capturing group" $ do
      parseRegex "(?:a)" `shouldBe` Right (RNonCapGroup (RChar 'a'))

    it "parses a star repetition" $ do
      parseRegex "a*" `shouldBe` Right (RStar (RChar 'a'))

    it "parses a combination of concatenation and alternation" $ do
      parseRegex "a|bc" `shouldBe` Right (RAlt (RChar 'a') (RConcat [RChar 'b', RChar 'c']))

    it "parses nested groups" $ do
      parseRegex "((a|b)c)" `shouldBe` Right (RGroup 0 (RConcat [RGroup 0 (RAlt (RChar 'a') (RChar 'b')), RChar 'c']))

    it "parses a complex regex with all constructs" $ do
      parseRegex "(a|b)(?1)(?=c)d*" `shouldBe`
        Right (RConcat
          [ RGroup 0 (RAlt (RChar 'a') (RChar 'b'))
          , RRef 1
          , RLookAhead (RChar 'c')
          , RStar (RChar 'd')
          ])

    it "fails to parse invalid regex" $ do
      parseRegex "(a|b" `shouldSatisfy` isLeft
      parseRegex "(?a)" `shouldSatisfy` isLeft
      parseRegex "(*)" `shouldSatisfy` isLeft

-- Helper function to check for parse failures
isLeft :: Either ParseError a -> Bool
isLeft (Left _) = True
isLeft _        = False
