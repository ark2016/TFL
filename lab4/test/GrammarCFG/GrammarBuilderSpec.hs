{-# LANGUAGE OverloadedStrings #-}

module GrammarCFG.GrammarBuilderSpec where

import Test.Hspec
import GrammarCFG.GrammarBuilder (buildFrameGrammar)
import Regex.SyntaxChecker (CheckedRegex(..))
-- import Regex.AST -- Не требуется, используем CheckedRegex
import GrammarCFG.CFG
import Data.List (sort)
import Data.Function (on)

-- | Вспомогательная функция для сравнения двух CFG без учёта порядка элементов.
sameCFG :: CFG -> CFG -> Bool
sameCFG g1 g2 =
  startSymbol g1 == startSymbol g2 &&
  sort (nonterminals g1) == sort (nonterminals g2) &&
  sort (terminals g1)    == sort (terminals g2)    &&
  sort (productions g1)  == sort (productions g2)

spec :: Spec
spec = do
  describe "GrammarCFG.GrammarBuilder.buildFrameGrammar" $ do
    it "builds CFG for a single character regex" $ do
      let regex = CRChar 'a'
          expectedCfg = CFG
            { nonterminals = ["N0"]
            , terminals = ['a']
            , startSymbol = "N0"
            , productions = [Production "N0" [T 'a']]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for concatenation regex" $ do
      let regex = CRConcat [CRChar 'a', CRChar 'b']
          expectedCfg = CFG
            { nonterminals = ["N0", "N1", "N2"]
            , terminals = ['a', 'b']
            , startSymbol = "N0"
            , productions = [ Production "N0" [N "N1", N "N2"]
                            , Production "N1" [T 'a']
                            , Production "N2" [T 'b']
                            ]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for alternation regex" $ do
      let regex = CRAlt (CRChar 'a') (CRChar 'b')
          expectedCfg = CFG
            { nonterminals = ["N0", "N1", "N2"]
            , terminals = ['a', 'b']
            , startSymbol = "N0"
            , productions = [ Production "N0" [N "N1"]
                            , Production "N0" [N "N2"]
                            , Production "N1" [T 'a']
                            , Production "N2" [T 'b']
                            ]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for a regex with Kleene star" $ do
      let regex = CRStar (CRChar 'a')
          expectedCfg = CFG
            { nonterminals = ["N0", "N1"]
            , terminals = ['a']
            , startSymbol = "N0"
            , productions = [ Production "N0" []
                            , Production "N0" [N "N1", N "N0"]
                            , Production "N1" [T 'a']
                            ]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for a group" $ do
      let regex = CRGroup 1 (CRChar 'b') -- Изменено с RGroup 0 на CRGroup 1
          expectedCfg = CFG
            { nonterminals = ["N0", "Group1"]
            , terminals = ['b']
            , startSymbol = "Group1"
            , productions = [ Production "N0" [T 'b']
                            , Production "Group1" [N "N0"]
                            ]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for a look-ahead assertion" $ do
      let regex = CRLookAhead (CRChar 'a')
          expectedCfg = CFG
            { nonterminals = ["N0"]
            , terminals = []
            , startSymbol = "N0"
            , productions = [Production "N0" []]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for a non-capturing group" $ do
      let regex = CRNonCapGroup (CRChar 'c')
          expectedCfg = CFG
            { nonterminals = ["N0"]
            , terminals = ['c']
            , startSymbol = "N0"
            , productions = [Production "N0" [T 'c']]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for a reference" $ do
      let regex = CRRef 1
          expectedCfg = CFG
            { nonterminals = ["Group1"]
            , terminals = []
            , startSymbol = "Group1"
            , productions = [Production "Group1" []]
            }
      buildFrameGrammar regex  `shouldBe` expectedCfg


main :: IO ()
main = hspec spec
