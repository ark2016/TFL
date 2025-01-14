{-# LANGUAGE OverloadedStrings #-}

module GrammarCFG.GrammarBuilderSpec where

import Test.Hspec
import GrammarCFG.GrammarBuilder (buildFrameGrammar)
import Regex.AST
import GrammarCFG.CFG
import Data.List (sort)

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
      let regex = RChar 'a'
          expectedCfg = CFG
            { nonterminals = ["N0"]
            , terminals = ['a']
            , startSymbol = "N0"
            , productions = [Production "N0" [T 'a']]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for concatenation regex" $ do
      let regex = RConcat [RChar 'a', RChar 'b']
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
      let regex = RAlt (RChar 'a') (RChar 'b')
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
      let regex = RStar (RChar 'a')
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
      let regex = RGroup 0 (RChar 'b')
          expectedCfg = CFG
            { nonterminals = ["N0"]
            , terminals = ['b']
            , startSymbol = "N0"
            , productions = [Production "N0" [T 'b']]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for a look-ahead assertion" $ do
      let regex = RLookAhead (RChar 'a')
          expectedCfg = CFG
            { nonterminals = ["N0"]
            , terminals = []
            , startSymbol = "N0"
            , productions = [Production "N0" []]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for a non-capturing group" $ do
      let regex = RNonCapGroup (RChar 'c')
          expectedCfg = CFG
            { nonterminals = ["N0"]
            , terminals = ['c']
            , startSymbol = "N0"
            , productions = [Production "N0" [T 'c']]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

    it "builds CFG for a reference" $ do
      let regex = RRef 1
          expectedCfg = CFG
            { nonterminals = ["N0"]
            , terminals = []
            , startSymbol = "N0"
            , productions = [Production "N0" []]
            }
      sameCFG (buildFrameGrammar regex) expectedCfg `shouldBe` True

--    it "builds CFG for a nested expression" $ do
--      let regex = RConcat [RGroup 0 (RAlt (RChar 'a') (RChar 'b')), RStar (RChar 'c')]
--          expectedCfg = CFG
--            { nonterminals = ["N0", "N1", "N2", "N3"]
--            , terminals = ['a', 'b', 'c']
--            , startSymbol = "N0"
--            , productions = [ Production "N0" [N "N1", N "N3"]
--                            , Production "N1" [N "N2"]
--                            , Production "N2" [T 'a']
--                            , Production "N2" [T 'b']
--                            , Production "N3" []
--                            , Production "N3" [N "N3", N "N3"] -- Исправлено на [N "N3", N "N3"]?
--                            ]
--            }
--      buildFrameGrammar regex `shouldBe` expectedCfg

main :: IO ()
main = hspec spec
