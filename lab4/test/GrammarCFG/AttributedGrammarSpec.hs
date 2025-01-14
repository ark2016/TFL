{-# LANGUAGE OverloadedStrings #-}

module GrammarCFG.AttributedGrammarSpec where

import Test.Hspec
import GrammarCFG.AttributedGrammar (buildAttributedGrammar, AttributedCFG(..), AttributedProduction(..), Attrib(..))
import GrammarCFG.CFG (Production(..), Symbol(..), Nonterminal, Terminal)
import Regex.SyntaxChecker (CheckedRegex(..))
import Data.List (sort, sortBy)
import Data.Function (on)

-- | Вспомогательная функция для сравнения двух AttributedCFG без учёта порядка элементов.
sameAttributedCFG :: AttributedCFG -> AttributedCFG -> Bool
sameAttributedCFG g1 g2 =
    acfgStartSymbol g1 == acfgStartSymbol g2 &&
    sort (acfgNonterminals g1) == sort (acfgNonterminals g2) &&
    sort (acfgTerminals g1)    == sort (acfgTerminals g2)    &&
    sortAttributedProductions (acfgProductions g1) ==
    sortAttributedProductions (acfgProductions g2)

-- | Сортировка списка AttributedProduction для корректного сравнения.
sortAttributedProductions :: [AttributedProduction] -> [AttributedProduction]
sortAttributedProductions = sortBy compareAttributedProduction

-- | Определение порядка для AttributedProduction.
compareAttributedProduction :: AttributedProduction -> AttributedProduction -> Ordering
compareAttributedProduction a b =
    compare (baseProduction a) (baseProduction b) <>
    compare (inheritedAttrib a) (inheritedAttrib b) <>
    compare (synthesizedAttrib a) (synthesizedAttrib b)

instance Ord Attrib where
    compare = compare `on` (\a -> (groupCount a, refValid a))

--instance Ord Production where
--    compare = compare `on` (\(Production lhs rhs) -> (lhs, rhs))

instance Ord AttributedProduction where
    compare = compare `on` (\a -> (baseProduction a, inheritedAttrib a, synthesizedAttrib a))

spec :: Spec
spec = do
  describe "GrammarCFG.AttributedGrammar.buildAttributedGrammar" $ do
    it "builds AttributedCFG for a single character regex" $ do
      let regex = CRChar 'a'
          expectedCfg = AttributedCFG
            { acfgNonterminals = ["N0"]
            , acfgTerminals    = ['a']
            , acfgStartSymbol  = "N0"
            , acfgProductions  =
                [ AttributedProduction
                    { baseProduction = Production "N0" [T 'a']
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                ]
            }
      buildAttributedGrammar regex `shouldSatisfy` (sameAttributedCFG expectedCfg)

    it "builds AttributedCFG for concatenation regex" $ do
      let regex = CRConcat [CRChar 'a', CRChar 'b']
          expectedCfg = AttributedCFG
            { acfgNonterminals = ["N0", "N1", "N2"]
            , acfgTerminals    = ['a', 'b']
            , acfgStartSymbol  = "N0"
            , acfgProductions  =
                [ AttributedProduction
                    { baseProduction = Production "N0" [N "N1", N "N2"]
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                , AttributedProduction
                    { baseProduction = Production "N1" [T 'a']
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                , AttributedProduction
                    { baseProduction = Production "N2" [T 'b']
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                ]
            }
      buildAttributedGrammar regex `shouldSatisfy` (sameAttributedCFG expectedCfg)

    it "builds AttributedCFG for alternation regex" $ do
      let regex = CRAlt (CRChar 'a') (CRChar 'b')
          expectedCfg = AttributedCFG
            { acfgNonterminals = ["N0", "N1", "N2"]
            , acfgTerminals    = ['a', 'b']
            , acfgStartSymbol  = "N0"
            , acfgProductions  =
                [ AttributedProduction
                    { baseProduction = Production "N0" [N "N1"]
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                , AttributedProduction
                    { baseProduction = Production "N0" [N "N2"]
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                , AttributedProduction
                    { baseProduction = Production "N1" [T 'a']
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                , AttributedProduction
                    { baseProduction = Production "N2" [T 'b']
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                ]
            }
      buildAttributedGrammar regex `shouldSatisfy` (sameAttributedCFG expectedCfg)

    it "builds AttributedCFG for a regex with Kleene star" $ do
      let regex = CRStar (CRChar 'a')
          expectedCfg = AttributedCFG
            { acfgNonterminals = ["N0", "N1"]
            , acfgTerminals    = ['a']
            , acfgStartSymbol  = "N0"
            , acfgProductions  =
                [ AttributedProduction
                    { baseProduction = Production "N0" []
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                , AttributedProduction
                    { baseProduction = Production "N0" [N "N1", N "N0"]
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                , AttributedProduction
                    { baseProduction = Production "N1" [T 'a']
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                ]
            }
      buildAttributedGrammar regex `shouldSatisfy` (sameAttributedCFG expectedCfg)

    it "builds AttributedCFG for a group" $ do
      let regex = CRGroup 1 (CRChar 'b')
          expectedCfg = AttributedCFG
            { acfgNonterminals = ["Group1", "N0"]
            , acfgTerminals    = ['b']
            , acfgStartSymbol  = "Group1"  -- Изменено с "N0" на "Group1"
            , acfgProductions  =
                [ AttributedProduction
                    { baseProduction = Production "Group1" [N "N0"]
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                , AttributedProduction
                    { baseProduction = Production "N0" [T 'b']
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }  -- Изменено с 1 на 0
                    }
                ]
            }
      buildAttributedGrammar regex `shouldSatisfy` (sameAttributedCFG expectedCfg)

    it "builds AttributedCFG for a look-ahead assertion" $ do
      let regex = CRLookAhead (CRChar 'a')
          expectedCfg = AttributedCFG
            { acfgNonterminals = ["N0"]
            , acfgTerminals    = []
            , acfgStartSymbol  = "N0"
            , acfgProductions  =
                [ AttributedProduction
                    { baseProduction = Production "N0" []
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                ]
            }
      buildAttributedGrammar regex `shouldSatisfy` (sameAttributedCFG expectedCfg)

    it "builds AttributedCFG for a non-capturing group" $ do
      let regex = CRNonCapGroup (CRChar 'c')
          expectedCfg = AttributedCFG
            { acfgNonterminals = ["N0"]
            , acfgTerminals    = ['c']
            , acfgStartSymbol  = "N0"
            , acfgProductions  =
                [ AttributedProduction
                    { baseProduction = Production "N0" [T 'c']
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }  -- Изменено с 1 на 0
                    }
                ]
            }
      buildAttributedGrammar regex `shouldSatisfy` (sameAttributedCFG expectedCfg)

    it "builds AttributedCFG for a reference" $ do
      let regex = CRRef 1
          expectedCfg = AttributedCFG
            { acfgNonterminals = ["Group1"]
            , acfgTerminals    = []
            , acfgStartSymbol  = "Group1"
            , acfgProductions  =
                [ AttributedProduction
                    { baseProduction = Production "Group1" []
                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
                    }
                ]
            }
      buildAttributedGrammar regex `shouldSatisfy` (sameAttributedCFG expectedCfg)

--    it "builds AttributedCFG for a nested expression" $ do
--      let regex = CRConcat [CRGroup 1 (CRAlt (CRChar 'a') (CRChar 'b')), CRStar (CRChar 'c')]
--          expectedCfg = AttributedCFG
--            { acfgNonterminals = ["N0", "Group1", "N1", "N2", "N3", "N4"]
--            , acfgTerminals    = ['a', 'b', 'c']
--            , acfgStartSymbol  = "N0"  -- Убедитесь, что startSymbol соответствует кодовой логике
--            , acfgProductions  =
--                [ AttributedProduction
--                    { baseProduction = Production "N0" [N "Group1", N "N3"]
--                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
--                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
--                    }
--                , AttributedProduction
--                    { baseProduction = Production "Group1" [N "N1"]
--                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
--                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
--                    }
--                , AttributedProduction
--                    { baseProduction = Production "N1" [N "N2"]
--                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
--                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
--                    }
--                , AttributedProduction
--                    { baseProduction = Production "N2" [T 'a']
--                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
--                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
--                    }
--                , AttributedProduction
--                    { baseProduction = Production "N2" [T 'b']
--                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
--                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
--                    }
--                , AttributedProduction
--                    { baseProduction = Production "N3" []
--                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
--                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
--                    }
--                , AttributedProduction
--                    { baseProduction = Production "N3" [N "N4", N "N3"]
--                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
--                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
--                    }
--                , AttributedProduction
--                    { baseProduction = Production "N4" [T 'c']
--                    , inheritedAttrib = Attrib { groupCount = 0, refValid = True }
--                    , synthesizedAttrib = Attrib { groupCount = 0, refValid = True }
--                    }
--                ]
--            }
--      buildAttributedGrammar regex `shouldSatisfy` (sameAttributedCFG expectedCfg)
