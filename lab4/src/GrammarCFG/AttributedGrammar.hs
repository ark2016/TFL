{-# LANGUAGE DeriveGeneric #-}
module GrammarCFG.AttributedGrammar
  ( Attrib(..)
  , AttributedProduction(..)
  , AttributedCFG(..)
  , buildAttributedGrammar
  ) where

import GHC.Generics (Generic)
import Data.List (nub, isPrefixOf)
import Control.Monad.State
import Control.Applicative ((<|>))

import GrammarCFG.CFG
import GrammarCFG.GrammarBuilder (buildFrameGrammar)
import Regex.SyntaxChecker (CheckedRegex(..))

--------------------------------------------------------------------------------
-- | Expand the attribute type with a field for the needed next char
data Attrib = Attrib
  { groupCount  :: Int          -- how many capturing groups so far
  , refValid    :: Bool         -- are references still valid
  , neededFirst :: Maybe Char   -- if we have a look-ahead constraint
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
data AttributedProduction = AttributedProduction
  { baseProduction     :: Production
  , inheritedAttrib    :: Attrib
  , synthesizedAttrib  :: Attrib
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
data AttributedCFG = AttributedCFG
  { acfgNonterminals :: [Nonterminal]
  , acfgTerminals    :: [Terminal]
  , acfgStartSymbol  :: Nonterminal
  , acfgProductions  :: [AttributedProduction]
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- The main function
buildAttributedGrammar :: CheckedRegex -> AttributedCFG
buildAttributedGrammar ast =
  let cfg = buildFrameGrammar ast
      attProds = evalState (mapM attributeProduction (productions cfg)) initState
  in AttributedCFG
       { acfgNonterminals = nonterminals cfg
       , acfgTerminals    = terminals cfg
       , acfgStartSymbol  = startSymbol cfg
       , acfgProductions  = attProds
       }

--------------------------------------------------------------------------------
data AttribState = AttribState
  { currentGroupCount :: Int
  , validReferences   :: Bool
  , pendingLookahead  :: Maybe Char
  }
  deriving (Eq, Show)

initState :: AttribState
initState = AttribState
  { currentGroupCount = 0
  , validReferences   = True
  , pendingLookahead  = Nothing
  }

--------------------------------------------------------------------------------
attributeProduction :: Production -> State AttribState AttributedProduction
attributeProduction p@(Production lhs rhs) = do
  st <- get

  -- inherited attribute
  let inh = Attrib
        { groupCount  = currentGroupCount st
        , refValid    = validReferences st
        , neededFirst = pendingLookahead  st
        }

  -- normal group counting
  let newGroupCount = currentGroupCount st + countGroups rhs
      newRefValid   = checkRefs rhs (currentGroupCount st)

  -- check if this production indicates a lookahead
  let thisLook = parseLookaheadSymbol lhs rhs

  -- combine with prior
  let combinedLook = thisLook <|> pendingLookahead st

  -- if we have a neededLook, check if 'rhs' starts with it
  let okLook = checkFirstMatchesLookahead rhs combinedLook

  let finalRefValid = newRefValid && okLook

  let syn = Attrib
        { groupCount  = newGroupCount
        , refValid    = finalRefValid
        , neededFirst = combinedLook
        }

  put st
    { currentGroupCount = newGroupCount
    , validReferences   = finalRefValid
    , pendingLookahead  = combinedLook
    }

  return AttributedProduction
    { baseProduction     = p
    , inheritedAttrib    = inh
    , synthesizedAttrib  = syn
    }

--------------------------------------------------------------------------------
countGroups :: [Symbol] -> Int
countGroups syms = length [ nt | N nt <- syms, "Group" `isPrefixOf` nt ]

checkRefs :: [Symbol] -> Int -> Bool
checkRefs syms tot =
  -- same as your code: check "GroupX"
  all (\num -> (num > 0) && (num <= tot))
      [ num
      | N nt <- syms
      , Just num <- [extractNumber nt]
      , "Group" `isPrefixOf` nt
      ]

extractNumber :: String -> Maybe Int
extractNumber nt = case dropWhile (not . (`elem` ['0'..'9'])) nt of
  [] -> Nothing
  ds -> case reads ds of
          [(n,"")] -> Just n
          _        -> Nothing

--------------------------------------------------------------------------------
-- If we named the LHS as "LookAhead_c" and the production is Epsilon,
-- we interpret that as lookahead c
parseLookaheadSymbol :: Nonterminal -> [Symbol] -> Maybe Char
parseLookaheadSymbol lhs rhs =
  if null rhs && "LookAhead_" `isPrefixOf` lhs
    then Just (last lhs)
    else Nothing

checkFirstMatchesLookahead :: [Symbol] -> Maybe Char -> Bool
checkFirstMatchesLookahead [] Nothing  = True
checkFirstMatchesLookahead [] (Just _) = True  -- no next symbol, can't confirm
checkFirstMatchesLookahead (T c : _) (Just needed) = (c == needed)
checkFirstMatchesLookahead (N nt : _) (Just needed) =
  -- If we want a deep check, we do FIRST(nt).
  -- We'll just return True for now, or attempt a naive check.
  True
checkFirstMatchesLookahead _ _ = True
