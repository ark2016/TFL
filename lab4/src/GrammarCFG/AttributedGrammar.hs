{-# LANGUAGE DeriveGeneric #-}
module GrammarCFG.AttributedGrammar
  ( Attrib(..)
  , AttributedProduction(..)
  , AttributedCFG(..)
  , buildAttributedGrammar
  ) where

import GHC.Generics (Generic)
import Data.List (nub, isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Applicative ((<|>))

import GrammarCFG.CFG
import GrammarCFG.GrammarBuilder (buildFrameGrammar)

import Regex.SyntaxChecker (CheckedRegex(..))

--------------------------------------------------------------------------------
-- | Attribute type with a field for needed next character (for look-ahead)
data Attrib = Attrib
  { groupCount  :: Int          -- ^ How many capturing groups so far
  , refValid    :: Bool         -- ^ Are references still valid
  , neededFirst :: Maybe Char   -- ^ If we have a look-ahead constraint
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- | An attributed production: base Production + inherited & synthesized attributes
data AttributedProduction = AttributedProduction
  { baseProduction     :: Production
  , inheritedAttrib    :: Attrib
  , synthesizedAttrib  :: Attrib
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- | An attributed CFG
data AttributedCFG = AttributedCFG
  { acfgNonterminals :: [Nonterminal]
  , acfgTerminals    :: [Terminal]
  , acfgStartSymbol  :: Nonterminal
  , acfgProductions  :: [AttributedProduction]
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- | Build the attributed CFG from a 'CheckedRegex'
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
-- | State for attribute assignment
data AttribState = AttribState
  { currentGroupCount :: Int
  , validReferences   :: Bool
  , pendingLookahead  :: Maybe Char
  } deriving (Eq, Show)

-- | Initial state for attribute assignment
initState :: AttribState
initState = AttribState
  { currentGroupCount = 0
  , validReferences   = True
  , pendingLookahead  = Nothing
  }

--------------------------------------------------------------------------------
-- | Assign attributes to a single production
attributeProduction :: Production -> State AttribState AttributedProduction
attributeProduction p@(Production lhs rhs) = do
  st <- get

  -- Inherited attributes
  let inh = Attrib
        { groupCount  = currentGroupCount st
        , refValid    = validReferences st
        , neededFirst = pendingLookahead  st
        }

  -- Count new groups in RHS
  let newlyFoundGroups = [ i
                         | N nt <- rhs
                         , Just i <- [extractGroupNumber nt]
                         ]

  -- Increment groupCount by the number of new groups found
  let newGroupCount = currentGroupCount st + length newlyFoundGroups

  -- Check references in RHS
  let newRefValid = checkRefs rhs (currentGroupCount st)

  -- Check if this production indicates a lookahead
  let thisLook = parseLookaheadSymbol lhs rhs

  -- Combine with prior lookahead constraints
  let combinedLook = thisLook <|> pendingLookahead st

  -- If we have a neededLook, verify if 'rhs' starts with it
  let okLook = checkFirstMatchesLookahead rhs combinedLook

  -- Final reference validity
  let finalRefValid = newRefValid && okLook

  -- Synthesized attributes
  let syn = Attrib
        { groupCount  = newGroupCount
        , refValid    = finalRefValid
        , neededFirst = combinedLook
        }

  -- Update the state
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
-- | Count the number of capturing groups in the RHS
countGroups :: [Symbol] -> Int
countGroups syms = length [ nt | N nt <- syms, "Group" `isPrefixOf` nt ]

-- | Check the validity of references in the RHS based on total groups so far
checkRefs :: [Symbol] -> Int -> Bool
checkRefs syms tot =
  all (\num -> (num > 0) && (num <= tot))
      [ num
      | N nt <- syms
      , Just num <- [extractGroupNumber nt]  -- Replaced extractNumber with extractGroupNumber
      , "Group" `isPrefixOf` nt
      ]

-- | Extract the group number from a Nonterminal named "GroupX"
extractGroupNumber :: Nonterminal -> Maybe Int
extractGroupNumber nt =
  if "Group" `isPrefixOf` nt
    then case drop 5 nt of
           [] -> Nothing
           ds -> case reads ds of
                   [(n,"")] -> Just n
                   _        -> Nothing
    else Nothing

--------------------------------------------------------------------------------
-- | Parse lookahead symbol from the production
-- If the production is named "LookAhead_c" and has an empty RHS, interpret it as a lookahead for 'c'
parseLookaheadSymbol :: Nonterminal -> [Symbol] -> Maybe Char
parseLookaheadSymbol lhs rhs =
  if null rhs && "LookAhead_" `isPrefixOf` lhs
    then Just (last lhs)
    else Nothing

-- | Check if the first symbol matches the needed lookahead character
checkFirstMatchesLookahead :: [Symbol] -> Maybe Char -> Bool
checkFirstMatchesLookahead [] Nothing  = True
checkFirstMatchesLookahead [] (Just _) = True  -- No next symbol, can't confirm
checkFirstMatchesLookahead (T c : _) (Just needed) = (c == needed)
checkFirstMatchesLookahead (N nt : _) (Just needed) =
  -- A naive approach: assume it can produce the needed character
  True
checkFirstMatchesLookahead _ _ = True
