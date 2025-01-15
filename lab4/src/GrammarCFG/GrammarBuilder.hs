{-# LANGUAGE DeriveGeneric #-}
module GrammarCFG.GrammarBuilder
  ( buildFrameGrammar
  ) where

import Regex.SyntaxChecker (CheckedRegex(..))
import GrammarCFG.CFG

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)

--------------------------------------------------------------------------------
-- | Schematic function to expand references by substituting the sub-tree
--   from CRGroup i. We store a map from i -> sub-tree for each CRGroup i.
--   We also store a 'visited' set to detect recursion.

expandReferences :: CheckedRegex
                 -> State (Map Int CheckedRegex, Set Int)
                           CheckedRegex
expandReferences regex = case regex of
  CRConcat rs -> do
    rs' <- mapM expandReferences rs
    return (CRConcat rs')

  CRAlt r1 r2 -> do
    r1' <- expandReferences r1
    r2' <- expandReferences r2
    return (CRAlt r1' r2')

  CRGroup i sub -> do
    -- Store sub in groupMap
    (gmap, visited) <- get
    let gmap' = Map.insert i sub gmap
    put (gmap', visited)
    -- Expand the sub
    sub' <- expandReferences sub
    return (CRGroup i sub')

  CRRef i -> do
    (gmap, visited) <- get
    if i `Set.member` visited
      then
        -- We hit recursion. Keep it as CRRef i to avoid infinite expansion.
        return (CRRef i)
      else case Map.lookup i gmap of
             Nothing -> return (CRRef i) -- No known group => keep as is
             Just subTree -> do
               -- Mark that we're now visiting i
               put (gmap, Set.insert i visited)
               expanded <- expandReferences subTree
               -- After expansion, remove i from visited to allow re-entry if needed
               modify (\(gm, vs) -> (gm, Set.delete i vs))
               return expanded

  CRLookAhead r -> do
    r' <- expandReferences r
    return (CRLookAhead r')

  CRNonCapGroup r -> do
    r' <- expandReferences r
    return (CRNonCapGroup r')

  CRStar r -> do
    r' <- expandReferences r
    return (CRStar r')

  CRChar c -> return (CRChar c)

--------------------------------------------------------------------------------
-- | Public function: buildFrameGrammar
--
-- Given a validated AST of a regular expression (`CheckedRegex`),
-- create a CFG that recognizes the "frame" of the regular expression,
-- ignoring look-ahead assertions and capturing/back-references,
-- transforming them into new nonterminals.

buildFrameGrammar :: CheckedRegex -> CFG
buildFrameGrammar ast =
  let initMap = Map.empty
      initSet = Set.empty
      (expandedAst, _finalState) = runState (expandReferences ast) (initMap, initSet)
      -- Now we have a tree with references inlined
      initSt = BuildState 0 []
      (startN, endSt) = runState (go expandedAst) initSt
      allProds        = prods endSt
      allNTs          = nub (map lhs allProds)
      allTerminals    = nub [ c | Production _ syms <- allProds, T c <- syms ]
  in CFG
     { nonterminals = allNTs
     , terminals    = allTerminals
     , startSymbol  = startN
     , productions  = allProds
     }

--------------------------------------------------------------------------------
-- | State for building CFG: tracking the next fresh index and accumulated productions.
data BuildState = BuildState
  { counter     :: Int               -- ^ Next fresh index
  , prods       :: [Production]      -- ^ Accumulated productions
  }

-- | The builder monad: State BuildState
type Builder a = State BuildState a

--------------------------------------------------------------------------------
-- | Create a new nonterminal, e.g., "N0", "N1", ...
freshNT :: Builder Nonterminal
freshNT = do
  st <- get
  let i = counter st
      nm = "N" ++ show i
  put st { counter = i + 1 }
  return nm

-- | Add a new production to the list of productions in the state.
addProduction :: Nonterminal -> [Symbol] -> Builder ()
addProduction left right = do
  st <- get
  let p = Production left right
  put st { prods = prods st ++ [p] }

--------------------------------------------------------------------------------
-- | Main recursion that traverses the CheckedRegex AST and generates CFG productions.
-- Returns the name of the nonterminal that recognizes the given sub-regular expression.

go :: CheckedRegex -> Builder Nonterminal
--------------------------------------------------------------------------------
-- Single character
go (CRChar c) = do
  nt <- freshNT
  addProduction nt [T c]
  return nt

--------------------------------------------------------------------------------
-- Concatenation
go (CRConcat rs) = do
  nt      <- freshNT
  subNTs  <- mapM go rs
  -- Single production: N -> N1 N2 ... Nn
  addProduction nt (map N subNTs)
  return nt

--------------------------------------------------------------------------------
-- Alternation
go (CRAlt r1 r2) = do
  nt   <- freshNT
  nt1  <- go r1
  nt2  <- go r2
  -- Two productions:
  addProduction nt [N nt1]
  addProduction nt [N nt2]
  return nt

--------------------------------------------------------------------------------
-- Capturing Group
go (CRGroup i r) = do
  let ntName = "Group" ++ show i
  st <- get
  if ntName `elem` map lhs (prods st)
    then return ntName
    else do
      subNT <- go r
      addProduction ntName [N subNT]
      return ntName

--------------------------------------------------------------------------------
-- Reference to a Group
-- Since we've already expanded references, this case should ideally not occur.
-- However, to handle any remaining cases, we map it to an empty production.
go (CRRef i) = do
  let ntName = "Group" ++ show i
  addProduction ntName []
  return ntName

--------------------------------------------------------------------------------
-- Look-ahead Assertion
go (CRLookAhead (CRChar c)) = do
  let nt = "LookAhead_" ++ [c]  -- Store the character in the name
  addProduction nt []
  return nt

--------------------------------------------------------------------------------
-- Non-Capturing Group
go (CRNonCapGroup r) = do
  -- Treat as a regular substring
  go r

--------------------------------------------------------------------------------
-- Kleene Star
go (CRStar r) = do
  nt    <- freshNT
  subNT <- go r
  -- Two productions:
  -- N -> ε
  addProduction nt []
  -- N -> subNT N
  addProduction nt [N subNT, N nt]
  return nt
