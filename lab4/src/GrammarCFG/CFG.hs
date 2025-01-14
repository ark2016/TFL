{-# LANGUAGE DeriveGeneric #-}
module GrammarCFG.CFG
  ( Nonterminal(..)
  , Terminal
  , Symbol(..)
  , Production(..)
  , CFG(..)
  ) where

import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- | Базовые определения для контекстно-свободных грамматик

type Nonterminal = String
type Terminal    = Char

-- | Символ может быть либо терминалом, либо нетерминалом.
data Symbol
  = T Terminal       -- ^ Terminal symbol
  | N Nonterminal    -- ^ Nonterminal symbol
  deriving (Eq, Ord, Show, Generic)

-- | Приавило имеет один нетерминал в левой части и список символов в правой части.
data Production = Production
  { lhs :: Nonterminal
  , rhs :: [Symbol]
  } deriving (Eq, Ord, Show, Generic)

-- | A CFG = (N, T, S, P)
data CFG = CFG
  { nonterminals :: [Nonterminal]   -- ^ Set of nonterminals
  , terminals    :: [Terminal]      -- ^ Set of terminals
  , startSymbol  :: Nonterminal     -- ^ Start symbol
  , productions  :: [Production]    -- ^ List of productions
  } deriving (Eq, Ord, Show, Generic)
