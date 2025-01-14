{-# LANGUAGE DeriveGeneric #-}
module GrammarCFG.GrammarBuilder
  ( buildFrameGrammar
  ) where

import Regex.SyntaxChecker (CheckedRegex(..))
import GrammarCFG.CFG
-- import Regex.AST -- Не требуется, используем CheckedRegex

import Control.Monad.State
import Data.List (nub)

--------------------------------------------------------------------------------
-- | Состояние построения CFG: нам нужно отслеживать
-- (1) следующий свежий индекс для именования нетерминалов,
-- (2) список построенных на данный момент производств.

data BuildState = BuildState
  { counter     :: Int               -- следующий свежий индекс
  , prods       :: [Production]      -- накопленные правила
  }

-- | The builder monad: State BuildState
type Builder a = State BuildState a

--------------------------------------------------------------------------------
-- | Публичная функция: buildFrameGrammar
--
-- Учитывая проверенный AST регулярного выражения (`CheckedRegex`),
-- создайте CFG, который распознает «фрейм» регулярного выражения,
-- игнорируя просмотры вперед и захват/обратные ссылки, превращая их
-- в новые нетерминалы.

buildFrameGrammar :: CheckedRegex -> CFG
buildFrameGrammar ast =
  let initSt = BuildState 0 []
      (startN, endSt) = runState (go ast) initSt
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
-- | Создать новый нетерминал, например, "N0", "N1", ...
freshNT :: Builder Nonterminal
freshNT = do
  st <- get
  let i = counter st
      nm = "N" ++ show i
  put st { counter = i + 1 }
  return nm

-- | Добавить новое правило в список правил в состоянии.
addProduction :: Nonterminal -> [Symbol] -> Builder ()
addProduction left right = do
  st <- get
  let p = Production left right
  put st { prods = prods st ++ [p] }

--------------------------------------------------------------------------------
-- | Основная рекурсия, которая обходит AST регулярного выражения и выдает правила CFG.
-- Возвращает имя нетерминала, который распознает это подрегулярное выражение.

go :: CheckedRegex -> Builder Nonterminal
--------------------------------------------------------------------------------
-- Одиночный символ
go (CRChar c) = do
  nt <- freshNT
  addProduction nt [T c]
  return nt

--------------------------------------------------------------------------------
-- Конкатенация
go (CRConcat rs) = do
  nt      <- freshNT
  subNTs  <- mapM go rs
  -- одно правило: N -> N1 N2 ... Nn
  addProduction nt (map N subNTs)
  return nt

--------------------------------------------------------------------------------
-- Альтернация
go (CRAlt r1 r2) = do
  nt   <- freshNT
  nt1  <- go r1
  nt2  <- go r2
  -- два правила:
  addProduction nt [N nt1]
  addProduction nt [N nt2]
  return nt

--------------------------------------------------------------------------------
-- Группа
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
-- Ссылка на группу
go (CRRef i) = do
  let ntName = "Group" ++ show i
  addProduction ntName []
  return ntName

--------------------------------------------------------------------------------
-- Look-ahead
go (CRLookAhead r) = do
  nt <- freshNT
  -- Создаём ε-правило
  addProduction nt []
  return nt

--------------------------------------------------------------------------------
-- Незахватывающая группа
go (CRNonCapGroup r) = do
  -- Рассматриваем как обычную подстроку
  go r

--------------------------------------------------------------------------------
-- Kleene star
go (CRStar r) = do
  nt    <- freshNT
  subNT <- go r
  -- N -> ε
  addProduction nt []
  -- N -> subNT N
  addProduction nt [N subNT, N nt]
  return nt
