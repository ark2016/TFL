{-# LANGUAGE DeriveGeneric #-}
module GrammarCFG.GrammarBuilder
  ( buildFrameGrammar
  ) where

import Regex.AST
import GrammarCFG.CFG
--import Regex.SyntaxChecker

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
-- Учитывая AST регулярного выражения, создайте CFG, который распознает «фрейм» регулярного выражения,
-- игнорируя просмотры вперед и захват/обратные ссылки в смысле фактического
-- захвата во время выполнения, но превращая их в новые нетерминалы.
--
-- Полученный CFG НЕ обрабатывает расширенные ограничения (такие как вложенные
-- просмотры вперед или частичная инициализация), поскольку мы строим только скелет
-- грамматики в соответствии с требованиями задачи.

buildFrameGrammar :: Regex -> CFG
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
-- | новый нетерминал, e.g. "N0", "N1", ...
freshNT :: Builder Nonterminal
freshNT = do
  st <- get
  let i = counter st
      nm = "N" ++ show i
  put st { counter = i + 1 }
  return nm

  -- | новое правило  в список правил в состоянии.
addProduction :: Nonterminal -> [Symbol] -> Builder ()
addProduction left right = do
  st <- get
  let p = Production left right
  put st { prods = prods st ++ [p] }

--------------------------------------------------------------------------------
-- | Основная рекурсия, которая обходит AST регулярного выражения и выдает правила CFG.
-- Возвращает имя нетерминала, который распознает это подрегулярное выражение.

go :: Regex -> Builder Nonterminal
--------------------------------------------------------------------------------
-- Одиночный символ
go (RChar c) = do
  nt <- freshNT
  addProduction nt [T c]
  return nt

--------------------------------------------------------------------------------
-- Concatenation: RConcat [r1, r2, ..., rn]
-- N -> N1 N2 ... Nn
go (RConcat rs) = do
  nt      <- freshNT
  subNTs  <- mapM go rs
  -- single production: N -> N1 N2 ... Nn
  addProduction nt (map N subNTs)
  return nt

--------------------------------------------------------------------------------
-- Alternation: RAlt r1 r2
-- N -> N1 | N2
go (RAlt r1 r2) = do
  nt   <- freshNT
  nt1  <- go r1
  nt2  <- go r2
  -- two productions:
  addProduction nt [N nt1]
  addProduction nt [N nt2]
  return nt

--------------------------------------------------------------------------------
-- Group: RGroup i r
-- Для «каркасной грамматики»  так же, как со скобками -> перейти к r
go (RGroup _ r) = go r

--------------------------------------------------------------------------------
-- нет фактического расширения для него в этой версии «только для каркаса».
-- Самый простой подход — притвориться, что это новое подвыражение нулевой длины
-- или «какое-то неизвестное подвыражение». Обычно связываем его с
-- группой, на которую оно ссылается. Для каркасной грамматики мы можем просто создать
-- новый нетерминал без правил или одно ε-правило.
--
-- Например, давайте создадим:
-- N_ref -> ε
-- так, чтобы не влиять на входные данные, но все еще присутствовать в грамматике.

go (RRef _i) = do
  nt <- freshNT
  -- produce epsilon:
  addProduction nt []
  return nt

--------------------------------------------------------------------------------
-- Look-ahead: RLookAhead r
-- В «каркасной грамматике» просмотр вперед не потребляет входные данные. Мы можем
-- просто создать эпсилон-правило, игнорируя содержимое r для
-- фактической распознанной строки (реальный парсер будет выполнять возвраты/проверки).
--
-- Итак, мы делаем:
--   N_look -> ε
go (RLookAhead _r) = do
  nt <- freshNT
  addProduction nt []
  return nt

--------------------------------------------------------------------------------
-- Незахватывающая группа: RNonCapGroup r
-- Мы относимся к ней так же, как к обычной группе: go r
go (RNonCapGroup r) = go r

--------------------------------------------------------------------------------
-- Kleene star: RStar r
-- We produce:
--   N -> ε | (subN) N
go (RStar r) = do
  nt    <- freshNT
  subNT <- go r
  -- N -> ε
  addProduction nt []
  -- N -> subNT N
  addProduction nt [N subNT, N nt]
  return nt
