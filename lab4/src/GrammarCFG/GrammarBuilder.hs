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
-- | функция для расширения ссылок путем замены поддерева
-- из CRGroup i. Мы сохраняем map из i -> поддерево для каждой CRGroup i.
-- Мы также сохраняем «посещенный» set для обнаружения рекурсии.
--
-- State используется для хранения глобального состояния:
--
-- get — получает текущее состояние (gmap, visited).
-- put — обновляет состояние.
-- modify — изменяет состояние частично (например, удаляет элементы).

expandReferences :: CheckedRegex
                 -> State (Map Int CheckedRegex, Set Int)
                           CheckedRegex
expandReferences regex = case regex of
  CRConcat rs -> do
    rs' <- mapM expandReferences rs -- Рекурсивно разворачиваем все элементы списка
    return (CRConcat rs') -- Собираем результаты в новый узел CRConcat rs'

  CRAlt r1 r2 -> do
    r1' <- expandReferences r1
    r2' <- expandReferences r2
    return (CRAlt r1' r2')

  CRGroup i sub -> do
    -- Сохранить подгруппу в groupMap
    (gmap, visited) <- get -- Получаем текущее состояние (gmap, visited) с помощью get
    let gmap' = Map.insert i sub gmap -- Добавляем группу CRGroup i sub в gmap с помощью Map.insert
    put (gmap', visited) -- Функция put — это часть монады State, которая обновляет состояние.
    -- Развернуть sub
    sub' <- expandReferences sub
    return (CRGroup i sub')

  CRRef i -> do
    (gmap, visited) <- get
    if i `Set.member` visited
      then
        -- Мы попали в рекурсию. Оставим как CRRef i, чтобы избежать бесконечного расширения.
        return (CRRef i)
      else case Map.lookup i gmap of
             Nothing -> return (CRRef i) -- Группа неизвестна => оставить как есть
             Just subTree -> do
               -- Отметим, что мы сейчас посещаем i
               put (gmap, Set.insert i visited)
               expanded <- expandReferences subTree
               -- После расширения удалим i из visited, чтобы разрешить повторный вход при необходимости.
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
-- Имея проверенное AST регулярного выражения (`CheckedRegex`),
-- создаем CFG, который распознает "каркас" регулярного выражения,
-- игнорируя утверждения о просмотре вперед и захват/обратные ссылки,
-- преобразуя их в новые нетерминалы.

buildFrameGrammar :: CheckedRegex -> CFG
buildFrameGrammar ast =
  let initMap = Map.empty -- Изначально словарь ссылок на группы пустой
      initSet = Set.empty -- Множество посещенных групп также пустое
      -- expandReferences ast — развертывает все ссылки CRRef i в их соответствующие поддеревья CRGroup i.
      -- runState запускает вычисление в монаде State с начальными значениями (initMap, initSet).
      -- Возвращается:
      -- expandedAst — дерево регулярного выражения со встроенными ссылками.
      -- _finalState — финальное состояние (игнорируется здесь).
      (expandedAst, _finalState) = runState (expandReferences ast) (initMap, initSet)
      -- Теперь у нас есть дерево со встроенными ссылками.
      initSt = BuildState 0 [] -- Начальный счетчик для нумерации нетерминалов
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
-- Эта строка определяет синоним типа Builder a, который представляет собой монаду состояния State BuildState a.
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
-- | Основная рекурсия, которая обходит AST CheckedRegex и генерирует правила CFG.
-- Возвращает имя нетерминала, который распознает заданное подрегулярное выражение.

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
-- Ссылка на группу
-- Поскольку мы уже расширили ссылки, этот случай в идеале не должен возникать.
-- Однако для обработки оставшихся случаев мы сопоставляем его с пустым правилом.

go (CRRef i) = do
  let ntName = "Group" ++ show i
  addProduction ntName []
  return ntName

--------------------------------------------------------------------------------
-- Look-ahead Assertion
go (CRLookAhead (CRChar c)) = do
  let nt = "LookAhead_" ++ [c]  -- храним символ в имени
  addProduction nt []
  return nt

--------------------------------------------------------------------------------
-- Non-Capturing Group
go (CRNonCapGroup r) = do
  -- Рассматривать как обычную подстроку
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
