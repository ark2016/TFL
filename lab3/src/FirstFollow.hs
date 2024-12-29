{-# LANGUAGE OverloadedStrings #-}
module FirstFollow (
    computeFirst,
    computeFollow
) where

import Grammar
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.List (foldl')

--------------------------------------------------------------------------------
-- Типы (если в Grammar.hs уже есть — дублировать не нужно,
-- тут приведены для иллюстрации)
--------------------------------------------------------------------------------

{-
-- Предположим, что в Grammar.hs у вас что-то подобное:

data Symbol
  = Terminal Char
  | NonTerminal String
  | Epsilon
  deriving (Eq, Ord, Show)

type Rule = (Symbol, [Symbol])

newtype Grammar = Grammar [Rule]
  deriving (Eq, Show)
-}

--------------------------------------------------------------------------------
-- Фиксированная точка (итеративное вычисление)
--------------------------------------------------------------------------------

fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f x =
  let x' = f x
  in if x' == x
     then x
     else fixPoint f x'

--------------------------------------------------------------------------------
-- 1. FIRST
--------------------------------------------------------------------------------

computeFirst :: Grammar -> Map Symbol (Set Symbol)
computeFirst (Grammar rules) = fixPoint updateFirst initialFirst
  where
    -- Изначально для каждого нетерминала — пустое множество FIRST
    initialFirst :: Map Symbol (Set Symbol)
    initialFirst = Map.fromList
      [ (lhs, Set.empty)
      | (lhs, _) <- rules
      ]

    -- Один "шаг" обновления для всех правил
    updateFirst :: Map Symbol (Set Symbol) -> Map Symbol (Set Symbol)
    updateFirst oldFirst = foldl' (applyRule oldFirst) oldFirst rules

    -- Применяем одно правило (lhs -> rhs) и дополняем FIRST(lhs)
    applyRule
      :: Map Symbol (Set Symbol)    -- текущее состояние FIRST
      -> Map Symbol (Set Symbol)    -- аккумулятор (уже обновлённый)
      -> Rule                       -- (lhs, rhs)
      -> Map Symbol (Set Symbol)
    applyRule oldFirst acc (lhs, rhs) =
      let newSyms = calcFirstFromRhs rhs oldFirst
      in Map.insertWith Set.union lhs newSyms acc

    -- Вычисляет FIRST для цепочки символов rhs
    calcFirstFromRhs
      :: [Symbol]
      -> Map Symbol (Set Symbol)
      -> Set Symbol
    calcFirstFromRhs [] _ = Set.singleton Epsilon
    calcFirstFromRhs (x:xs) firstMap =
      case x of
        Terminal t -> Set.singleton (Terminal t)
        NonTerminal nt ->
          let ntFirst = Map.findWithDefault Set.empty (NonTerminal nt) firstMap
          in if Set.member Epsilon ntFirst
                then Set.union (Set.delete Epsilon ntFirst)
                               (calcFirstFromRhs xs firstMap)
                else ntFirst
        Epsilon -> Set.singleton Epsilon

--------------------------------------------------------------------------------
-- 2. FOLLOW
--------------------------------------------------------------------------------

computeFollow :: Grammar -> Map Symbol (Set Symbol)
computeFollow g@(Grammar rules) =
    fixPoint (\oldFollow -> step rules firstMap oldFollow) initialFollow
  where
    -- Определим стартовый символ (первый нетерминал из списка правил)
    startSymbol :: Symbol
    startSymbol = case rules of
                    [] -> error "Empty grammar"
                    ((lhs, _):_) -> lhs

    -- Сначала вычтем FIRST
    firstMap :: Map Symbol (Set Symbol)
    firstMap = computeFirst g

    -- Инициализируем FOLLOW: у стартового нетерминала — {'$'}, у остальных — {}
    initialFollow :: Map Symbol (Set Symbol)
    initialFollow =
      Map.fromList
        [ (lhs, if lhs == startSymbol
                   then Set.singleton (Terminal '$')
                   else Set.empty
          )
        | (lhs, _) <- rules
        ]

    -- Функция одного шага обновления FOLLOW (применяется ко всем правилам)
    step
      :: [Rule]                          -- Правила
      -> Map Symbol (Set Symbol)         -- FIRST-множества
      -> Map Symbol (Set Symbol)         -- Текущее состояние FOLLOW
      -> Map Symbol (Set Symbol)
    step allRules fMap oldFollow =
      foldl' (updateRule fMap) oldFollow allRules

    -- updateRule: применяет правило (lhs -> rhs) к текущему FOLLOW
    updateRule
      :: Map Symbol (Set Symbol)        -- FIRST
      -> Map Symbol (Set Symbol)        -- текущее FOLLOW
      -> Rule                           -- (lhs, rhs)
      -> Map Symbol (Set Symbol)
    updateRule fMap followSoFar (lhs, rhs) =
      foldl' (processSymbol fMap lhs rhs) followSoFar [0.. length rhs - 1]

    -- processSymbol: обрабатывает symbol = rhs[i],
    -- если это NonTerminal, добавляем FIRST(следующие) и при нужде FOLLOW(lhs)
    processSymbol
      :: Map Symbol (Set Symbol)        -- FIRST
      -> Symbol                         -- lhs
      -> [Symbol]                       -- rhs
      -> Map Symbol (Set Symbol)        -- текущее FOLLOW
      -> Int                            -- позиция i
      -> Map Symbol (Set Symbol)
    processSymbol fMap lhs rhs followAcc i =
      case rhs !! i of
        NonTerminal nt ->
          let nextPart = drop (i+1) rhs
              -- FIRST от следующих символов
              firstNext = calcFirstFromRhs nextPart fMap
              withoutEps = Set.delete Epsilon firstNext
              -- Добавляем во FOLLOW(nt) = FIRST(следующих) \ {ε}
              followAcc' = Map.insertWith Set.union (NonTerminal nt) withoutEps followAcc
              -- Если ε ∈ FIRST(следующих), то FOLLOW(nt) += FOLLOW(lhs)
              followOfLhs = Map.findWithDefault Set.empty lhs followAcc
              followAcc'' = if Set.member Epsilon firstNext
                               then Map.insertWith Set.union (NonTerminal nt) followOfLhs followAcc'
                               else followAcc'
          in followAcc''
        _ -> followAcc   -- Terminal/ Epsilon => ничего не делаем

    -- calcFirstFromRhs: берём FIRST цепочки (x:xs)
    -- (та же логика, что в computeFirst)
    calcFirstFromRhs
      :: [Symbol]
      -> Map Symbol (Set Symbol)
      -> Set Symbol
    calcFirstFromRhs [] _ = Set.singleton Epsilon
    calcFirstFromRhs (x:xs) fm =
      case x of
        Terminal t -> Set.singleton (Terminal t)
        NonTerminal nt ->
          let ntFirst = Map.findWithDefault Set.empty (NonTerminal nt) fm
          in if Set.member Epsilon ntFirst
               then Set.union (Set.delete Epsilon ntFirst)
                              (calcFirstFromRhs xs fm)
               else ntFirst
        Epsilon -> Set.singleton Epsilon
