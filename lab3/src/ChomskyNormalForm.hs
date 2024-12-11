{-# LANGUAGE OverloadedStrings #-}
module ChomskyNormalForm (
    toChomskyNormalForm
) where

import Grammar (Grammar(..), Rule, Symbol(..))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe (fromMaybe)

-- Предполагается, что данные модули уже реализованы
import RemoveLongRules (removeLongRules)
import RemoveEpsilonRules (removeEpsilonRules)
import RemoveChainRules (removeChainRules)
import RemoveUselessSymbols (removeUselessSymbols)

-- | Приводит заданную грамматику к нормальной форме Хомского.
-- Шаги:
-- 1) Удаление длинных правил
-- 2) Удаление ε-правил
-- 3) Удаление цепных правил
-- 4) Удаление бесполезных символов
-- 5) Замена терминалов в двусимвольных правилах на нетерминалы
toChomskyNormalForm :: Grammar -> Grammar
toChomskyNormalForm grammar =
    let
        g1 = removeLongRules grammar
        g2 = removeEpsilonRules g1
        g3 = removeChainRules g2
        g4 = removeUselessSymbols g3
        g5 = introduceTerminalNonTerminals g4
    in g5

-- | Функция introduceTerminalNonTerminals:
-- На пятом шаге для каждого правила вида A -> u1 u2, где u_i может быть терминалом,
-- нам необходимо заменить терминалы на нетерминалы, выводящие этот терминал.
-- Например, если есть правило A -> B a, то заменим его на A -> B X_a,
-- где X_a → a добавляется в грамматику.
introduceTerminalNonTerminals :: Grammar -> Grammar
introduceTerminalNonTerminals (Grammar rules) =
    let
        -- Находим все терминалы, появляющиеся в двусимвольных правилах
        terminalRules = [ (lhs, rhs) | (lhs, rhs) <- rules, length rhs == 2 ]
        terminalsInBinary = concatMap (\(_, rhs) -> [ t | t <- rhs, isTerminal t ]) terminalRules

        uniqueTerminals = nub terminalsInBinary

        -- Для каждого терминала создадим новый нетерминал U_T, где T - терминал
        -- Правило: U_T → T
        extraRules = [ (NonTerminal ("X_" ++ [c]), [Terminal c])
                     | Terminal c <- uniqueTerminals ]

        -- Создадим карту терминал -> нетерминал для замены
        terminalMap = Map.fromList [ (Terminal c, NonTerminal ("X_" ++ [c]))
                                   | Terminal c <- uniqueTerminals ]

        -- Применим замену в правилах
        replacedRules = map (replaceTerminalsInBinaryRules terminalMap) rules

        finalRules = nub (replacedRules ++ extraRules)
    in Grammar finalRules

-- | Заменяет терминалы в двусимвольных правилах на соответствующие нетерминалы
replaceTerminalsInBinaryRules :: Map.Map Symbol Symbol -> Rule -> Rule
replaceTerminalsInBinaryRules terminalMap (lhs, rhs)
    | length rhs == 2 =
        let newRhs = map (\sym -> if isTerminal sym
                                  then fromMaybe sym (Map.lookup sym terminalMap)
                                  else sym
                     ) rhs
        in (lhs, newRhs)
    | otherwise = (lhs, rhs)

-- | Проверяет, является ли символ терминалом
isTerminal :: Symbol -> Bool
isTerminal (Terminal _) = True
isTerminal Epsilon      = False
isTerminal (NonTerminal _) = False