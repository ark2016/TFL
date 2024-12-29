{-# LANGUAGE OverloadedStrings #-}

module RemoveLeftRecursion (
    removeLeftRecursion
) where

import Grammar
import Data.List (nub, partition)
import Data.Maybe (fromMaybe)

-- | Основная функция removeLeftRecursion
removeLeftRecursion :: Grammar -> Grammar
removeLeftRecursion (Grammar rules) =
    let
        -- Извлечём список нетерминалов в порядке их появления
        nonTerminals = extractNonTerminals rules
        -- Удалим дубликаты, сохраним порядок
        uniqueNTs = nub nonTerminals


        ruleMap = buildRuleMap rules

        -- Применим алгоритм удаления левой рекурсии
        finalRuleMap = removeLeftRecursionMap uniqueNTs ruleMap

        -- Превратим Map обратно в список правил
        finalRules = mapFromRuleMap finalRuleMap
    in
        Grammar (nub finalRules) -- Удалим дублирующиеся правила


-- | Удаление левой рекурсии (общий алгоритм) для Map вида A -> [[Symbol]]


-- | removeLeftRecursionMap упорядочивает нетерминалы [A1..An],
--   для i в [1..n]:
--     для каждого j в [1..i-1]:
--       заменяет Ai -> Aj α на Ai -> δ α для каждого Aj -> δ
--     устраняет непосредственную левую рекурсию для Ai
removeLeftRecursionMap
    :: [String]                    -- ^ упорядоченный список нетерминалов (["S","A","B",...])
    -> RuleMap                     -- ^ Map: "S" -> [[Symbol]]
    -> RuleMap
removeLeftRecursionMap nts ruleMap = go nts ruleMap []
  where
    go [] ruleMap _ = ruleMap
    go (ai:rest) ruleMap processed =
        -- Шаг a: для каждого Aj из processed, заменяем Ai -> Aj γ на Ai -> δ γ для всех Aj -> δ
        let
            ruleMapAfterSubst = foldl (\acc aj -> replaceAjInAi ai aj acc) ruleMap processed
            -- Шаг b: удаляем непосредственную левую рекурсию из Ai
            ruleMapAfterLR = removeImmediateLeftRec ai ruleMapAfterSubst
        in
            -- Добавляем Ai в список обработанных и продолжаем
            go rest ruleMapAfterLR (processed ++ [ai])

    -- | replaceAjInAi заменяет все правила Ai -> Aj γ на Ai -> δ γ для каждого Aj -> δ
    replaceAjInAi :: String -> String -> RuleMap -> RuleMap
    replaceAjInAi ai aj ruleMap =
        case lookup ai ruleMap of
          Nothing -> ruleMap
          Just aiRHS ->
            let
                -- Разделяем правила Ai на те, что начинаются с Aj и остальные
                (toReplace, toKeep) = partition (\rhs -> case rhs of
                                                          (NonTerminal x : _) | x == aj -> True
                                                          _ -> False
                                           ) aiRHS
                -- Получаем все правые части Aj
                ajProductions = fromMaybe [] (lookup aj ruleMap)
                -- Для каждого Ai -> Aj γ заменяем на Ai -> δ γ для всех Aj -> δ
                newRules = concatMap (\gamma -> map (++ gamma) ajProductions ) toReplace
                -- Новые правые части Ai
                newAiRHS = toKeep ++ newRules
            in
                updateRuleMap ai newAiRHS ruleMap

-- | Удаление непосредственной левой рекурсии для Ai

-- | removeImmediateLeftRec устраняет непосредственную левую рекурсию для Ai
removeImmediateLeftRec :: String -> RuleMap -> RuleMap
removeImmediateLeftRec ai mp =
    case lookup ai mp of
      Nothing -> mp
      Just allRHS ->
        let
          -- Разделяем правила Ai на леворекурсивные и нетерминаловые
          (alphaParts, betaParts) =
              splitImmediateLR ai allRHS
        in
          if null alphaParts
            then
              -- Нет непосредственной левой рекурсии
              mp
            else
              let
                -- Создаём новый нетерминал Ai'
                aiPrime = ai ++ "'"

                -- Если есть нетерминальные правые части (betaParts), добавляем Ai -> β Ai'
                -- Иначе, если нет, добавляем только Ai -> Ai'
                newA = if null betaParts
                       then [ [NonTerminal aiPrime] ]
                       else [ beta ++ [NonTerminal aiPrime] | beta <- betaParts ]

                -- Ai' -> α Ai' | α
                newAPrime =
                  [ alphaTail ++ [NonTerminal aiPrime] | alphaTail <- alphaParts ]
                  ++ alphaParts

                -- Обновляем правила для Ai и Ai'
                mp1 = updateRuleMap ai    newA     mp
                mp2 = updateRuleMap aiPrime newAPrime mp1
              in mp2

-- | splitImmediateLR разбивает правила Ai на леворекурсивные и нет
splitImmediateLR :: String -> [[Symbol]] -> ([[Symbol]], [[Symbol]])
splitImmediateLR ai allRHS =
    let (alphas, betas) = foldr f ([],[]) allRHS
        f rhs (accA, accB) =
          case rhs of
            (NonTerminal x : xs) | x == ai -> (xs : accA, accB)
            _                              -> (accA, rhs : accB)
    in (alphas, betas)

-- 4. Простые инструменты RuleMap = [(String, [[Symbol]])]
--    и функции buildRuleMap / updateRuleMap / mapFromRuleMap

type RuleMap = [(String, [[Symbol]])]

-- | buildRuleMap строит Map из списка правил
buildRuleMap :: [Rule] -> RuleMap
buildRuleMap rs =
    let grouped = gather rs
    in grouped
  where
    gather :: [Rule] -> [(String, [[Symbol]])]
    gather [] = []
    gather ((NonTerminal lhs, rhs):xs) =
        let (sameLHS, rest) = span (\(NonTerminal l, _) -> l == lhs) xs
            currentRHS = rhs : map snd sameLHS
            newMapEl   = (lhs, currentRHS)
            rest'      = filter (\(NonTerminal l,_) -> l /= lhs) rest
        in newMapEl : gather rest'
    gather ((_,_):xs) = gather xs  -- Игнорируем правила с lhs не-нетерминалом

-- | updateRuleMap обновляет правила для заданного нетерминала
updateRuleMap :: String -> [[Symbol]] -> RuleMap -> RuleMap
updateRuleMap lhs newRHS [] = [(lhs, newRHS)]
updateRuleMap lhs newRHS ((x, rhsList):rest)
    | x == lhs  = (x, nub newRHS) : rest
    | otherwise = (x, rhsList) : updateRuleMap lhs newRHS rest

-- | mapFromRuleMap превращает RuleMap обратно в список правил
mapFromRuleMap :: RuleMap -> [Rule]
mapFromRuleMap mp =
    concatMap (\(lhs, rhss) ->
        [ (NonTerminal lhs, rhs) | rhs <- rhss ]
      ) mp


-- | Вспомогательные функции для извлечения списка нетерминалов

-- | extractNonTerminals извлекает список нетерминалов из правил
extractNonTerminals :: [Rule] -> [String]
extractNonTerminals = foldr f []
  where
    f (NonTerminal lhs, _) acc = lhs : acc
    f _ acc = acc
