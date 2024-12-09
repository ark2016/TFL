-- RemoveUselessSymbols.hs
module RemoveUselessSymbols (
    removeUselessSymbols
) where

import Grammar (Grammar(..), Rule, Symbol(..))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.List (foldl', nub)

-- | Удаляет бесполезные символы из грамматики
removeUselessSymbols :: Grammar -> Grammar
removeUselessSymbols (Grammar rules) =
    let
        -- Шаг 1: Найти все порождающие нетерминалы
        generating = findGeneratingSymbols rules

        -- Шаг 2: Удалить правила, содержащие непорождающие нетерминалы
        rulesWithoutNonGenerating = filter (ruleIsGenerating generating) rules

        -- Шаг 3: Найти все достижимые символы начиная со стартового символа
        startSymbol = case rules of
                        ((NonTerminal s, _):_) -> NonTerminal s
                        _ -> error "Empty grammar"
        reachable = findReachableSymbols rulesWithoutNonGenerating startSymbol

        -- Шаг 4: Удалить правила, содержащие недостижимые нетерминалы
        finalRules = filter (ruleIsReachable reachable) rulesWithoutNonGenerating

        -- Шаг 5: Удалить дублирующиеся правила, сохраняя порядок
        dedupedRules = nub finalRules
    in Grammar dedupedRules

-- | Проверяет, является ли правило порождающим
ruleIsGenerating :: Set Symbol -> Rule -> Bool
ruleIsGenerating generating (lhs, rhs) =
    Set.member lhs generating && all (\sym -> isSymbolGenerating sym generating) rhs

-- | Проверяет, является ли символ порождающим
isSymbolGenerating :: Symbol -> Set Symbol -> Bool
isSymbolGenerating (Terminal _) _ = True
isSymbolGenerating Epsilon _ = True
isSymbolGenerating (NonTerminal s) generating = Set.member (NonTerminal s) generating

-- | Находит все порождающие нетерминалы в грамматике
findGeneratingSymbols :: [Rule] -> Set Symbol
findGeneratingSymbols rules = go initialGenerating
  where
    -- Начальное множество: нетерминалы, у которых есть правило с RHS из терминалов и/или ε
    initialGenerating = Set.fromList [ lhs
                                      | (lhs, rhs) <- rules
                                      , all isTerminalOrEpsilon rhs
                                      ]

    go generating =
        let
            newGenerating = Set.union generating (Set.fromList [ lhs
                                                                 | (lhs, rhs) <- rules
                                                                 , not (Set.member lhs generating)
                                                                 , all (`Set.member` generating) rhs
                                                                 ])
        in
            if Set.size newGenerating == Set.size generating
            then generating
            else go newGenerating

    isTerminalOrEpsilon :: Symbol -> Bool
    isTerminalOrEpsilon (Terminal _) = True
    isTerminalOrEpsilon Epsilon      = True
    isTerminalOrEpsilon _            = False

-- | Проверяет, является ли правило достижимым
ruleIsReachable :: Set Symbol -> Rule -> Bool
ruleIsReachable reachable (lhs, rhs) =
    Set.member lhs reachable && all (symbolIsReachable reachable) rhs

-- | Проверяет, является ли символ достижимым
symbolIsReachable :: Set Symbol -> Symbol -> Bool
symbolIsReachable _ (Terminal _) = True
symbolIsReachable _ Epsilon     = True
symbolIsReachable reachable (NonTerminal s) = Set.member (NonTerminal s) reachable

-- | Находит все достижимые символы, начиная со стартового символа
findReachableSymbols :: [Rule] -> Symbol -> Set Symbol
findReachableSymbols rules start =
    go (Set.singleton start) (Set.singleton start)
  where
    ruleMap = buildRuleMap rules

    go reachable frontier
        | Set.null frontier = reachable
        | otherwise =
            let
                -- Для всех символов во frontier, собираем все символы из их правил
                symbolsToAdd = Set.unions [ Set.fromList rhs
                                         | sym <- Set.toList frontier
                                         , rhs <- Map.findWithDefault [] sym ruleMap ]
                -- Отфильтровываем только нетерминалы, чтобы не добавлять терминалы в reachable
                nonTerminalsToAdd = Set.filter isNonTerminal symbolsToAdd
                newSymbols = Set.difference nonTerminalsToAdd reachable
                newReachable = Set.union reachable newSymbols
                newFrontier = newSymbols
            in go newReachable newFrontier

    buildRuleMap :: [Rule] -> Map Symbol [[Symbol]]
    buildRuleMap = foldl' insertRule Map.empty
      where
        insertRule acc (lhs, rhs) =
            Map.insertWith (++) lhs [rhs] acc

    isNonTerminal :: Symbol -> Bool
    isNonTerminal (NonTerminal _) = True
    isNonTerminal _               = False
