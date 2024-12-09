-- RemoveChainRules.hs
module RemoveChainRules (
    removeChainRules
) where

import Grammar (Grammar(..), Rule, Symbol(..))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.List (foldl', nub)

-- | Удаляет цепные правила из грамматики в соответствии со стандартным алгоритмом
removeChainRules :: Grammar -> Grammar
removeChainRules (Grammar rules) =
    let
        -- Шаг 1: Найти все цепные пары (A, B)
        unitPairs = findUnitPairs rules

        -- Шаг 2: Собрать все нецепные правила (B -> α, где α не одиночный нетерминал)
        nonUnitRules = filter (not . isUnitRule) rules

        -- Шаг 3: Создать карту от нетерминала B к его нецепным правилам B -> α
        bToAlphaMap = buildBToAlphaMap nonUnitRules

        -- Шаг 4: Для каждой цепной пары (A, B), добавить все нецепные правила B -> α к A -> α
        additionalRules =
            [ (a, rhs)
            | (a, b) <- Set.toList unitPairs,
              rhs <- Map.findWithDefault [] b bToAlphaMap
            ]

        -- Шаг 5: Собрать итоговые правила, исключив цепные правила и добавив новые правила
        finalRules = nonUnitRules ++ additionalRules

        -- Шаг 6: Удалить дублирующиеся правила, сохраняя порядок
        dedupedRules = nub finalRules
    in Grammar dedupedRules


-- | Проверяет, является ли правило цепным (unit rule)
isUnitRule :: Rule -> Bool
isUnitRule (lhs, [rhs]) = case rhs of
    NonTerminal _ -> True
    _             -> False
isUnitRule _ = False

-- | Строит карту от нетерминала B к его нецепным правилам B -> α
buildBToAlphaMap :: [Rule] -> Map Symbol [[Symbol]]
buildBToAlphaMap nonUnitRules =
    foldl' insertRule Map.empty nonUnitRules
  where
    insertRule acc (lhs, rhs) =
        Map.insertWith (++) lhs [rhs] acc

-- | Находит все цепные пары (A, B), где A ⇒* B через цепные правила
findUnitPairs :: [Rule] -> Set (Symbol, Symbol)
findUnitPairs rules =
    let
        -- Собираем все нетерминалы
        nonTerminals = Set.fromList [ lhs | (lhs, _) <- rules, isNonTerminal lhs ]

        -- Начальное множество: все пары (A, A) для каждого нетерминала A
        initialPairs = Set.fromList [ (nt, nt) | nt <- Set.toList nonTerminals ]

        -- Прямые цепные правила: (A, B) где A -> B
        directUnitRules = Set.fromList [ (lhs, rhs) | (lhs, [rhs]) <- rules, isNonTerminal lhs, isNonTerminal rhs ]

        -- Функция для расширения цепных пар
        expandPairs currentPairs newRules =
            Set.union currentPairs (Set.fromList [ (a, c) | (a, b) <- Set.toList currentPairs, (x, c) <- Set.toList newRules, b == x ])
    in
        -- Итеративно расширяем цепные пары до тех пор, пока не будут найдены все возможные
        closure initialPairs directUnitRules
  where
    closure currentPairs newRules
        | Set.null newRules = currentPairs
        | otherwise =
            let
                newPairs = Set.fromList [ (a, c) | (a, b) <- Set.toList currentPairs, (x, c) <- Set.toList newRules, b == x ]
                uniqueNewPairs = Set.difference newPairs currentPairs
            in
                if Set.null uniqueNewPairs
                then currentPairs
                else closure (Set.union currentPairs uniqueNewPairs) newRules

-- | Проверяет, является ли символ нетерминалом
isNonTerminal :: Symbol -> Bool
isNonTerminal (NonTerminal _) = True
isNonTerminal _               = False
