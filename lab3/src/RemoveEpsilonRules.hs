module RemoveEpsilonRules (
    removeEpsilonRules
) where

import Grammar (Grammar(..), Rule, Symbol(..))
import qualified Data.Set as Set
import Data.List (nub)

-- | Удаляет ε-правила из грамматики в соответствии со стандартным алгоритмом
removeEpsilonRules :: Grammar -> Grammar
removeEpsilonRules (Grammar rules) =
    let
        originalRules = rules
        nullable = findNullable originalRules
        expandedRules = concatMap (generateNewRules nullable) originalRules

        startSymbol = case originalRules of
                        ((NonTerminal s, _):_) -> s
                        _ -> error "Empty grammar"

        startNullable = Set.member startSymbol nullable

        -- Фильтрация: удаляем все правила с Epsilon, кроме S → ε если S был помечен как nullable
        noEpsRules = filter (keepRule startNullable startSymbol) expandedRules

        -- Добавляем S → ε только если S было nullable и S → ε ещё не существует
        hasS_Epsilon = any (\(lhs, rhs) -> lhs == NonTerminal startSymbol && rhs == [Epsilon]) originalRules
        finalRules =
            if startNullable && not hasS_Epsilon
            then (NonTerminal startSymbol, [Epsilon]) : noEpsRules
            else noEpsRules

        -- Удаляем дублирующиеся правила
        dedupedRules = nub finalRules
    in Grammar dedupedRules

-- | Оставляем правило, если:
-- 1. В нём нет Epsilon вообще, или
-- 2. Это правило S→ε (если S был помечен как nullable)
keepRule :: Bool -> String -> Rule -> Bool
keepRule startNullable startSymbol (lhs, rhs) =
    let hasEps = Epsilon `elem` rhs
        isStartSymbolEps = (lhs == NonTerminal startSymbol) && (rhs == [Epsilon]) && startNullable
    in (not hasEps) || isStartSymbolEps

-- | Находит все ε-порождающие нетерминалы в грамматике
findNullable :: [Rule] -> Set.Set String
findNullable rules = go initialNullable
  where
    -- Начальное множество: нетерминалы с правилами A→ε
    initialNullable = Set.fromList [ s | (NonTerminal s, [Epsilon]) <- rules ]

    go nullable =
        let newNullable = Set.union nullable (Set.fromList
                              [ s
                              | (NonTerminal s, rhs) <- rules
                              , not (Set.member s nullable)
                              , all (isNullable nullable) rhs
                              ])
        in if Set.size newNullable == Set.size nullable
           then nullable
           else go newNullable

-- | Проверка, является ли символ nullable
isNullable :: Set.Set String -> Symbol -> Bool
isNullable nullable (NonTerminal s) = Set.member s nullable
isNullable _        Epsilon         = True
isNullable _        _               = False

-- | Генерирует новые правила, исключая nullable символы.
-- По алгоритму мы должны взять каждое правило A→α0B1α1…Bkαk,
-- где Bj nullable, и добавить все варианты, где каждый Bj либо присутствует, либо удалён.
generateNewRules :: Set.Set String -> Rule -> [Rule]
generateNewRules nullable (left, rhs) =
    let
        -- Находим nullable символы в правой части
        nullableIndices = [ i | (i, sym) <- zip [0..] rhs, isNullable nullable sym ]
        -- Генерируем все подмножества nullable индексов (включая пустое для оригинального правила)
        subsets = powerset nullableIndices
        -- Для каждого подмножества удаляем соответствующие символы
        newRhsList = [ removeIndices rhs subset | subset <- subsets ]
        -- Исключаем правила с пустой правой частью (A → ε будет удалено позже)
        finalRules = [ (left, newRhs) | newRhs <- newRhsList, not (null newRhs) ]
    in finalRules

-- | Удаляет элементы из списка по индексам
removeIndices :: [a] -> [Int] -> [a]
removeIndices xs indices = [ x | (x, i) <- zip xs [0..], not (i `elem` indices) ]

-- | Генерирует все подмножества списка
powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) =
    let ps = powerset xs
    in ps ++ map (x:) ps
