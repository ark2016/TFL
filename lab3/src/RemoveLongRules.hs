module RemoveLongRules (
    removeLongRules
) where

import Grammar (Grammar(..), Rule, Symbol(..))
import Data.List (intercalate)

-- | Удаляет длинные правила из грамматики, преобразуя её в эквивалентную грамматику без длинных правил
removeLongRules :: Grammar -> Grammar
removeLongRules (Grammar rules) = Grammar (concatMap processRule rules)
  where
    -- Обрабатывает одно правило: если оно длинное, разбивает его, иначе оставляет без изменений
    processRule :: Rule -> [Rule]
    processRule (left, right)
        | length right > 2 = splitLongRule left right
        | otherwise        = [(left, right)]

    -- Разбивает длинное правило A -> a1 a2 ... ak на цепочку правил без длинных
    splitLongRule :: Symbol -> [Symbol] -> [Rule]
    splitLongRule left right =
        let k = length right
            prefix = case left of
                        NonTerminal s -> s ++ "B"
                        _             -> "B"
            -- Генерируем новые нетерминалы: B1, B2, ..., B(k-2)
            newNonTerminals = [NonTerminal (prefix ++ show i) | i <- [1..(k-2)]]
            -- Первое правило: A -> a1 B1
            firstRule = (left, [right !! 0, head newNonTerminals])
            -- Средние правила: B1 -> a2 B2, B2 -> a3 B3, ..., B(k-3) -> a(k-2) B(k-2)
            middleRules = [ (newNonTerminals !! i, [right !! (i + 1), newNonTerminals !! (i + 1)])
                          | i <- [0..(length newNonTerminals - 2)] ]
            -- Последнее правило: B(k-2) -> a(k-1) ak
            lastRule = (last newNonTerminals, [right !! (k - 2), right !! (k - 1)])
        in [firstRule] ++ middleRules ++ [lastRule]
