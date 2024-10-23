{-|
Этот модуль предоставляет функциональность для симуляции работы детерминированного
конечного автомата (ДКА) на заданной входной строке.
-}
module AutomatonInclusion where

import qualified Data.Map as Map
import Automaton (Automaton(..))

{-|
Имитирует работу автомата на входной строке и возвращает результат.

@
isStringAccepted :: Automaton -> String -> Int
@

Параметры:
  * automaton - автомат, на котором производится симуляция
  * input - входная строка

Возвращаемое значение:
  * 1, если строка принимается автоматом
  * 0, если строка не принимается автоматом

Алгоритм:
1. Начинает с начального состояния автомата.
2. Последовательно применяет функцию перехода для каждого символа входной строки.
3. Проверяет, является ли конечное состояние финальным.

Примечание: Если в процессе симуляции встречается отсутствующий переход,
автомат переходит в недопустимое состояние (-1) и "отвергает" строку.
-}
isStringAccepted :: Automaton -> String -> Int
isStringAccepted automaton input =
    let finalState = foldl (transition automaton) (initialState automaton) input
    in if finalState `elem` (acceptingStates automaton) && finalState /= -1
       then 1
       else 0

{-|
Вспомогательная функция для перехода между состояниями на основе текущего состояния и входного символа.

@
transition :: Automaton -> Int -> Char -> Int
@

Параметры:
  * automaton - автомат
  * currentState - текущее состояние
  * char - входной символ

Возвращаемое значение:
  * Следующее состояние автомата, если переход определен
  * -1, если переход не определен (недопустимое состояние)

Примечание: Использование -1 как недопустимого состояния позволяет
обрабатывать случаи, когда переход не определен в автомате.
-}
transition :: Automaton -> Int -> Char -> Int
transition automaton currentState char =
    case Map.lookup (currentState, char) (transitions automaton) of
        Just nextState -> nextState
        Nothing -> -1  -- Use -1 as an invalid state to represent a lack of transition

{-|
Примеры использования:

1. Проверка принятия строки автоматом:
@
let dfa = Automaton {states = [0, 1],
                     alphabet = ['a', 'b'],
                     transitions = Map.fromList [((0, 'a'), 1), ((0, 'b'), 0), ((1, 'a'), 1), ((1, 'b'), 1)],
                     initialState = 0,
                     acceptingStates = [1]}
isStringAccepted dfa "ab"  -- Должно вернуть 1
isStringAccepted dfa "b"   -- Должно вернуть 0
@

2. Проверка строки с недопустимым переходом:
@
isStringAccepted dfa "abc"  -- Должно вернуть 0, так как 'c' не определен в алфавите
@
-}