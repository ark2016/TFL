module AutomatonFromEquivalenceTable (fromEquivalenceTable) where
{-|
Этот модуль предоставляет функциональность для преобразования таблицы классов
эквивалентности в представление автомата.
-}
import qualified Data.Map as Map
import Automaton (Automaton(..))
import Data.List (nub)

{-|
Преобразует таблицу классов эквивалентности в автомат.

Функция принимает список кортежей, где каждый кортеж содержит имя класса (String)
и целочисленное значение (0 или 1, где 1 указывает на принимающее состояние).

Функция выполняет следующие шаги:
1. Проверяет наличие дублирующихся имен классов
2. Убеждается, что класс 'epsilon' присутствует и помещает его первым в упорядоченную таблицу
3. Создает отображение от имен классов к идентификаторам состояний
4. Инициализирует состояния, алфавит и переходы
5. Определяет начальное состояние и принимающие состояния
6. Конструирует и возвращает автомат

Примечание: Эта функция предполагает базовый алфавит, состоящий только из 'a' для простоты.

@param table Таблица классов эквивалентности в виде списка кортежей (String, Int)
@return Представление автомата на основе таблицы классов эквивалентности
@throws Ошибка, если найдены дублирующиеся имена классов или отсутствует класс 'epsilon'
-}
fromEquivalenceTable :: [(String, Int)] -> Automaton
fromEquivalenceTable table =
    let
        -- Check for duplicate class names
        classNames = map fst table
        uniqueClassNames = nub classNames
        _ = if length classNames /= length uniqueClassNames
            then error "Duplicate class names found"
            else ()

        -- Ordered table ensures 'epsilon' is at the first if present
        orderedTable = case lookup "epsilon" table of
            Just _  -> ("epsilon", fromMaybe 0 (lookup "epsilon" table)) :
                       filter ((/= "epsilon") . fst) table
            Nothing -> error "No epsilon class found in the table"

        -- Create a mapping from class names to state IDs
        classToState = Map.fromList $ zip (map fst orderedTable) [0..]

        -- Initialize states
        statesList = [0..length orderedTable - 1]

        -- Assuming a basic alphabet for simplicity
        alphabetList = ['a']

        -- The initial state is always 0 as we ensured 'epsilon' is first
        initialStateId = 0

        -- Accepting states are those with value 1
        acceptingStatesList = [stateId | (className, val) <- orderedTable, val == 1, Just stateId <- [Map.lookup className classToState]]

        -- Transitions: Create a cyclic transition pattern if more than one state
        transitions = if length orderedTable > 1
                      then Map.fromList [((stateId, 'a'), (stateId + 1) `mod` length orderedTable) | stateId <- statesList]
                      else Map.empty

    in Automaton
        { states = statesList
        , alphabet = alphabetList
        , transitions = transitions
        , initialState = initialStateId
        , acceptingStates = acceptingStatesList
        }

{-|
Вспомогательная функция для предоставления значения по умолчанию для типов Maybe.

@param defval Значение по умолчанию, возвращаемое, если Maybe является Nothing
@param wrapped Значение Maybe для распаковки
@return Распакованное значение, если Just, иначе значение по умолчанию
-}
fromMaybe :: a -> Maybe a -> a
fromMaybe defval wrapped =
    case wrapped of
        Nothing -> defval
        Just value -> value