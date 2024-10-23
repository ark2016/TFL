module Automaton (Automaton(..)) where
import qualified Data.Map as Map
-- | Модуль, предоставляющий структуру данных для автомата.
{-|
Тип данных 'Automaton' представляет конечный автомат.

Конструктор:
@
Automaton { states :: [Int]
          , alphabet :: [Char]
          , transitions :: Map.Map (Int, Char) Int
          , initialState :: Int
          , acceptingStates :: [Int]
          }
@

Поля:
* states: список состояний автомата
* alphabet: алфавит символов, которые может обрабатывать автомат
* transitions: функция переходов, представленная как Map от пары (состояние, символ) к следующему состоянию
* initialState: начальное состояние автомата
* acceptingStates: список финальных состояний

Этот тип данных автоматически реализует типовые классы 'Show' и 'Eq'.
-}
data Automaton = Automaton
    { states :: [Int]
    , alphabet :: [Char]
    , transitions :: Map.Map (Int, Char) Int
    , initialState :: Int
    , acceptingStates :: [Int]
    } deriving (Show, Eq)