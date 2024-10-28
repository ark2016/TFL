module Automaton (Automaton(..)) where
import qualified Data.Map as Map
import Text.Read (Read(..), readPrec)
import Text.ParserCombinators.ReadP (ReadP, string, readP_to_S, readS_to_P)
import Control.Applicative ((<|>))
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

-- Реализация инстанса Read для Automaton
instance Read Automaton where
    readsPrec _ = readP_to_S parseAutomaton

-- Парсер для автомата
parseAutomaton :: ReadP Automaton
parseAutomaton = do
    _ <- string "Automaton {"
    _ <- string "states = "
    states <- readS_to_P reads
    _ <- string ", alphabet = "
    alphabet <- readS_to_P reads
    _ <- string ", transitions = fromList "
    transitions <- readS_to_P reads
    _ <- string ", initialState = "
    initialState <- readS_to_P reads
    _ <- string ", acceptingStates = "
    acceptingStates <- readS_to_P reads
    _ <- string "}"
    return Automaton
        { states = states
        , alphabet = alphabet
        , transitions = Map.fromList transitions
        , initialState = initialState
        , acceptingStates = acceptingStates
        }