module Automaton (Automaton(..)) where
import qualified Data.Map as Map

-- Automaton data structure
data Automaton = Automaton
    { states :: [Int]
    , alphabet :: [Char]
    , transitions :: Map.Map (Int, Char) Int
    , initialState :: Int
    , acceptingStates :: [Int]
    } deriving (Show, Eq)