import System.Random
import Data.List
import qualified Data.Map as Map
import Control.Monad (forM, forever)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)

-- Структура для представления автомата
data Automaton = Automaton {
    states :: [Int],
    alphabet :: [Char],
    transitions :: Map.Map (Int, Char) Int,
    initialState :: Int,
    acceptingStates :: [Int]
} deriving (Show)

-- Генерация случайного автомата
generateRandomAutomaton :: Int -> [Char] -> IO Automaton
generateRandomAutomaton maxStates alph = do
    numStates <- randomRIO (1, maxStates)
    let states = [0..(numStates-1)]
    initialState <- randomRIO (0, numStates-1)
    numAccepting <- randomRIO (1, numStates)
    acceptingStates <- take numAccepting <$> shuffle states
    transitions <- generateRandomTransitions states alph
    return $ Automaton states alph transitions initialState acceptingStates

-- Генерация случайных переходов
generateRandomTransitions :: [Int] -> [Char] -> IO (Map.Map (Int, Char) Int)
generateRandomTransitions states alph = do
    let pairs = [(s, c) | s <- states, c <- alph]
    transitions <- mapM (\p -> do
        nextState <- randomChoice states
        return (p, nextState)) pairs
    return $ Map.fromList transitions

-- Случайный выбор элемента из списка
randomChoice :: [a] -> IO a
randomChoice xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- Перемешивание списка
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i, n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1, n) xs

-- Проверка принадлежности строки языку автомата
accepts :: Automaton -> String -> Bool
accepts (Automaton _ _ transitions initial accepting) input =
    let finalState = foldl' (\state char -> Map.findWithDefault (-1) (state, char) transitions) initial input
    in finalState `elem` accepting

-- Обработка запроса на включение
handleInclusionQuery :: Automaton -> String -> String
handleInclusionQuery automaton input =
    if accepts automaton input then "1" else "0"

-- Обработка запроса на эквивалентность (упрощенная версия)
handleEquivalenceQuery :: Automaton -> [(String, Bool)] -> String
handleEquivalenceQuery automaton classes =
    case find (\(input, expected) -> accepts automaton input /= expected) classes of
        Just (counterexample, _) -> counterexample
        Nothing -> "TRUE"

-- Визуализация автомата (простой вывод в консоль)
visualizeAutomaton :: Automaton -> IO ()
visualizeAutomaton (Automaton states alph transitions initial accepting) = do
    putStrLn "Automaton:"
    putStrLn $ "States: " ++ show states
    putStrLn $ "Alphabet: " ++ show alph
    putStrLn $ "Initial state: " ++ show initial
    putStrLn $ "Accepting states: " ++ show accepting
    putStrLn "Transitions:"
    mapM_ (\((from, char), to) -> putStrLn $ show from ++ " --" ++ [char] ++ "--> " ++ show to) (Map.toList transitions)

-- Основная функция MAT
mat :: IO ()
mat = do
    let alphabet = "abc012"
    automaton <- generateRandomAutomaton 10 alphabet

    -- Обработка запросов
    forever $ do
        query <- getLine
        case words query of
            ["INCLUSION", input] -> putStrLn $ handleInclusionQuery automaton input
            "EQUIVALENCE" : rest -> do
                let classes = read (unwords rest) :: [(String, Bool)]
                putStrLn $ handleEquivalenceQuery automaton classes
            ["VISUALIZE"] -> visualizeAutomaton automaton
            _ -> putStrLn "Unknown query"

main :: IO ()
main = mat