import System.Random
import Data.List
import qualified Data.Map as Map
import Control.Monad (forM, forever)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import Data.Function (on)

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



-- Проверка пересечения двух автоматов (через декартово произведение)
hasIntersection :: Automaton -> Automaton -> Bool
hasIntersection (Automaton states1 alph1 trans1 init1 accept1)
                (Automaton states2 alph2 trans2 init2 accept2) =
    let combinedAlphabet = nub (alph1 ++ alph2)
        combinedStates = [(s1, s2) | s1 <- states1, s2 <- states2]
        combinedTransitions = Map.fromList
            [(((s1, s2), c), (Map.findWithDefault (-1) (s1, c) trans1,
                              Map.findWithDefault (-1) (s2, c) trans2))
            | s1 <- states1, s2 <- states2, c <- combinedAlphabet]
        combinedAccepting = [(s1, s2) | s1 <- accept1, s2 <- accept2]
        combinedAutomaton = Automaton
            [0..length combinedStates - 1]
            combinedAlphabet
            (Map.fromList
             [((index, c), findIndexOrDefault (nextState1, nextState2) combinedStates)
             | ((state1, state2), c) <- Map.keys combinedTransitions,
               let (nextState1, nextState2) = combinedTransitions Map.! ((state1, state2), c),
               let index = findIndexOrDefault (state1, state2) combinedStates])
            (findIndexOrDefault (init1, init2) combinedStates)
            (map (`findIndexOrDefault` combinedStates) combinedAccepting)

    in accepts combinedAutomaton ""

findIndexOrDefault :: (Eq a) => a -> [a] -> Int
findIndexOrDefault x xs = case elemIndex x xs of
    Just index -> index
    Nothing -> -1

-- Структуры для представления лексем
data Lexeme = EOL | Blank | Equal | Sep | LBracket Int | RBracket Int | Var | Const
    deriving (Show, Eq, Ord)

-- Реализация класса Read для типа Lexeme
instance Read Lexeme where
    readsPrec _ value = case value of
        "EOL" -> [(EOL, "")]
        "Blank" -> [(Blank, "")]
        "Equal" -> [(Equal, "")]
        "Sep" -> [(Sep, "")]
        "Var" -> [(Var, "")]
        "Const" -> [(Const, "")]
        "LBracket1" -> [(LBracket 1, "")]
        "RBracket1" -> [(RBracket 1, "")]
        "LBracket2" -> [(LBracket 2, "")]
        "RBracket2" -> [(RBracket 2, "")]
        "LBracket3" -> [(LBracket 3, "")]
        "RBracket3" -> [(RBracket 3, "")]
        _ -> []

-- Функция для генерации автоматов для каждого типа лексем
generateLexemeAutomata :: IO (Map.Map Lexeme Automaton)
generateLexemeAutomata = do
    eolAutomaton <- generateSpecialAutomaton "eol" "\n"
    blankAutomaton <- generateSpecialAutomaton "blank" " \t"
    equalAutomaton <- generateFiniteAutomaton "equal" "="
    sepAutomaton <- generateFiniteAutomaton "sep" ","
    bracketAutomata <- generateBracketAutomata
    varAutomaton <- generateInfiniteAutomaton "var" "abcdefghijklmnopqrstuvwxyz"
    constAutomaton <- generateInfiniteAutomaton "const" "0123456789"

    return $ Map.fromList [
        (EOL, eolAutomaton),
        (Blank, blankAutomaton),
        (Equal, equalAutomaton),
        (Sep, sepAutomaton),
        (Var, varAutomaton),
        (Const, constAutomaton)
        ] `Map.union` bracketAutomata

-- Функция для генерации специфических автоматов
generateSpecialAutomaton :: String -> [Char] -> IO Automaton
generateSpecialAutomaton _ chars = generateFiniteAutomaton (show chars) chars

generateFiniteAutomaton :: String -> [Char] -> IO Automaton
generateFiniteAutomaton _ chars = do
    let states = [0, 1]
        transitions = Map.fromList [((0, c), 1) | c <- chars]
    return $ Automaton states chars transitions 0 [1]

generateBracketAutomata :: IO (Map.Map Lexeme Automaton)
generateBracketAutomata = do
    lb1 <- generateFiniteAutomaton "LBracket1" "("
    rb1 <- generateFiniteAutomaton "RBracket1" ")"
    lb2 <- generateFiniteAutomaton "LBracket2" "<"
    rb2 <- generateFiniteAutomaton "RBracket2" ">"
    lb3 <- generateFiniteAutomaton "LBracket3" "{"
    rb3 <- generateFiniteAutomaton "RBracket3" "}"
    return $ Map.fromList [
        (LBracket 1, lb1),
        (RBracket 1, rb1),
        (LBracket 2, lb2),
        (RBracket 2, rb2),
        (LBracket 3, lb3),
        (RBracket 3, rb3)
        ]

generateInfiniteAutomaton :: String -> [Char] -> IO Automaton
generateInfiniteAutomaton _ chars = do
    let states = [0, 1]
        transitions = Map.fromList [((0, c), 1) | c <- chars] `Map.union` Map.fromList [((1, c), 1) | c <- chars]
    return $ Automaton states chars transitions 0 [1]

-- Проверка пересечения языков
checkLanguageIntersection :: Map.Map Lexeme Automaton -> Bool
checkLanguageIntersection lexemeAutomata = all (== False) pairs
  where
    pairs = [hasIntersection a1 a2 | (k1, a1) <- automataList, (k2, a2) <- automataList, k1 /= k2]
    automataList = Map.toList lexemeAutomata

-- Проверка соответствия грамматике (упрощенная версия)
-- В вашем случае вам придется заменить это на более сложную проверку
checkGrammar :: String -> Map.Map Lexeme Automaton -> Bool
checkGrammar input automataMap = all (`acceptsAny` automataMap) (words input)

acceptsAny :: String -> Map.Map Lexeme Automaton -> Bool
acceptsAny input automataMap = any (\automaton -> accepts automaton input) (Map.elems automataMap)

-- Обработка запроса на включение
handleInclusionQuery :: Automaton -> String -> String
handleInclusionQuery automaton input =
    if accepts automaton input then "1" else "0"

---- Modified equivalence query handler using minimization
--handleEquivalenceQuery :: Automaton -> [(String, Bool)] -> String
--handleEquivalenceQuery automaton classes =
--    let minimalAutomaton = minimizeAutomaton automaton
--    in case find (\(input, expected) -> accepts minimalAutomaton input /= expected) classes of
--        Just (counterexample, _) -> counterexample
--        Nothing -> "TRUE"
--
---- Function to check if a string is accepted by the automaton
--accepts :: Automaton -> String -> Bool
--accepts (Automaton _ _ transitions initial accepting) input =
--    let finalState = foldl' (\state char -> Map.findWithDefault (-1) (state, char) transitions) initial input
--    in finalState `elem` accepting
-- Проверка принадлежности строки языку автомата
accepts :: Automaton -> String -> Bool
accepts (Automaton _ _ transitions initial accepting) input =
    let finalState = foldl' (\state char -> Map.findWithDefault (-1) (state, char) transitions) initial input
    in finalState `elem` accepting

-- Обработка запроса на эквивалентность (упрощенная версия)
handleEquivalenceQuery :: Automaton -> [(String, Bool)] -> String
handleEquivalenceQuery automaton classes =
   case find (\(input, expected) -> accepts automaton input /= expected) classes of
       Just (counterexample, _) -> counterexample
       Nothing -> "TRUE"

-- Function to minimize a DFA by merging equivalent states
minimizeAutomaton :: Automaton -> Automaton
minimizeAutomaton automaton@(Automaton states alphabet transitions initial acceptingStates) =
    let
        -- Initial partition separates accepting from non-accepting states
        initialPartition = [acceptingStates, states \\ acceptingStates]

        -- Refinement function: split a block based on transitions
        refine :: [[Int]] -> [Int] -> Char -> [[Int]]
        refine partitions block symbol = concatMap splitBlock partitions
          where
            splitBlock b =
                let (inBlock, notInBlock) = partition (`elem` block) (map (`nextState` symbol) b)
                in if null inBlock || null notInBlock then [b] else [inBlock, notInBlock]

            nextState state char = Map.findWithDefault (-1) (state, char) transitions

        -- Iteratively refine partitions until stable
        refinePartitions :: [[Int]] -> [[Int]]
        refinePartitions partitions =
            let refined = foldl (\parts symbol -> concatMap (refine parts (concat parts) symbol) alphabet) partitions alphabet
            in if refined == partitions then partitions else refinePartitions refined

        -- Find the partition block that a state belongs to
        findBlock :: [[Int]] -> Int -> Int
        findBlock partitions state = maybe (-1) head (find (elem state) partitions)

        -- Final partitions after refinement
        finalPartitions = refinePartitions initialPartition

        -- New state mapping based on partitions
        newStateMapping = Map.fromList [(state, findBlock finalPartitions state) | state <- states]

        -- Transition function for minimized DFA
        newTransitions = Map.fromList
            [ ((newStateMapping Map.! state, symbol), newStateMapping Map.! nextState)
            | ((state, symbol), nextState) <- Map.toList transitions
            , nextState /= -1
            ]

        -- New accepting states
        newAcceptingStates = nub $ map (findBlock finalPartitions) acceptingStates

    in Automaton {
        states = nub $ Map.elems newStateMapping,
        alphabet = alphabet,
        transitions = newTransitions,
        initialState = newStateMapping Map.! initial,
        acceptingStates = newAcceptingStates
    }

-- Выводит информацию об автомате в простом графическом формате (Graphviz/DOT)
visualizeAutomaton :: Automaton -> IO ()
visualizeAutomaton (Automaton states alph transitions initial accepting) = do
   putStrLn "Automaton:"
   putStrLn $ "States: " ++ show states
   putStrLn $ "Alphabet: " ++ show alph
   putStrLn $ "Initial state: " ++ show initial
   putStrLn $ "Accepting states: " ++ show accepting
   putStrLn "Transitions:"
   putStrLn "digraph {"
   putStrLn $ "  \"start\" [shape=point];"
   putStrLn $ "  \"start\" -> " ++ show initial ++ ";"
   mapM_ (\x -> putStrLn $ "  " ++ show x ++ " [shape=doublecircle];") accepting
   mapM_ (\((from, char), to) -> putStrLn $ "  " ++ show from ++ " -> " ++ show to ++ " [label = \"" ++ [char] ++ "\"]")
         (Map.toList transitions)
   putStrLn "}"

-- Основная функция MAT
mat :: IO ()
mat = do
   lexemeAutomata <- generateLexemeAutomata

   if not (checkLanguageIntersection lexemeAutomata)
       then error "Language intersection detected"
       else putStrLn "Automata generated successfully"

   -- Генерация случайного автомата для примеров запросов INCLUSION и EQUIVALENCE
   let alphabet = "abc012"
   randomAutomaton <- generateRandomAutomaton 10 alphabet

   -- Обработка запросов
   forever $ do
       query <- getLine
       case words query of
          ["INCLUSION", automatonIndex, input] -> do
              let automatonList = Map.toList lexemeAutomata
              if all isDigit automatonIndex && (read automatonIndex :: Int) < length automatonList then do
                  let (lexeme, automaton) = automatonList !! (read automatonIndex :: Int)
                  putStrLn $ handleInclusionQuery automaton input
              else putStrLn "Invalid automaton index"

          ["EQUIVALENCE", automatonIndex, examples] -> do
              let automatonList = Map.toList lexemeAutomata
              if all isDigit automatonIndex && (read automatonIndex :: Int) < length automatonList then do
                  let (lexeme, automaton) = automatonList !! (read automatonIndex :: Int)
                  let testCases = parseExamples examples
                  putStrLn $ handleEquivalenceQuery automaton testCases
              else putStrLn "Invalid automaton index"

          _ -> putStrLn "Unknown query. Use 'INCLUSION <automatonIndex> <input>' or 'EQUIVALENCE <automatonIndex> <examples>'"
    where
      parseExamples :: String -> [(String, Bool)]
      parseExamples = map parseExample . words

      parseExample :: String -> (String, Bool)
      parseExample s =
        let (input, result) = break (== ':') s
        in (input, read result == "True")

main :: IO ()
main = mat