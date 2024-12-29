{-# OPTIONS_GHC -Wall #-}

module Grammar (
    Grammar(..),
    Rule,
    Symbol(..),
    parseGrammar,
    printGrammar,
    parseRule,
    parseSymbol
) where

import Data.Char (isAlphaNum, isLower, isUpper, isSpace)
import Data.List (intercalate)
import Control.Monad (guard)

----------------------------------------------------------------
-- 1. Типы данных
----------------------------------------------------------------

-- | Символ грамматики: Терминал, Нетерминал или Epsilon (ε).
data Symbol
    = Terminal Char
    | NonTerminal String
    | Epsilon
    deriving (Eq, Show, Ord)

-- | Правило: левая часть + список символов в правой части.
type Rule = (Symbol, [Symbol])

-- | Грамматика — просто список правил.
newtype Grammar = Grammar [Rule]
    deriving (Eq, Show)

----------------------------------------------------------------
-- 2. Основные функции парсинга
----------------------------------------------------------------

-- | Парсинг всей грамматики из многострочной строки.
--
-- Каждая строка должна представлять одно правило вида:
--   LHS -> RHS
-- где LHS — ровно один нетерминал (например, "S"),
-- а RHS — последовательность из нетерминалов и терминалов (включая "ε").
parseGrammar :: String -> Maybe Grammar
parseGrammar s = do
    let linesOfGrammar = lines s -- Разбиваем на строки
    rules <- mapM parseRule linesOfGrammar
    return (Grammar rules)

-- | Парсинг одной строки в правило (LHS -> RHS).
parseRule :: String -> Maybe Rule
parseRule line = do
    -- 1. Уберём лидирующие/конечные пробелы
    let trimmed = stripSpaces line

    -- 2. Разбиваем по "->"
    (lhsStr, rhsStr) <- splitOnArrow trimmed
    let lhsTrim = stripSpaces lhsStr
    let rhsTrim = stripSpaces rhsStr

    -- 3. LHS: ровно один нетерминал
    leftSymbol <- parseNonTerminal lhsTrim

    -- 4. RHS: возможно несколько символов (нетерминал, терминал, либо Epsilon).
    --    Учитывая, что между ними могут отсутствовать пробелы,
    --    сделаем «токенизацию» по шаблону (см. tokenizeSymbols).
    let tokens = tokenizeSymbols rhsTrim
    guard (not (null tokens))             -- Правый блок не может быть пустым
    rightSymbols <- mapM parseSymbol tokens

    return (leftSymbol, rightSymbols)

----------------------------------------------------------------
-- 3. Парсинг «токенов» и отдельных символов
----------------------------------------------------------------

-- | Парсинг произвольного символа (терминал, нетерминал, либо "ε").
parseSymbol :: String -> Maybe Symbol
parseSymbol s
    | s == "ε" = Just Epsilon
    -- ТЕРМИНАЛ: по условию [a-z], ровно один символ
    | length s == 1 && isLower (head s) =
        Just (Terminal (head s))
    -- НЕТЕРМИНАЛ:
    --   - начинается с заглавной буквы
    --   - может содержать только буквы и цифры
    --   - минимум один символ
    | isNonTerminalPattern s =
        Just (NonTerminal s)
    | otherwise = Nothing

-- | Парсинг ровно одного нетерминала (для левой части правила).
parseNonTerminal :: String -> Maybe Symbol
parseNonTerminal s
    | isNonTerminalPattern s = Just (NonTerminal s)
    | otherwise              = Nothing

-- | Проверка строки на «нетерминальный» шаблон согласно ТЗ:
--   [A-Z][0-9]? | [[A-z]+([0-9])∗]
-- Упрощённо: первая буква — заглавная буква A-Z,
-- далее — произвольное сочетание букв и цифр.
isNonTerminalPattern :: String -> Bool
isNonTerminalPattern "" = False
isNonTerminalPattern (c:cs) =
    isUpper c &&                          -- первый символ должен быть заглавной буквой
    all (\x -> isAlphaNum x) cs          -- последующие могут быть только буквами и цифрами

----------------------------------------------------------------
-- 4. Вспомогательные инструменты (splitOnArrow, tokenizeSymbols, etc.)
----------------------------------------------------------------

-- | Удаляет лидирующие и конечные пробелы в строке.
stripSpaces :: String -> String
stripSpaces = f . f
  where f = reverse . dropWhile isSpace

-- | Делит строку на (левая часть, правая часть), используя первое вхождение "->".
--   Возвращает Nothing, если "->" не найдено.
splitOnArrow :: String -> Maybe (String, String)
splitOnArrow str =
    case breakSubstring "->" str of
        Just (lhs, rhs) -> Just (lhs, rhs)
        Nothing         -> Nothing

-- | Ищет первое вхождение подстроки "->" и возвращает (до, после).
--   Если не найдено — Nothing.
breakSubstring :: String -> String -> Maybe (String, String)
breakSubstring pat txt =
    case go txt "" of
      Nothing -> Nothing
      Just (pre, post) -> Just (pre, post)
  where
    plen = length pat

    go :: String -> String -> Maybe (String, String)
    go [] _ = Nothing
    go xs acc
      | pat `isPrefixOf` xs =
          let after = drop plen xs
          in Just (acc, after)
      | otherwise =
          go (tail xs) (acc ++ [head xs])

    isPrefixOf :: String -> String -> Bool
    isPrefixOf p s = take (length p) s == p

-- | Разбивает RHS на «токены» (каждый — либо нетерминал, либо терминал, либо "ε").
--   Пример: "ABC aB ε"  -> ["ABC", "aB", "ε"]
--   Пример: "SAbcd" (если всё склеено) -> например, ["S", "A", "b", "c", "d"] (в зависимости от логики разбора).
--
--   Здесь реализован простой «жадный» разбор:
--     1. Если встретили "ε" — это отдельный токен.
--     2. Если встретили строчную букву [a-z] — это одиночный терминал (один символ).
--     3. Иначе «кушаем» подряд [A-Z0-9]+ (пока не встретимся на строчную букву или пробел/конец).
--
--   При необходимости можно усложнять логику (регулярками и т.д.).
tokenizeSymbols :: String -> [String]
tokenizeSymbols = go . dropWhile isSpace
  where
    go "" = []
    go s
      -- 1) Если начинается на «ε», считаем это отдельным токеном
      | prefix "ε" s =
          "ε" : go (drop 1 s)

      -- 2) Если это одиночная [a-z], считаем это терминалом
      | isLower (head s) =
          [head s] : go (tail s)

      -- 3) Иначе «кушаем» подряд [A-Z0-9]+
      | isUpper (head s) =
          let (chunk, rest) = span isAlphaNum s
          in chunk : go rest

      -- 4) Пропускаем пробелы и любые другие символы (например, лишние)
      | otherwise =
          go (tail s)

    prefix p ss = take (length p) ss == p

----------------------------------------------------------------
-- 5. Печать грамматики
----------------------------------------------------------------

-- | Функция для текстового отображения грамматики (для отладки).
printGrammar :: Grammar -> String
printGrammar (Grammar rules) =
    intercalate "\n" (map printRule rules)
  where
    printRule (left, right) =
        printSymbol left ++ " -> " ++ unwords (map printSymbol right)

    printSymbol :: Symbol -> String
    printSymbol (Terminal c)   = [c]
    printSymbol (NonTerminal nt) = nt
    printSymbol Epsilon        = "ε"
