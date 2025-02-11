module Regex.Parser(
  parseRegex
) where

import Control.Monad (void)
import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.String (Parser)

import Regex.AST

--------------------------------------------------------------------------------
-- Вспомогательные парсеры

-- | Парсим один символ из диапазона [a-z]
pChar :: Parser Regex
pChar = do
  c <- oneOf ['a'..'z']
  return (RChar c)

-- | Парсим ссылку на группу вида (?1), (?2), ... (?9)
--   Предполагаем, что номер группы — это одна цифра 1..9
pGroupRef :: Parser Regex
pGroupRef = try $ do
  _ <- string "(?"
  d <- oneOf ['1'..'9'] -- ограничиваемся одной цифрой
  _ <- char ')'
  return (RRef (digitToInt d))

-- | Парсим look-ahead: (?= expr)
pLookAhead :: Parser Regex
pLookAhead = try $ do
  _ <- string "(?="
  inner <- pRegex
  _ <- char ')'
  return (RLookAhead inner)

-- | Парсим незахватывающую группу: (?: expr)
pNonCaptureGroup :: Parser Regex
pNonCaptureGroup = try $ do
  _ <- string "(?:"
  inner <- pRegex
  _ <- char ')'
  return (RNonCapGroup inner)

-- | Парсим обычную захватывающую группу: ( expr )
--   Номер группы присвоим позже (или будем присваивать на этапе проверки)
--   Для упрощения сейчас ставим 0, а реальный номер назначим при проверке
pCaptureGroup :: Parser Regex
pCaptureGroup = do
  _ <- char '('
  -- проверяем, чтобы не было "?:", "?=", тогда это другая конструкция
  lookAhead $ noneOf "?"
  inner <- pRegex
  _ <- char ')'
  return (RGroup 0 inner)

-- | Парсим базовый элемент без учёта звезды (char, group, ref, look-ahead, etc.)
--   Оператор <|> в библиотеке Parsec — это комбинатор альтернативы. Он последовательно пытается применить два парсера:
--   если первый парсер неуспешен, то применяется второй. Если первый парсер сработал, второй парсер не выполняется.
pBase :: Parser Regex
pBase =
      pLookAhead
  <|> pGroupRef
  <|> pNonCaptureGroup
  <|> pCaptureGroup
  <|> pChar

-- | Парсим элемент со звёздочкой (expr*):
pStar :: Parser Regex
pStar = do
  base <- pBase
  rest <- optionMaybe (char '*') -- Парсим символ `*`, если он есть, иначе возвращаем `Nothing`.
  case rest of
    Just _  -> return (RStar base) -- Если `*` найден (`Just _`), создаем конструкцию `RStar base` (повторение base).
    Nothing -> return base -- Если `*` отсутствует (`Nothing`), возвращаем исходное базовое выражение.

--------------------------------------------------------------------------------
-- Парсер для конкатенации и альтернации
--
-- Регулярное выражение pRegex:
--   - «разбор по альтернативам» (A | B)
--   - внутри каждой альтернативы — последовательность (конкатенация) элементов

-- | основной парсер регулярного выражения.
pRegex :: Parser Regex
pRegex = pAlt

-- | Парсим альтернативу (A | B).
pAlt :: Parser Regex
pAlt = do
  first <- pConcat
  rest  <- many (char '|' >> pConcat) -- Парсим остальные альтернативы, если встречается `|`.
  return $ case rest of
    [] -> first -- Если альтернатив нет, возвращаем только `first`.
    _  -> foldl RAlt first rest -- Если есть альтернативы, собираем их в `RAlt`.

-- | Парсим конкатенацию (несколько подряд идущих кусков без разделителя).
pConcat :: Parser Regex
pConcat = do
  xs <- many1 pStar -- Парсим последовательность из одного или более элементов.
  -- many1, чтобы хотя бы один элемент схватить
  return $ if length xs == 1
           then head xs -- Если один элемент, возвращаем его как есть.
           else RConcat xs -- Если несколько элементов, собираем их в `RConcat`.

--------------------------------------------------------------------------------
-- Экспортируемая функция

parseRegex :: String -> Either ParseError Regex
parseRegex = parse (pRegex <* eof) "Regex"
-- <* eof — требует, чтобы парсер успешно дошел до конца строки (eof — "end of file"). Это гарантирует, что вся строка
-- разобрана целиком.
-- "Regex" — имя входных данных (используется для сообщений об ошибках).
-- parseRegex возвращает результат вызова parse, который будет:
--
-- Right regex, если разбор успешен (где regex — абстрактное синтаксическое дерево регулярного выражения).
-- Left parseError, если возникла ошибка при разборе.
