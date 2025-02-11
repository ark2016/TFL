{-# LANGUAGE DeriveGeneric #-}
module Regex.AST(
  Regex(..)
) where

-- | Абстрактное синтаксическое дерево регулярных выражений
data Regex
  = RConcat [Regex]        -- Конкатенация нескольких подвыражений
  | RAlt    Regex Regex    -- Альтернация (A | B)
  | RGroup  Int Regex      -- Захватывающая группа (номер группы, вложенное выражение)
  | RRef    Int            -- Ссылка на группу: (?1), (?2), ...
  | RLookAhead Regex       -- Опережающая проверка: (?= expr)
  | RNonCapGroup Regex     -- Незахватывающая группа: (?: expr)
  | RStar   Regex          -- Клонирование (звезда): expr*
  | RChar   Char           -- Обычный символ [a-z]
  deriving (Eq, Show)
