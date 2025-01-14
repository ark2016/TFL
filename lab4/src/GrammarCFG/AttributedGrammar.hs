{-# LANGUAGE DeriveGeneric #-}
module GrammarCFG.AttributedGrammar
  ( Attrib(..)
  , AttributedProduction(..)
  , AttributedCFG(..)
  , buildAttributedGrammar
  ) where

import GHC.Generics (Generic)
import Data.List (nub, isPrefixOf)
import Control.Monad.State

import GrammarCFG.CFG
import GrammarCFG.GrammarBuilder (buildFrameGrammar)
import Regex.SyntaxChecker (CheckedRegex(..), RegexError(..))

--------------------------------------------------------------------------------
-- | Тип атрибутов. Здесь мы можем хранить любую информацию, необходимую для
--   проверки ограничений синтаксиса регулярного выражения.
--   Например:
--     - Количество групп
--     - Валидность ссылок на группы
--     - Информация о вложенных конструкциях
data Attrib = Attrib
  { groupCount :: Int   -- Количество захватывающих групп до текущего правила
  , refValid   :: Bool  -- Валидность ссылок в текущем правиле
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- | Атрибутированное правило.
data AttributedProduction = AttributedProduction
  { baseProduction     :: Production
  , inheritedAttrib    :: Attrib
  , synthesizedAttrib  :: Attrib
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- | Атрибутная контекстно-свободная грамматика.
data AttributedCFG = AttributedCFG
  { acfgNonterminals :: [Nonterminal]
  , acfgTerminals    :: [Terminal]
  , acfgStartSymbol  :: Nonterminal
  , acfgProductions  :: [AttributedProduction]
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- | Построение атрибутной грамматики из `CheckedRegex`.
buildAttributedGrammar :: CheckedRegex -> AttributedCFG
buildAttributedGrammar goodAst =
    let cfg = buildFrameGrammar goodAst
        attProds = evalState (mapM attributeProduction (productions cfg)) initialAttribState
    in AttributedCFG
        { acfgNonterminals = nonterminals cfg
        , acfgTerminals    = terminals cfg
        , acfgStartSymbol  = startSymbol cfg
        , acfgProductions  = attProds
        }

--------------------------------------------------------------------------------
-- | Состояние для построения атрибутов.
data AttribState = AttribState
  { currentGroupCount :: Int    -- Текущее количество групп
  , validReferences   :: Bool   -- Флаг валидности ссылок
  }
  deriving (Eq, Show)

-- | Начальное состояние атрибутного прохода.
initialAttribState :: AttribState
initialAttribState = AttribState
  { currentGroupCount = 0
  , validReferences   = True
  }

--------------------------------------------------------------------------------
-- | Функция для присвоения атрибутов одному правилу.
attributeProduction :: Production -> State AttribState AttributedProduction
attributeProduction p@(Production lhs rhs) = do
  st <- get
  let inherited = Attrib
        { groupCount = currentGroupCount st
        , refValid   = validReferences st
        }

  -- Логика атрибутов:
  -- Если правило содержит группу, увеличиваем count
  -- Если правило содержит ссылку, проверяем валидность
  let newGroupCount = currentGroupCount st + countGroups rhs
      newRefValid   = checkRefs rhs (currentGroupCount st)

  let synthesized = Attrib
        { groupCount = newGroupCount
        , refValid   = newRefValid
        }

  -- Обновляем состояние
  put st { currentGroupCount = newGroupCount, validReferences = newRefValid }

  return AttributedProduction
    { baseProduction     = p
    , inheritedAttrib    = inherited
    , synthesizedAttrib  = synthesized
    }

--------------------------------------------------------------------------------
-- | Функция для подсчёта количества групп в правой части правила.
countGroups :: [Symbol] -> Int
countGroups syms = length [ nt | N nt <- syms, isGroup nt ]

-- | Определение, является ли нетерминал группой
isGroup :: Nonterminal -> Bool
isGroup nt = "Group" `isPrefixOf` nt

-- | Функция для проверки валидности ссылок на группы.
--   Здесь предполагается, что ссылки имеют формат "Group<num>", например, "Group1", "Group2".
checkRefs :: [Symbol] -> Int -> Bool
checkRefs syms totalGroups =
  all (\ num -> num > 0 && num <= totalGroups)
      [ num | N nt <- syms, Just num <- [extractNumber nt], isGroup nt ]

-- | Вспомогательная функция для извлечения числа из имени нетерминала.
extractNumber :: Nonterminal -> Maybe Int
extractNumber nt =
  case dropWhile (not . (`elem` ['0'..'9'])) nt of
    []     -> Nothing
    digits -> case reads digits of
                [(n, "")] -> Just n
                _         -> Nothing

-- | Вспомогательная функция для извлечения номера группы.
extractGroupNumber :: Nonterminal -> Int
extractGroupNumber nt =
  case extractNumber nt of
    Just n  -> n
    Nothing -> 0 -- по умолчанию, если не найдено
