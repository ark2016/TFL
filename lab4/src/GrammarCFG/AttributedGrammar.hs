{-# LANGUAGE DeriveGeneric #-}
module GrammarCFG.AttributedGrammar
  ( Attrib(..)
  , AttributedProduction(..)
  , AttributedCFG(..)
  , buildAttributedGrammar
  ) where

import GHC.Generics (Generic)
import Data.List (nub, isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Applicative ((<|>))

import GrammarCFG.CFG
import GrammarCFG.GrammarBuilder (buildFrameGrammar)

import Regex.SyntaxChecker (CheckedRegex(..))

--------------------------------------------------------------------------------
-- | Тип атрибута с полем для необходимого следующего символа (для просмотра вперед)
--   Также отслеживается количество захватывающих групп и валидность ссылок на группы.
data Attrib = Attrib
  { groupCount  :: Int          -- ^ Сколько групп захвата было обнаружено до текущего момента.
  , refValid    :: Bool         -- ^ Валидны ли ссылки на группы до текущего момента.
  , neededFirst :: Maybe Char   -- ^ Требуемый следующий символ из-за ограничения look-ahead.
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- | Правила атрибутов: базовое Правило + унаследованные и синтезированные атрибуты.
--   Унаследованные атрибуты передаются из контекста, синтезированные атрибуты генерируются в результате.
data AttributedProduction = AttributedProduction
  { baseProduction     :: Production  -- ^ Базовое Правило из CFG.
  , inheritedAttrib    :: Attrib      -- ^ Унаследованные атрибуты для данного Правила.
  , synthesizedAttrib  :: Attrib      -- ^ Синтезированные атрибуты после обработки Правила.
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- | Атрибутированная контекстно-свободная грамматика.
--   Содержит список нетерминалов, терминалов, начальный символ и список атрибутированных Правил.
data AttributedCFG = AttributedCFG
  { acfgNonterminals :: [Nonterminal]             -- ^ Список нетерминалов грамматики.
  , acfgTerminals    :: [Terminal]                -- ^ Список терминалов грамматики.
  , acfgStartSymbol  :: Nonterminal               -- ^ Начальный символ грамматики.
  , acfgProductions  :: [AttributedProduction]    -- ^ Список атрибутированных Правил.
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- | Функция для построения атрибутированной грамматики из проверенного регулярного выражения.
--   Она сначала строит базовую CFG, затем присваивает атрибуты каждому Правилу.
buildAttributedGrammar :: CheckedRegex -> AttributedCFG
buildAttributedGrammar ast =
  let cfg = buildFrameGrammar ast
      attProds = evalState (mapM attributeProduction (productions cfg)) initState
  in AttributedCFG
       { acfgNonterminals = nonterminals cfg
       , acfgTerminals    = terminals cfg
       , acfgStartSymbol  = startSymbol cfg
       , acfgProductions  = attProds
       }

--------------------------------------------------------------------------------
-- | Состояние для присваивания атрибутов.
--   Отслеживает текущее количество групп, валидность ссылок и требуемый следующий символ.
data AttribState = AttribState
  { currentGroupCount :: Int           -- ^ Текущее количество обнаруженных групп захвата.
  , validReferences   :: Bool          -- ^ Валидны ли ссылки на группы.
  , pendingLookahead  :: Maybe Char    -- ^ Требуемый следующий символ из-за ограничения look-ahead.
  } deriving (Eq, Show)

-- | Начальное состояние для присваивания атрибутов.
initState :: AttribState
initState = AttribState
  { currentGroupCount = 0
  , validReferences   = True
  , pendingLookahead  = Nothing
  }

--------------------------------------------------------------------------------
-- | Функция для присваивания атрибутов одному Правилу.
--   Она обновляет состояние атрибутов на основе текущего Правила.
attributeProduction :: Production -> State AttribState AttributedProduction
attributeProduction p@(Production lhs rhs) = do
  st <- get

  -- Унаследованные атрибуты: копируем текущие атрибуты из состояния.
  let inh = Attrib
        { groupCount  = currentGroupCount st
        , refValid    = validReferences st
        , neededFirst = pendingLookahead  st
        }

  -- Определяем новые группы захвата в правой части Правила.
  let newlyFoundGroups = [ i
                         | N nt <- rhs
                         , Just i <- [extractGroupNumber nt]
                         ]

  -- Обновляем счетчик групп захвата.
  let newGroupCount = currentGroupCount st + length newlyFoundGroups

  -- Проверяем валидность ссылок на группы в правой части Правила.
  let newRefValid = checkRefs rhs (currentGroupCount st)

  -- Проверяем, является ли текущее Правило ограничением look-ahead.
  let thisLook = parseLookaheadSymbol lhs rhs

  -- Объединяем текущие ограничения look-ahead с предыдущими.
  let combinedLook = thisLook <|> pendingLookahead st

  -- Если есть требование look-ahead, проверяем соответствие первого символа.
  let okLook = checkFirstMatchesLookahead rhs combinedLook

  -- Определяем окончательную валидность ссылок с учетом ограничений look-ahead.
  let finalRefValid = newRefValid && okLook

  -- Синтезированные атрибуты: обновленные значения на основе текущего Правила.
  let syn = Attrib
        { groupCount  = newGroupCount
        , refValid    = finalRefValid
        , neededFirst = combinedLook
        }

  -- Обновляем состояние атрибутов.
  put st
    { currentGroupCount = newGroupCount
    , validReferences   = finalRefValid
    , pendingLookahead  = combinedLook
    }

  -- Возвращаем атрибутированное Правило.
  return AttributedProduction
    { baseProduction     = p
    , inheritedAttrib    = inh
    , synthesizedAttrib  = syn
    }

--------------------------------------------------------------------------------
-- | Функция для подсчета количества новых захватывающих групп в правой части Правила.
countGroups :: [Symbol] -> Int
countGroups syms = length [ nt | N nt <- syms, "Group" `isPrefixOf` nt ]

--------------------------------------------------------------------------------
-- | Функция для проверки валидности ссылок на группы в правой части Правила.
--   Проверяет, что номера групп находятся в допустимом диапазоне.
checkRefs :: [Symbol] -> Int -> Bool
checkRefs syms tot =
  all (\num -> (num > 0) && (num <= tot))
      [ num
      | N nt <- syms
      , Just num <- [extractGroupNumber nt]  -- Используем extractGroupNumber вместо extractNumber
      , "Group" `isPrefixOf` nt
      ]

--------------------------------------------------------------------------------
-- | Функция для извлечения номера группы из нетерминала вида "GroupX",
--   где X — номер группы.
extractGroupNumber :: Nonterminal -> Maybe Int
extractGroupNumber nt =
  if "Group" `isPrefixOf` nt
    then case drop 5 nt of
           [] -> Nothing
           ds -> case reads ds of
                   [(n,"")] -> Just n
                   _        -> Nothing
    else Nothing

--------------------------------------------------------------------------------
-- | Функция для определения, представляет ли текущее Правило ограничение look-ahead.
--   Если имя нетерминала начинается с "LookAhead_" и правая часть пустая, то это ограничение.
parseLookaheadSymbol :: Nonterminal -> [Symbol] -> Maybe Char
parseLookaheadSymbol lhs rhs =
  if null rhs && "LookAhead_" `isPrefixOf` lhs
    then Just (last lhs)  -- Извлекаем последний символ как требуемый.
    else Nothing

--------------------------------------------------------------------------------
-- | Функция для проверки соответствия первого символа правой части требуемому символу из look-ahead.
checkFirstMatchesLookahead :: [Symbol] -> Maybe Char -> Bool
checkFirstMatchesLookahead [] Nothing  = True
checkFirstMatchesLookahead [] (Just _) = True  -- Нет следующего символа, проверку пропускаем.
checkFirstMatchesLookahead (T c : _) (Just needed) = (c == needed)  -- Сравниваем терминал.
checkFirstMatchesLookahead (N nt : _) (Just needed) =
  -- предполагаем, что нетерминал может генерировать нужный символ.
  True
checkFirstMatchesLookahead _ _ = True
