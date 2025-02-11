module Regex.SyntaxChecker
  ( checkRegex
  , CheckedRegex(..)
  , RegexError(..) -- Экспорт типа RegexError и его конструкторов
) where

import Regex.AST
import Data.Set (Set)
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- | Промежуточное дерево, где группы уже пронумерованы корректно
data CheckedRegex
  = CRConcat [CheckedRegex]         -- Конкатенация
  | CRAlt CheckedRegex CheckedRegex -- Альтернатива
  | CRGroup Int CheckedRegex        -- Номер группы
  | CRRef   Int                     -- Ссылка на группу по номеру
  | CRLookAhead CheckedRegex        -- Look-ahead
  | CRNonCapGroup CheckedRegex      -- незахваченная группа
  | CRStar CheckedRegex             -- Повторение (*)
  | CRChar Char                     -- Символ
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Возможные ошибки
data RegexError
  = TooManyGroups               -- Слишком много групп захвата (> 9)
  | NestedLookAhead             -- Вложенный look-ahead
  | InvalidGroupRef Int         -- Ссылка на группу, которой нет в тексте
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Основная функция проверки.
--
-- 1. Сбор всех групп (в порядке появления), присвоение им номеров 1..k.
-- 2. Сбор всех ссылок (?n). Проверка, что n ≤ k.
-- 3. Построение CheckedRegex (замена RGroup 0 на CRGroup i и т.д.).
--
checkRegex :: Regex -> Either RegexError CheckedRegex
checkRegex ast = do
  -- 1. Собираем группы (в порядке появления) + ссылки
  let (groupsList, refsSet) = collectGroupsAndRefs ast []
      groupCount = length groupsList

  -- 2. Проверяем число групп
  if groupCount > 9
    then Left TooManyGroups
    else Right ()

  -- 3. Проверяем все ссылки
  --    Ссылка (RRef n) корректна, если 1 <= n <= groupCount
  -- Возвращает новое множество, содержащее только элементы, удовлетворяющие условию.
  let invalidRefs = Set.filter (\r -> r < 1 || r > groupCount) refsSet
  if not (Set.null invalidRefs)
    then Left (InvalidGroupRef (Set.findMin invalidRefs))
    else Right () -- результат типа Either RegexError (), который используется для обозначения успешного выполнения
                  -- проверки без ошибки.

  -- 4. Строим CheckedRegex
  buildChecked ast groupsList

--------------------------------------------------------------------------------
-- | collectGroupsAndRefs
--   Обход дерева слева направо:
--   - Когда видим RGroup 0, добавляем его в конец списка groupsList
--   - Когда видим RRef n, добавляем n во множество refsSet
--
-- Возвращаем ( [порядок групп], {все ссылки} ).
--
collectGroupsAndRefs :: Regex
                     -> [Regex]       -- уже найденные группы (в порядке)
                     -> ([Regex], Set Int)
collectGroupsAndRefs (RConcat rs) acc =
  -- св развёртке foldl, аккуратно обходим каждый r
  foldl
    (\(grps, refs) r ->  -- Обработчик каждого узла r
       let (g2, r2) = collectGroupsAndRefs r grps -- Рекурсивный вызов на поддерево
       in (g2, refs `Set.union` r2)) -- Объединяем ссылки
    (acc, Set.empty) -- Начальные значения: список групп и пустое множество ссылок
    rs -- Список дочерних узлов

collectGroupsAndRefs (RAlt r1 r2) acc =
  let (g1, s1) = collectGroupsAndRefs r1 acc -- Обход первого подвыражения
      (g2, s2) = collectGroupsAndRefs r2 g1 -- Обход второго подвыражения
  in (g2, s1 `Set.union` s2) -- Объединяем группы и ссылки

collectGroupsAndRefs (RStar r) acc =
  collectGroupsAndRefs r acc -- Обход вложенного выражения

collectGroupsAndRefs (RGroup 0 r) acc =
  -- добавляем эту группу (RGroup 0 r) в конец списка
  let acc' = acc ++ [RGroup 0 r]
      (g1, s1) = collectGroupsAndRefs r acc' -- Рекурсивно обходим вложенное выражение
  in (g1, s1)

collectGroupsAndRefs (RRef n) acc =
  (acc, Set.singleton n) -- Добавляем ссылку на группу в множество
  -- Set.singleton — это функция из модуля Data.Set, которая создает множество с единственным элементом.

collectGroupsAndRefs (RLookAhead r) acc =
  let (g1, s1) = collectGroupsAndRefs r acc -- Обходим подвыражение `r`
  in (g1, s1)

collectGroupsAndRefs (RNonCapGroup r) acc =
  collectGroupsAndRefs r acc -- Обходим подвыражение `r`

collectGroupsAndRefs (RChar _) acc =
  (acc, Set.empty) -- Символ не добавляет групп или ссылок

--------------------------------------------------------------------------------
-- | buildChecked
--   Второй проход: заменяем группы (RGroup 0) на CRGroup i, где i — индекс
--   этой группы в списке groupsList, а также проверяем вложенные look-ahead’ы.
--
buildChecked :: Regex -> [Regex] -> Either RegexError CheckedRegex
buildChecked ast groupsList = go ast
  where
    -- найдем индекс данной группы (RGroup 0 r) в списке groupsList
    groupIndex :: Regex -> Int
    groupIndex g =
      case lookup g (zip groupsList [1..]) of -- создание пары (группа, индекс) + поиск индекса группы
        Just i  -> i
        Nothing -> error "Impossible: group not found"
    --mapM возвращает список результатов m [b].
    --mapM_ игнорирует результаты и возвращает m ().
    go (RConcat rs) = do
      rs' <- mapM go rs
      return (CRConcat rs')

    go (RAlt r1 r2) = do
      r1' <- go r1
      r2' <- go r2
      return (CRAlt r1' r2')

    go (RStar r) = do
      r' <- go r
      return (CRStar r')

    go (RGroup 0 r) = do
      -- Найти индекс группы в списке:
      let i = groupIndex (RGroup 0 r)
      r' <- go r
      return (CRGroup i r')

    go (RRef n) =
      return (CRRef n)

    go (RNonCapGroup r) = do
      r' <- go r
      return (CRNonCapGroup r')

    go (RLookAhead r) = do
      -- Запрещаем вложенные look-ahead и группы внутри
      validateLookAhead r
      r' <- go r
      return (CRLookAhead r')

    go (RChar c) =
      return (CRChar c)

    -- Проверка look-ahead: запрещаем (RLookAhead _) и (RGroup _ _)
    validateLookAhead :: Regex -> Either RegexError ()
    validateLookAhead (RLookAhead _) =
      Left NestedLookAhead
    validateLookAhead (RGroup _ _) =
      Left NestedLookAhead
    validateLookAhead (RRef _) =
      Left NestedLookAhead
    validateLookAhead (RStar r) =
      validateLookAhead r
    validateLookAhead (RAlt r1 r2) = do
      validateLookAhead r1
      validateLookAhead r2
    -- mapM_ — это версия mapM, которая игнорирует возвращаемый результат и используется, когда нам важен только эффект
    -- выполнения функций.
    -- Если хотя бы одно выражение вызывает ошибку (Left NestedLookAhead), весь результат будет Left.
    -- Если все вызовы успешны (Right ()), результат — Right ().
    validateLookAhead (RConcat rs) =
      mapM_ validateLookAhead rs
    validateLookAhead (RNonCapGroup r) =
      validateLookAhead r
    validateLookAhead (RChar _) =
      return ()
