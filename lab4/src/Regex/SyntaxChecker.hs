module Regex.SyntaxChecker
  ( checkRegex
  , CheckedRegex(..)
  , RegexError(..) -- Export the RegexError type and its constructors
) where



import Control.Monad
import Regex.AST

--------------------------------------------------------------------------------
-- Промежуточное дерево, где группы уже пронумерованы корректно
data CheckedRegex
  = CRConcat [CheckedRegex]
  | CRAlt CheckedRegex CheckedRegex
  | CRGroup Int CheckedRegex    -- Номер группы
  | CRRef   Int
  | CRLookAhead CheckedRegex
  | CRNonCapGroup CheckedRegex
  | CRStar CheckedRegex
  | CRChar Char
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Возможные ошибки
data RegexError
  = TooManyGroups               -- Слишком много групп захвата (> 9)
  | NestedLookAhead             -- Вложенный look-ahead
  | InvalidGroupRef Int         -- Ссылка на неинициализированную/невалидную группу
  | FutureGroupRef Int          -- Ссылка на группу, объявленную "позже"
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Тип состояния проверяющего модуля
type GroupCount = Int
type Declared   = [Int]  -- Список номеров уже объявленных групп

type CheckerState = (GroupCount, Declared)

-- Монадический стек для проверки
newtype RegexErrorState a = RegexErrorState
  { runCheck :: CheckerState -> Either RegexError (a, CheckerState) }

instance Functor RegexErrorState where
  fmap f ma = RegexErrorState $ \st ->
    case runCheck ma st of
      Left err          -> Left err
      Right (val, st')  -> Right (f val, st')

instance Applicative RegexErrorState where
  pure x = RegexErrorState $ \st -> Right (x, st)
  mf <*> ma = RegexErrorState $ \st ->
    case runCheck mf st of
      Left err         -> Left err
      Right (f, st')   ->
        case runCheck ma st' of
          Left err2          -> Left err2
          Right (val, st'')  -> Right (f val, st'')

instance Monad RegexErrorState where
  return = pure
  ma >>= f = RegexErrorState $ \st ->
    case runCheck ma st of
      Left err        -> Left err
      Right (val,st') -> runCheck (f val) st'

-- Функции для работы с состоянием и ошибками
throwError :: RegexError -> RegexErrorState a
throwError e = RegexErrorState $ \_ -> Left e

getState :: RegexErrorState CheckerState
getState = RegexErrorState $ \st -> Right (st, st)

putState :: CheckerState -> RegexErrorState ()
putState st = RegexErrorState $ \_ -> Right ((), st)

--------------------------------------------------------------------------------
-- Монадические «примитивы» для удобства

-- Регистрируем новую группу (инкремент счётчика)
newGroup :: RegexErrorState Int
newGroup = do
  (n, declared) <- getState
  let n' = n + 1
  putState (n', declared ++ [n'])
  return n'

-- Проверяем ссылку на группу
checkRef :: Int -> RegexErrorState ()
checkRef num = do
  (_n, declared) <- getState
  if num `elem` declared
    then return ()
    else throwError (InvalidGroupRef num)

-- Проверяем «будущую» ссылку
checkFutureRef :: Int -> RegexErrorState ()
checkFutureRef num = do
  (cnt, _decls) <- getState
  if num <= cnt
    then return ()
    else throwError (FutureGroupRef num)

--------------------------------------------------------------------------------
-- Основная функция проверки (возвращает либо ошибку, либо «причесанный» AST)

checkRegex :: Regex -> Either RegexError CheckedRegex
checkRegex ast = do
  let initialState = (0, []) -- (текущее число групп, номера уже объявленных групп)
  result <- runCheck (check ast) initialState
  let (ast', (finalCount, _decls)) = result
  -- Проверяем, что количество групп не превышает 9
  if finalCount > 9
    then Left TooManyGroups
    else Right ast'

--------------------------------------------------------------------------------
-- Функция обхода AST с проверкой

check :: Regex -> RegexErrorState CheckedRegex
check (RChar c) =
  return (CRChar c)

check (RConcat rs) = do
  rs' <- mapM check rs
  return $ CRConcat rs'

check (RAlt r1 r2) = do
  r1' <- check r1
  r2' <- check r2
  return $ CRAlt r1' r2'

check (RStar r) = do
  r' <- check r
  return (CRStar r')

check (RGroup _innerId r) = do
  -- При встрече захватывающей группы регистрируем новую
  newId <- newGroup
  r' <- check r
  return (CRGroup newId r')

check (RRef num) = do
  -- Проверяем, что ссылка допустима
  checkRef num
  checkFutureRef num
  return (CRRef num)

check (RNonCapGroup r) = do
  r' <- check r
  return (CRNonCapGroup r')

check (RLookAhead r) = do
  -- Запрещаем вложенные look-ahead и захватывающие группы внутри look-ahead
  disableInnerLookAhead r
  r' <- check r
  return (CRLookAhead r')

-- Рекурсивная функция, которая проверяет отсутствие вложенных look-ahead’ов и групп
disableInnerLookAhead :: Regex -> RegexErrorState ()
disableInnerLookAhead (RLookAhead _) =
  throwError NestedLookAhead

disableInnerLookAhead (RGroup _ _) =
  throwError NestedLookAhead

disableInnerLookAhead (RChar _) = return ()
disableInnerLookAhead (RRef _) = return ()
disableInnerLookAhead (RStar r) = disableInnerLookAhead r
disableInnerLookAhead (RAlt r1 r2) = do
  disableInnerLookAhead r1
  disableInnerLookAhead r2
disableInnerLookAhead (RConcat rs) = mapM_ disableInnerLookAhead rs
disableInnerLookAhead (RNonCapGroup r) = disableInnerLookAhead r
