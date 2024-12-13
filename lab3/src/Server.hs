{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Grammar
import CYKParser
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status400)
import Data.Aeson (encode, decode, object, (.=), FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.ByteString.Lazy (ByteString)
import Control.Monad (forM)

-- | Входные данные для генерации тестов
data TestRequest = TestRequest
  { grammarIndex :: Int  -- ^ Индекс грамматики
  , numTests     :: Int  -- ^ Количество тестов
  } deriving (Show, Generic)

-- | Результат теста
data TestCase = TestCase
  { input    :: T.Text  -- ^ Строка для тестирования
  , expected :: Bool    -- ^ Ожидаемый результат
  , actual   :: Bool    -- ^ Фактический результат
  , status   :: T.Text  -- ^ PASS или FAIL
  } deriving (Show, Generic)

-- | Ответ сервера
data TestResponse = TestResponse
  { totalTests :: Int         -- ^ Общее количество тестов
  , passed     :: Int         -- ^ Успешные тесты
  , failed     :: Int         -- ^ Проваленные тесты
  , cases      :: [TestCase]  -- ^ Список тестов
  } deriving (Show, Generic)

-- JSON-инстансы
instance FromJSON TestRequest
instance ToJSON TestCase
instance ToJSON TestResponse

-- | Пример списка грамматик
grammars :: [Grammar]
grammars =
  [ Grammar [(NonTerminal "S", [Terminal 'a'])]
  , Grammar [(NonTerminal "S", [NonTerminal "A", Terminal 'b'])]
  ]

-- | Генерация результатов тестов
generateTests :: Grammar -> Int -> IO [TestCase]
generateTests grammar n = do
  let testStrings = map (\i -> replicate i 'a') [1..n]  -- Примеры: "a", "aa", "aaa"
  return $ flip map testStrings $ \s ->
    let isValid = cykParse grammar s
    in TestCase (T.pack s) True isValid (if isValid then "PASS" else "FAIL")

-- | Основное приложение
app :: Application
app req respond = do
  body <- strictRequestBody req
  case decode body :: Maybe TestRequest of
    Just (TestRequest gIdx n) ->
      if gIdx <= 0 || gIdx > length grammars
        then respond $ responseLBS status400 [("Content-Type", "application/json")]
                        $ encode $ object ["error" .= ("Invalid grammar index" :: T.Text)]
        else do
          let grammar = grammars !! (gIdx - 1)
          testCases <- generateTests grammar n
          let passedTests = length $ filter (\tc -> status tc == "PASS") testCases
          let failedTests = length testCases - passedTests
          respond $ responseLBS status200 [("Content-Type", "application/json")]
            $ encode TestResponse
                { totalTests = length testCases
                , passed = passedTests
                , failed = failedTests
                , cases = testCases
                }
    Nothing -> respond $ responseLBS status400 [("Content-Type", "application/json")]
                 $ encode $ object ["error" .= ("Invalid request" :: T.Text)]

-- | Запуск сервера
main :: IO ()
main = do
  putStrLn "Server running on http://localhost:8080"
  run 8080 app
