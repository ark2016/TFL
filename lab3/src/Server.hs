{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Grammar (Grammar, parseGrammar)
import ChomskyNormalForm (toChomskyNormalForm)
import TestGenerator (generateTestCases)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status400)
import Data.Aeson (encode, decode, object, (.=), FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.ByteString.Lazy (ByteString)

-- JSON структуры для запросов и ответов
data RuleJSON = RuleJSON
  { lhs :: String
  , rhs :: [String]
  } deriving (Show, Generic, FromJSON, ToJSON)

data CustomGrammarRequest = CustomGrammarRequest
  { grammarRules :: [RuleJSON]
  } deriving (Show, Generic, FromJSON, ToJSON)

data TestRequest = TestRequest
  { grammarInput :: Maybe String
  , maxDepth     :: Int
  , numNegatives :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

data TestCase = TestCase
  { is_in_lang :: Bool
  , input      :: T.Text
  } deriving (Show, Generic, ToJSON)

data TestResponse = TestResponse
  { cases      :: [TestCase]
  , positive   :: Int
  , negative   :: Int
  , totalTests :: Int
  } deriving (Show, Generic, ToJSON)

app :: Application
app req respond = do
  body <- strictRequestBody req
  case decode body :: Maybe TestRequest of
    Just (TestRequest mInput maxDepth numNegatives) -> do
      case mInput of
        Nothing -> respond $ responseLBS status400 [("Content-Type", "application/json")] $ encode $ object ["error" .= ("Missing grammar input" :: T.Text)]
        Just grammarInput -> case parseGrammar grammarInput of
          Nothing -> respond $ responseLBS status400 [("Content-Type", "application/json")] $ encode $ object ["error" .= ("Invalid grammar format" :: T.Text)]
          Just grammar -> do
            let cnfGrammar = toChomskyNormalForm grammar
            testCases <- map (\(str, label) -> TestCase { input = T.pack str, is_in_lang = label }) <$> generateTestCases cnfGrammar maxDepth numNegatives
            let positiveTests = length $ filter is_in_lang testCases
            let negativeTests = length testCases - positiveTests
            respond $ responseLBS status200 [("Content-Type", "application/json")] $ encode TestResponse
              { cases = testCases
              , positive = positiveTests
              , negative = negativeTests
              , totalTests = length testCases
              }
    Nothing -> respond $ responseLBS status400 [("Content-Type", "application/json")] $ encode $ object ["error" .= ("Invalid request body" :: T.Text)]

main :: IO ()
main = do
  putStrLn "Server running on http://localhost:8080"
  run 8080 app
