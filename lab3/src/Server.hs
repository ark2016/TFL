{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Main where

import Grammar
import CYKParser
import ChomskyNormalForm (toChomskyNormalForm)

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status400)
import Data.Aeson (encode, decode, object, (.=), FromJSON, ToJSON)  -- Импортируем классы
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.ByteString.Lazy (ByteString)
import Control.Monad (forM)
import System.Random (randomRIO)

data RuleJSON = RuleJSON
  { lhs :: String
  , rhs :: [String]
  } deriving (Show, Generic, FromJSON)

data CustomGrammarRequest = CustomGrammarRequest
  { grammarRules :: [RuleJSON]
  } deriving (Show, Generic, FromJSON)

data TestRequest = TestRequest
  { grammarIndex  :: Maybe Int
  , numTests      :: Int
  , customGrammar :: Maybe CustomGrammarRequest
  } deriving (Show, Generic, FromJSON)

-- Просто добавляем deriving (ToJSON)
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

grammars :: [Grammar]
grammars =
  [ Grammar [(NonTerminal "S", [Terminal 'a'])],
    Grammar [(NonTerminal "S", [Terminal 'a', NonTerminal "S", Terminal 'b']),
             (NonTerminal "S", [Epsilon])]
  ]

generateTests :: Grammar -> Int -> IO [TestCase]
generateTests grammar n = do
  let testStrings = ["a", "aa", "aaa", "aab", "abb", "ab", "b", ""]
  let selectedStrings = take n (cycle testStrings)
  testCases <- forM selectedStrings $ \s -> do
    let isValid = cykParse grammar s
    return TestCase { is_in_lang = isValid, input = T.pack s }
  return testCases

parseCustomGrammar :: CustomGrammarRequest -> Grammar
parseCustomGrammar (CustomGrammarRequest rules) =
  Grammar $ map toRule rules
  where
    toRule (RuleJSON l r) =
      (NonTerminal l, map parseSymbol r)

    parseSymbol "ε" = Epsilon
    parseSymbol s
      | length s == 1 && s >= "a" && s <= "z" = Terminal (head s)
      | otherwise = NonTerminal s

app :: Application
app req respond = do
  body <- strictRequestBody req
  case decode body :: Maybe TestRequest of
    Just (TestRequest mgIdx n mCustom) -> do
      grammar <- case mgIdx of
        Just gIdx ->
          if gIdx <= 0 || gIdx > length grammars
            then return Nothing
            else return (Just (grammars !! (gIdx - 1)))
        Nothing -> case mCustom of
          Just custReq -> return (Just (parseCustomGrammar custReq))
          Nothing -> return Nothing

      case grammar of
        Nothing -> respond $ responseLBS status400 [("Content-Type", "application/json")]
                        $ encode $ object ["error" .= ("Invalid grammar" :: T.Text)]
        Just g -> do
          let cnfGrammar = toChomskyNormalForm g
          testCases <- generateTests cnfGrammar n
          let passedTests = length $ filter is_in_lang testCases
          let failedTests = length testCases - passedTests
          respond $ responseLBS status200 [("Content-Type", "application/json")]
            $ encode TestResponse
                { cases = testCases
                , positive = passedTests
                , negative = failedTests
                , totalTests = length testCases
                }
    Nothing -> respond $ responseLBS status400 [("Content-Type", "application/json")]
                 $ encode $ object ["error" .= ("Invalid request" :: T.Text)]

main :: IO ()
main = do
  putStrLn "Server running on http://localhost:8080"
  run 8080 app
