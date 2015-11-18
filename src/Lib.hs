{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( fetch
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Network.HTTP.Conduit
import           Network.URL

someFunc :: IO ()
someFunc = putStrLn "someFunc"

formURL :: String -> Maybe URL
formURL surl = do
  url <- importURL surl
  return $ add_param url ("alt","json")

fetch :: String -> IO (Maybe [Value])
fetch url = do
  urlJSON <- simpleHttp url
  let jsonValueForm = decode urlJSON:: Maybe Value
  let parsed = case jsonValueForm of
                  Just val -> parseMaybe parseSheet val
                  Nothing -> Nothing
  return parsed

parseSheet :: Value -> Parser [Value]
parseSheet = withObject "value" $ \obj -> do
    feed <- obj .: "feed"
    entry <- feed .: "entry"
    columns <- getColumnNames entry

    let parseRows = withObject "object" $ \obj ->do
                      -- cols <- getColumnNames entry
                      let makePairs :: T.Text -> Parser (T.Text, Value)
                          makePairs key = do
                                          valObject <- obj .: key
                                          val <- valObject .: "$t"
                                          return (T.drop 4 key, val)
                      currRow <- mapM makePairs columns
                      let consObj = object currRow
                      return consObj

    let parseEntry = withArray "array" $ \arr -> return $ map parseRows (V.toList arr)
    rows <- parseEntry entry
    sequence rows

getColumnNames  =withArray "array" $ \arr -> do
  let obj = V.head arr
  let extract = withObject "object" $ \obj -> return $ HM.keys obj
  allkeys <- extract obj
  let columns = filter (T.isPrefixOf (T.pack "gsx$")) allkeys
  return columns
