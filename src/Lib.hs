{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( fetch
    , formURL
    , Sheet (..)
    , doitall
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Network.HTTP.Conduit
import           Network.URL
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Scientific
-- import Control.Monad.Maybe

type MaybeIO = MaybeT IO

data Sheet = Sheet {
                      key :: String
                      , url :: URL
                      , fullJSON :: Value
                      , parsedData :: [Value]
                   }

data CellType = String !T.Text | Number !Scientific | Bool !Bool | Null
             deriving (Eq, Read, Show)

doitall :: String -> IO (Maybe Sheet)
doitall key = runMaybeT $ make key

make :: String -> MaybeIO Sheet
make keyf = do
  -- let urlf = formURL keyf
  -- jsonf <- msum $ getUrlCall $ exportURL urlf
  urlf <- MaybeT $ return $ formURL keyf
  jsonf <- MaybeT $ getUrlCall $ exportURL urlf
  parsed <- MaybeT $ fetch jsonf
  return Sheet {
                  key = keyf,
                  url = urlf,
                  fullJSON =jsonf,
                  parsedData = parsed
               }


urlTemplate = "https://spreadsheets.google.com/feeds/list/${key}/od6/public/values" :: String

formURL :: String -> Maybe URL
formURL key = do
  -- do concatenation to make url
  let template = T.pack urlTemplate
  let keyPattern = T.pack "${key}"
  let sUrl = T.replace keyPattern (T.pack key) template
  url <- importURL $ T.unpack sUrl
  return $ add_param url ("alt","json")

getUrlCall :: String -> IO (Maybe Value)
getUrlCall url = do
  urlJSON <- simpleHttp url
  let jsonValueForm = decode urlJSON:: Maybe Value
  return jsonValueForm


fetch :: Value -> IO (Maybe [Value])
fetch json = return $ parseMaybe parseSheet json


-- fetch :: String -> IO (Maybe [Value])
-- fetch url = do
--   urlJSON <- simpleHttp url
--   let jsonValueForm = decode urlJSON:: Maybe Value
--   let parsed = case jsonValueForm of
--                   Just val -> parseMaybe parseSheet val
--                   Nothing -> Nothing
--   return parsed

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
