{-# LANGUAGE OverloadedStrings #-}
module Lib
    (
      Sheet
    , Row (row)
    , deleteRow
    , initialize
    , getRows
    , addRow
    , CellType (..)
    ) where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types          as AT
-- import qualified Data.ByteString.Lazy      as L
-- import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.ByteString as L8
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                 as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector               as V
import           GoogleRequest
import           Network.Connection        (TLSSettings (..))
import           Network.Google.OAuth2
import           Network.HTTP.Conduit
import Network.HTTP.Types.Status
import           Network.URL
import           Text.XML.Light
import           Text.XML.Light.Types

type MaybeIO = MaybeT IO

-- type Row = HM.HashMap T.Text CellType

data Row = Row {
                  rowid :: T.Text,
                  row   :: HM.HashMap T.Text CellType
                }deriving (Eq, Show)

data Sheet = Sheet {
                        key       :: String
                      , url     :: URL
                      , columns :: [T.Text]
                      , client  :: OAuth2Client
                   }


data CellType = String !T.Text | Number !Scientific | Bool !Bool | Null
             deriving (Eq, Read, Show)


initialize :: String -> String -> String -> IO (Maybe Sheet)
initialize keyf clientId clientSecret = runMaybeT $ do
  let clientf = OAuth2Client clientId clientSecret
  urlf <- MaybeT $ return $ formURL keyf
  jsonf <- MaybeT $ getValue (exportURL urlf) clientf
  cols <- MaybeT $ return $ parseMaybe getColumns jsonf
  return Sheet {
                  key = keyf,
                  url = urlf,
                  columns = cols,
                  client = clientf
               }


xmlns = Attr{
         attrKey= unqual "xmlns",
         attrVal="http://www.w3.org/2005/Atom"
         }

xmlnsgsx = Attr{
           attrKey = unqual "xmlns:gsx",
           attrVal = "http://schemas.google.com/spreadsheets/2006/extended"
          }


makexmlrow :: (T.Text, CellType) -> Element
makexmlrow (key, value) = do
  let valstring = case value of
                    Lib.String x -> T.unpack x
                    Lib.Number n -> show n
                    Lib.Bool b -> show b
  unode ("gsx:" ++ T.unpack key) valstring

makexml :: [(T.Text, CellType)] -> String
makexml rawRows = showElement $ add_attrs [xmlns, xmlnsgsx] $ unode "entry" $ map makexmlrow rawRows

addRow :: [(T.Text, CellType)] -> Sheet -> IO (Maybe Status)
addRow rawRow sheet =do
  let isvalid :: [(T.Text, CellType)] -> [T.Text] -> Bool
      isvalid [] _ = True
      isvalid ((first,_):xs) cols = elem first cols && isvalid xs cols

  let isEqualLength :: [a] -> [b] -> Bool
      isEqualLength x y = length x == length y

  let cols = map (T.drop 4) $ columns sheet
  if not (isEqualLength rawRow cols && isvalid rawRow cols)
    then do
      print  cols
      print rawRow
      return Nothing
    else do
      let xml = makexml rawRow
      -- print $ L8.pack xml
      status <- post (formPostURL ( key sheet)) (client sheet) xml
      return $ Just status


getRows :: Sheet -> IO (Maybe [Row])
getRows sheet = do
  jsonValueForm <- getValue (exportURL ( url sheet)) (client sheet)
  -- print jsonValueForm
  case jsonValueForm of
    Just val -> parseSheetJson val
    Nothing -> return Nothing


deleteRow :: Sheet -> Row -> IO ()
deleteRow sheet r = do
  let url = rowid r
  -- print $ T.unpack url
  status <- delete (T.unpack url) (client sheet)
  -- print status
  return ()

urlTemplate = "https://spreadsheets.google.com/feeds/list/${key}/od6/private/full" :: String

formURL :: String -> Maybe URL
formURL key = do
  -- do concatenation to make url
  let template = T.pack urlTemplate
  let keyPattern = T.pack "${key}"
  let sUrl = T.replace keyPattern (T.pack key) template
  url <- importURL $ T.unpack sUrl
  return $ add_param url ("alt","json")

formPostURL :: String -> String
formPostURL key = T.unpack $ T.replace (T.pack "${key}") (T.pack key) (T.pack urlTemplate)


getValue url client = do
  urlJSON <- get url client
  let jsonValueForm = decode urlJSON:: Maybe Value
  return jsonValueForm


parseSheetJson :: Value -> IO (Maybe [Row])
parseSheetJson json = return $ parseMaybe parseSheet json
                        where parseSheet = withObject "value" $ \obj -> do
                                  feed <- obj .: "feed"
                                  entry <- feed .: "entry"
                                  columns <- getColumnHelper entry

                                  let parseRows = withObject "object" $ \obj ->do
                                                    -- cols <- getColumnNames entry
                                                    let makePairs :: T.Text -> Parser (T.Text, CellType)
                                                        makePairs key = do
                                                                        valObject <- obj .: key
                                                                        val <- (valObject .: "$t") :: Parser Value
                                                                        let cell = case val of
                                                                                      AT.String x -> Lib.String x
                                                                                      AT.Bool b -> Lib.Bool b
                                                                                      AT.Number n -> Lib.Number n
                                                                                      _ -> Lib.Null
                                                                        return (T.drop 4 key, cell)
                                                    currRow <- mapM makePairs columns
                                                    linkId <- obj .: "id"
                                                    link <- (linkId .: "$t" ) :: Parser Value
                                                    return Row {
                                                                    rowid =  case link of
                                                                                AT.String u -> u
                                                                                _ -> T.pack "",
                                                                    row = HM.fromList currRow
                                                                  }

                                  let parseEntry = withArray "array" $ \arr -> return $ map parseRows (V.toList arr)
                                  rows <- parseEntry entry
                                  sequence rows


getColumns :: Value -> Parser [T.Text]
getColumns = withObject "value" $ \obj -> do
    feed <- obj .: "feed"
    entry <- feed .: "entry"
    getColumnHelper entry


getColumnHelper  =withArray "array" $ \arr -> do
  let obj = V.head arr
  let extract = withObject "object" $ \obj -> return $ HM.keys obj
  allkeys <- extract obj
  let columns = filter (T.isPrefixOf (T.pack "gsx$")) allkeys
  return columns
