{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module SheetDB
  (
    access
  , Selection(..),  Query(..), Order, query
  , find, remove, insert, update
  , Selector(..), (~>),(~>=), (~<=), (~<), (~=), (~&&~), (~||~)
  ) where

import           Control.Monad             (filterM)
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                as A
import           Data.Aeson.Types          as AT
import qualified Data.ByteString.Char8     as B8
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           GoogleRequest
import           Network.Google.OAuth2
import           Network.HTTP.Types        hiding (Query)
import           Network.URL
import           SheetTypes                as ST
import           Text.XML.Light
import           Text.XML.Light.Types

urlTemplate = "https://spreadsheets.google.com/feeds/list/${key}/${worksheetid}/private/full" :: String

oauth :: Sheet -> OAuth2Client
oauth sheet = OAuth2Client (cid sheet) (csecret sheet)

access :: String -> String -> String -> String -> IO (Maybe Sheet)
access keyf worksheetidf clientId clientSecret = runMaybeT $ do
  let clientf = OAuth2Client clientId clientSecret
  urlf <- MaybeT $ return $ formURL keyf worksheetidf [("alt","json")]
  jsonf <- MaybeT $ getJson (exportURL urlf) clientf
  cols <- MaybeT $ return $ AT.parseMaybe getColumns jsonf
  return Sheet {
                  key = keyf,
                  worksheetId = worksheetidf,
                  columns = cols,
                  cid = clientId,
                  csecret = clientSecret
               }

data Outcome = Success | Failure
                  deriving (Show, Eq)
-- Selection

data Selector = Gtr ColName ST.Value
              | Gteqr ColName ST.Value
              | Ltr ColName ST.Value
              | Lteqr ColName ST.Value
              | Eqr ColName ST.Value
              | And Selector Selector
              | Or Selector Selector
              | Empty
                deriving (Show, Eq)

(~>) :: (Val v) => ColName -> v -> Selector
k ~> v = Gtr k (val v)

(~>=) :: (Val v) => ColName -> v -> Selector
k ~>= v = Gteqr k (val v)

(~<) :: (Val v) => ColName -> v -> Selector
k ~< v = Ltr k (val v)

(~<=) :: (Val v) => ColName -> v -> Selector
k ~<= v = Lteqr k (val v)

(~=) :: (Val v) => ColName -> v -> Selector
k ~= v = Eqr k (val v)

(~&&~) :: Selector -> Selector -> Selector
s ~&&~ t = And s t

(~||~) :: Selector -> Selector -> Selector
s ~||~ t = Or s t

data Selection = Select {selector :: Selector, sheet :: Sheet}  deriving (Show, Eq)
-- Selects rows in spreadsheet that match selector

-- Query

-- Use 'Select' to create a basic query with defaults, then modify if desired. For example, @(Select sel sheet) {limit = 10}@
data Query = Query {
  selection :: Selection,
  skip      :: Int,  -- ^ Number of initial matching documents to skip. Default = 0
  limit     :: Int, -- ^ Maximum number of documents to return, 0 = no limit. Default = 0
  sort      :: Order  -- ^ Sort results by this order, [] = no sort. Default = []
  } deriving (Show, Eq)


data Order = Order {
  colname :: ColName,
  reverse :: Bool
  } | NoOrder deriving (Show, Eq)


query :: Selector -> Sheet -> Query
query sel sheet = Query (Select sel sheet) 0 0 NoOrder


makeQueryUrl :: Query -> Maybe String
makeQueryUrl q = do
  let spreadsheet = sheet $ selection q
  let order = sort q
  let param1 = [("alt","json")]::[(String,String)]
  let selectString = selectorQueryUrl $ selector $ selection q
  let param2 = if  selectString == "" then  param1 else ("sq", selectString ) : param1
  let params = case order of
                NoOrder -> param2
                Order col rev -> param2 ++ [("orderby",T.unpack col),("reverse", show rev)]
  url <- formURL (key spreadsheet ) (worksheetId spreadsheet) params
  return $ exportURL url

-- Converts Selector to String. example: Gtr "age" 25 -> age > 25
selectorQueryUrl :: Selector -> String
selectorQueryUrl (Gtr colname value) = T.unpack colname ++ " > " ++ show value
selectorQueryUrl (Gteqr colname value) = T.unpack colname ++ " >= " ++ show value
selectorQueryUrl (Ltr colname value) = T.unpack colname ++ " < " ++ show value
selectorQueryUrl (Lteqr colname value) = T.unpack colname ++ " <= " ++ show value
selectorQueryUrl (Eqr colname value) = T.unpack colname ++ " = " ++ show value
selectorQueryUrl (And sel1 sel2) = selectorQueryUrl sel1 ++ " and " ++ selectorQueryUrl sel2
selectorQueryUrl (Or sel1 sel2) = selectorQueryUrl sel1 ++ " or " ++ selectorQueryUrl sel2
selectorQueryUrl Empty = ""

-- Encodes Query parameter
encodeSelectorQueryUrl :: String -> String
encodeSelectorQueryUrl q = B8.unpack $ urlEncode True $ B8.pack q

idKey = T.pack "id"

validate :: Query -> Bool
validate q = skip q >= 0 && limit q >= 0

find :: Query -> IO (Either String [Row])
find q = do
  let sel = selection q
  if validate q
    then do
      let spreadsheet = sheet sel
      -- print $ makeQueryUrl q
      mayberows <- runMaybeT $ do
        url <- MaybeT $ return $ makeQueryUrl q
        jsonValueForm <-MaybeT $ getJson url (oauth spreadsheet)
        MaybeT $ parseSheetJson jsonValueForm
      case mayberows of
        Just rs ->do
          let rows
                | skip q == 0 && limit q == 0 = rs
                | skip q > 0 && limit q == 0 = drop (skip q) rs
                | skip q == 0 && limit q > 0 = take (limit q) rs
                | otherwise = take (limit q) $ drop (skip q) rs
          return $ Right rows
        Nothing -> return $ Left "Parse error or sheet data error."
    else
      return $ Left "Query skip or Query limit may be invalid."


-- Used to send a delete request to the Google Sheet.
remove :: Sheet -> Row -> IO Outcome
remove sheet r = do
  let url = at idKey r
  -- print $ T.unpack url
  status<-delete (T.unpack url) (oauth sheet)
  if status == status200 then return SheetDB.Success
    else return SheetDB.Failure

  -- print status

-- Adds a new row to google sheet after validation of data to be inserted
insert :: Row -> Sheet -> IO Outcome
insert rawRow sheet =do
  let cols = columns sheet
  if not (isEqualLength rawRow cols && isvalid rawRow cols)
    then
      -- print  cols
      -- print rawRow
      return SheetDB.Failure
    else do
      let xml = rowToXml rawRow
      print xml
      let maybeurl = formURL ( key sheet) (worksheetId sheet) []
      case maybeurl of
        Just url ->do
          status <- post (exportURL url) (oauth sheet) xml
          if status == status201
            then return SheetDB.Success
            else return SheetDB.Failure
        Nothing -> return SheetDB.Failure

update :: Sheet -> Row -> IO Outcome
update sheet toRow = do
  -- print "================================update Row==================================="
  puturl <- runMaybeT $ do
     jsonValueForm <- MaybeT $ getJson (T.unpack (at idKey toRow) ++ "?alt=json") (oauth sheet)
     MaybeT $ parseEditURL jsonValueForm
  -- print "here in update Row"
  print puturl
  case puturl of
    Just purl -> do
      let cols = idKey :  columns sheet
      if not (isEqualLength toRow cols && isvalid toRow cols)
        then do
          print  cols
          print toRow
          return SheetDB.Failure
        else do
          let xml = rowToXml toRow
          -- print xml
          status <- put purl (oauth sheet) xml
          if status == status200
            then return SheetDB.Success
            else return SheetDB.Failure
    _ -> return SheetDB.Failure


-- Checks if Row has all the Columns of the corresponding spreadsheet
isvalid :: Row -> [ColName] -> Bool
isvalid [] _ = True
isvalid ((key := _):xs) cols = elem key cols && isvalid xs cols

isEqualLength :: [a] -> [b] -> Bool
isEqualLength x y = length x == length y

xmlns = Attr{
         attrKey= unqual "xmlns",
         attrVal="http://www.w3.org/2005/Atom"
         }

xmlnsgsx = Attr{
           attrKey = unqual "xmlns:gsx",
           attrVal = "http://schemas.google.com/spreadsheets/2006/extended"
          }

cellToXml :: Cell -> Element
cellToXml (key := v) = do
  let valstring = case v of
                    ST.String x -> T.unpack x
                    _ -> show v
  unode ("gsx:" ++ T.unpack key) valstring

-- Create xml string from list of key value tuples used for insert method
rowToXml :: Row -> String
rowToXml row = showElement $ add_attrs [xmlns, xmlnsgsx] $ unode "entry" $ map cellToXml row


-- Converts the Aeson Value of the sheet to list of rows.
-- Parses the json tree and iterates through it to produce the list.
parseSheetJson :: A.Value -> IO (Maybe [Row])
parseSheetJson json = return $ parseMaybe parseSheet json
    where parseSheet = withObject "value" $ \obj -> do
            feed <- obj .: "feed"
            entry <- feed .: "entry"
            columns <- getColumnHelper entry

            let parseRows = withObject "object" $ \obj2 ->do
                    let makePairs :: T.Text -> Parser Cell
                        makePairs key = do
                          valObject <- obj2 .: T.append (T.pack "gsx$") key
                          val <- (valObject .: "$t") :: Parser A.Value
                          let cell = case val of
                                AT.String x -> key =: x
                                AT.Bool b -> key =: b
                                AT.Number n -> key =: n
                                _ -> key =: (Nothing :: Maybe ST.Value)
                          return cell
                    currRow <- mapM makePairs columns
                    linkId <- obj2 .: "id"
                    link <- (linkId .: "$t" ) :: Parser A.Value
                    let rowid =  case link of
                                AT.String u -> idKey =: u
                                _ -> idKey =: (Nothing :: Maybe ST.Value)
                    return $ rowid : currRow

            let parseEntry = withArray "array" $ \arr -> return $ map parseRows (V.toList arr)
            rows <-parseEntry entry
            sequence rows

parseEditURL :: A.Value -> IO (Maybe String)
parseEditURL json =
   case parseMaybe parseRow json of
      Just (AT.String url) ->return $ Just $ T.unpack url
      _ ->return Nothing
    where parseRow = withObject "value" $ \obj -> do
                      entry <- obj .: "entry"
                      link <- entry .: "link" :: Parser A.Value
                      let isEdit = withObject "object" $ \ rowobj ->do
                                 rel <- rowobj .: "rel" :: Parser A.Value
                                 return $ rel == AT.String "edit"
                      let parseURLSObjs = withArray "array" $ \arr -> filterM isEdit (V.toList arr)
                      editurlobjlist <- parseURLSObjs link
                      let editurlobj = head editurlobjlist
                      let extract = withObject "object" $ \ obj-> obj .: "href" :: Parser A.Value
                      extract editurlobj

-- Takes spreadsheet key and worksheet id along with parameters to construct URL
formURL :: String -> String-> [(String,String)] -> Maybe URL
formURL key worksheetid params = do
 let template = T.pack urlTemplate
 let keyPattern = T.pack "${key}"
 let worksheetidPattern = T.pack "${worksheetid}"
 let keyUrl = T.replace keyPattern (T.pack key) template
 let keyWorksheetUrl = T.replace worksheetidPattern (T.pack worksheetid) keyUrl
 url <- importURL $ T.unpack keyWorksheetUrl
 return $ foldl add_param url params

 -- forms the post URL without json parameter.
formPostURL :: String -> String -> String
formPostURL key worksheetid = T.unpack $ T.replace (T.pack "${key}") (T.pack key) (T.pack urlTemplate)

getJson url client = do
 urlJSON <- get url client
 let jsonValueForm = A.decode urlJSON:: Maybe A.Value
 -- print jsonValueForm
 return jsonValueForm


getColumns :: A.Value -> Parser [ColName]
getColumns = withObject "value" $ \obj -> do
   feed <- obj .: "feed"
   entry <- feed .: "entry"
   getColumnHelper entry


getColumnHelper  =withArray "array" $ \arr -> do
 let obj = V.head arr
 let extract = withObject "object" $ \obj -> return $ HM.keys obj
 allkeys <- extract obj
 let columns = filter (T.isPrefixOf (T.pack "gsx$")) allkeys
 return $ map (T.drop 4) columns
