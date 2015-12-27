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
import           Network.HTTP.Types        (Status, urlEncode)
import           Network.URL
import           SheetTypes                as ST
import           Text.XML.Light
import           Text.XML.Light.Types

urlTemplate = "https://spreadsheets.google.com/feeds/list/${key}/od6/private/full" :: String

oauth :: Sheet -> OAuth2Client
oauth sheet = OAuth2Client (cid sheet) (csecret sheet)

access :: String -> String -> String -> IO (Maybe Sheet)
access keyf clientId clientSecret = runMaybeT $ do
  let clientf = OAuth2Client clientId clientSecret
  urlf <- MaybeT $ return $ formURL keyf [("alt","json")]
  jsonf <- MaybeT $ getJson (exportURL urlf) clientf
  cols <- MaybeT $ return $ AT.parseMaybe getColumns jsonf
  return Sheet {
                  key = keyf,
                  url = exportURL urlf,
                  columns = cols,
                  cid = clientId,
                  csecret = clientSecret
               }


-- * Selection

data Selection = Select {selector :: Selector, sheet :: Sheet}  deriving (Show, Eq)
-- ^ Selects documents in collection that match selector


class Select aQueryOrSelection where
  select :: Selector -> Sheet -> aQueryOrSelection
  -- ^ 'Query' or 'Selection' that selects documents in collection that match selector. The choice of type depends on use, for example, in @find (select sel col)@ it is a Query, and in @delete (select sel col)@ it is a Selection.

instance Select Selection where
  select = Select

instance Select Query where
  select = query

-- ** Query

-- | Use 'select' to create a basic query with defaults, then modify if desired. For example, @(select sel col) {limit = 10}@
data Query = Query {
  selection :: Selection,
  project   :: Projector,  -- ^ \[\] = all fields. Default = []
  skip      :: Int,  -- ^ Number of initial matching documents to skip. Default = 0
  limit     :: Limit, -- ^ Maximum number of documents to return, 0 = no limit. Default = 0
  sort      :: Order  -- ^ Sort results by this order, [] = no sort. Default = []
  --hint :: Order  -- ^ Force MongoDB to use this index, [] = no hint. Default = []
  } deriving (Show, Eq)


type Projector = Row
type Limit = Int
type Order = Row

query :: Selector -> Sheet -> Query
query sel sheet = Query (Select sel sheet) [] 0 0 []


makeQueryUrl :: Query -> Maybe String
makeQueryUrl q = do
  let spreadsheet = sheet $ selection q
  url <- case selector $ selection q of
    Empty -> formURL (key spreadsheet ) [("alt","json")]
    _ -> formURL (key spreadsheet ) [("alt","json"),("sq", selectorQueryUrl $ selector $ selection q )]
  return $ exportURL url

selectorQueryUrl :: Selector -> String
selectorQueryUrl (Gtr colname value) = T.unpack colname ++ " > " ++ show value
selectorQueryUrl (Gteqr colname value) = T.unpack colname ++ " >= " ++ show value
selectorQueryUrl (Ltr colname value) = T.unpack colname ++ " < " ++ show value
selectorQueryUrl (Lteqr colname value) = T.unpack colname ++ " <= " ++ show value
selectorQueryUrl (And sel1 sel2) = selectorQueryUrl sel1 ++ " and " ++ selectorQueryUrl sel2
selectorQueryUrl (Or sel1 sel2) = selectorQueryUrl sel1 ++ " or " ++ selectorQueryUrl sel2

encodeSelectorQueryUrl :: String -> String
encodeSelectorQueryUrl q = B8.unpack $ urlEncode True $ B8.pack q

idKey = T.pack "id"

find :: Query -> IO (Maybe [Row])
find q = do
  let sel = selection q
  let spreadsheet = sheet sel
  runMaybeT $ do
    url <- MaybeT $ return $ makeQueryUrl q
    jsonValueForm <-MaybeT $ getJson url (oauth spreadsheet)
    MaybeT $ parseSheetJson jsonValueForm


-- Used to send a delete request to the Google Sheet.
remove :: Sheet -> Row -> IO Status
remove sheet r = do
  let url = at idKey r
  -- print $ T.unpack url
  delete (T.unpack url) (oauth sheet)
  -- print status

-- Adds a new row to google sheet after validation of data to be inserted
--insert :: Row -> Sheet -> IO (Maybe Status)
insert rawRow sheet =do
  let cols = columns sheet
  if not (isEqualLength rawRow cols && isvalid rawRow cols)
    then
      -- print  cols
      -- print rawRow
      return Nothing
    else do
      let xml = makexml rawRow
      status <- post (formPostURL ( key sheet)) (oauth sheet) xml
      return $ Just status

update :: Sheet -> Row -> IO (Maybe Status)
update sheet toRow = do
  -- print "================================update Row==================================="
  puturl <- runMaybeT $ do
     jsonValueForm <- MaybeT $ getJson (T.unpack (at idKey toRow) ++ "?alt=json") (oauth sheet)
     MaybeT $ parseEditURL jsonValueForm
  -- print "here in update Row"
  print puturl
  case puturl of
    Just purl -> do
      let cols = map (T.drop 4) $ columns sheet
      if not (isEqualLength toRow cols && isvalid toRow cols)
        then do
          print  cols
          print toRow
          return Nothing
        else do
          let xml = makexml toRow
          print xml
          status <- put purl (oauth sheet) xml
          return $ Just status
    _ -> return Nothing



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



makexmlrow :: Cell -> Element
makexmlrow (key := value) = do
  let valstring = show value
  unode ("gsx:" ++ T.unpack key) valstring

-- Create xml string from list of key value tuples used for insert method
makexml :: Row -> String
makexml rawRows = showElement $ add_attrs [xmlns, xmlnsgsx] $ unode "entry" $ map makexmlrow rawRows


-- Converts the Aeson Value of the sheet to list of rows.
-- Parses the json tree and iterates through it to produce the list.
parseSheetJson :: A.Value -> IO (Maybe [Row])
parseSheetJson json = return $ parseMaybe parseSheet json
    where parseSheet = withObject "value" $ \obj -> do
            feed <- obj .: "feed"
            entry <- feed .: "entry"
            columns <- getColumnHelper entry

            let parseRows = withObject "object" $ \obj ->do
                    -- cols <- getColumnNames entry
                    let makePairs :: T.Text -> Parser Cell
                        makePairs key = do
                          valObject <- obj .: key
                          val <- (valObject .: "$t") :: Parser A.Value
                          let cell = case val of
                                AT.String x -> T.drop 4 key =: x
                                AT.Bool b -> T.drop 4 key =: b
                                AT.Number n -> T.drop 4 key =: n
                                _ -> T.drop 4 key =: (Nothing :: Maybe ST.Value)
                          return cell
                    currRow <- mapM makePairs columns
                    linkId <- obj .: "id"
                    link <- (linkId .: "$t" ) :: Parser A.Value
                    let rowid =  case link of
                                AT.String u -> idKey =: u
                                _ -> idKey =: (Nothing :: Maybe ST.Value)
                    return $ rowid : currRow

            let parseEntry = withArray "array" $ \arr -> return $ map parseRows (V.toList arr)
            rows <- parseEntry entry
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

formURL :: String -> [(String,String)] -> Maybe URL
formURL key params = do
 -- do concatenation to make url
 let template = T.pack urlTemplate
 let keyPattern = T.pack "${key}"
 let sUrl = T.replace keyPattern (T.pack key) template
 url <- importURL $ T.unpack sUrl
 return $ foldl add_param url params -- ("alt","json")

 -- forms the post URL without json parameter.
formPostURL :: String -> String
formPostURL key = T.unpack $ T.replace (T.pack "${key}") (T.pack key) (T.pack urlTemplate)

getJson url client = do
 urlJSON <- get url client
 let jsonValueForm = A.decode urlJSON:: Maybe A.Value
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
