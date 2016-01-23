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
  ( access
  , Selection(..), Query(..), Order(..), query
  , find, remove, insert, update
  , Selector, everyting, (~>),(~>=), (~<=), (~<), (~=), (~&&~), (~||~)
  ) where

import           Control.Monad             (filterM, when)
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                as A
import           Data.Aeson.Types          as AT
import qualified Data.ByteString.Char8     as B8
import           Data.Char                 (toLower)
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Network.Google.OAuth2
import           Network.HTTP.Types        hiding (Query)
import           Network.URL
import           SheetTypes                as ST
import           Text.XML.Light
import           Text.XML.Light.Types
import           Prelude                   hiding (log)
import           GoogleRequest
import           API                       hiding (URL)

urlTemplate = "https://spreadsheets.google.com/feeds/list/${key}/${worksheetid}/private/full" :: String

--oauth :: Sheet -> OAuth2Client
--oauth sheet = OAuth2Client (cid sheet) (csecret sheet)

access :: String -> String -> String -> String -> API Sheet
-- ^ get an access to the sheet for the "cont" block, temporarily injecting the token
access keyf worksheetidf clientId clientSecret = do
    let clientf = OAuth2Client clientId clientSecret
    
    urlf  <- formURL keyf worksheetidf [("alt","json")]
    token <- io (createToken clientf)
    jsonf <- getJson token (exportURL urlf)

    let cols  = AT.parseMaybe getColumns jsonf

    case cols of
        Just cols ->
            return Sheet 
              { key         = keyf
              , worksheetId = worksheetidf
              , columns     = cols
              , cid         = clientId
              , csecret     = clientSecret
              }

        Nothing ->
            die ["Cannot parse columns:", show cols]


-- ^ Selection
data Selector 
  = Gtr   ColName ST.Value
  | Gteqr ColName ST.Value
  | Ltr   ColName ST.Value
  | Lteqr ColName ST.Value
  | Eqr   ColName ST.Value
  
  | And   Selector Selector
  | Or    Selector Selector
  
  | Everyting
  deriving (Show, Eq)

(~>), (~>=), (~<), (~<=), (~=) :: (Val v) => ColName -> v -> Selector
k ~>  v = Gtr   k (val v)
k ~>= v = Gteqr k (val v)
k ~<  v = Ltr   k (val v)
k ~<= v = Lteqr k (val v)
k ~=  v = Eqr   k (val v)

(~&&~), (~||~) :: Selector -> Selector -> Selector
(~&&~) = And
(~||~) = Or

everyting :: Selector
everyting = Everyting

data Selection = Select
  { selector :: Selector
  , sheet    :: Sheet
  }
  deriving (Show, Eq)
-- Selects rows in spreadsheet that match selector

-- Query

-- Use 'Select' to create a basic query with defaults, then modify if desired. For example, @(Select sel sheet) {limit = 10}@
data Query = Query 
  { selection :: Selection
  , skip      :: Int  -- Number of initial matching documents to skip. Default = 0
  , limit     :: Int -- Maximum number of documents to return, 0 = no limit. Default = 0
  , sort      :: Order  -- Sort results by this order, NoOrder = no sort. Default = NoOrder
  } deriving (Show, Eq)


data Order 
  = Order 
      { colName :: ColName
      , reverse :: Bool
      }
  | NoOrder -- Indicates no specific order is required
  deriving (Show, Eq)


query :: Selector -> Sheet -> Query
query sel sheet = Query (Select sel sheet) 0 0 NoOrder

makeQueryUrl :: Query -> API String
makeQueryUrl q = do
  let 
    spreadsheet  = sheet $ selection q
    order        = sort q
    param1       = [("alt","json")] :: [(String,String)]
    selectString = selectorQueryUrl $ selector $ selection q
    encWithSp    = encString False ok_param
    param2       = [("sq", selectString) | not (null selectString)] 
                ++ param1
    
  let
    params = case order of
      NoOrder       -> param2
      Order col rev -> param2 ++ 
        [ ("orderby", "column:" ++ T.unpack col)
        , ("reverse", map toLower (show rev))
        ]
  
  url <- formURL 
    (key         spreadsheet)
    (worksheetId spreadsheet)
    params

  return (exportURL url)

-- Converts Selector to String. example: Gtr "age" 25 -> age > 25
selectorQueryUrl :: Selector -> String
selectorQueryUrl (Gtr   colName value) = T.unpack colName ++ " > "  ++ show value
selectorQueryUrl (Gteqr colName value) = T.unpack colName ++ " >= " ++ show value
selectorQueryUrl (Ltr   colName value) = T.unpack colName ++ " < "  ++ show value
selectorQueryUrl (Lteqr colName value) = T.unpack colName ++ " <= " ++ show value
selectorQueryUrl (Eqr   colName value) = T.unpack colName ++ " = "  ++ show value
selectorQueryUrl (And   sel1    sel2)  = selectorQueryUrl sel1 ++ " and " ++ selectorQueryUrl sel2
selectorQueryUrl (Or    sel1    sel2)  = selectorQueryUrl sel1 ++ " or "  ++ selectorQueryUrl sel2
selectorQueryUrl Everyting = ""

-- Encodes Query parameter
encodeSelectorQueryUrl :: String -> String
encodeSelectorQueryUrl q = B8.unpack $ urlEncode True $ B8.pack q

idKey = T.pack "id"

validate :: Query -> Bool
validate q = skip q >= 0 && limit q >= 0

find :: Query  -> API [Row]
find q = do
    let sel = selection q
    
    when (not $ validate q) $ do
        die ["Query skip or Query limit may be invalid."]

    let spreadsheet = sheet sel
    
    url           <- makeQueryUrl q
    token         <- io $ tokenOf (sheet sel)
    jsonValueForm <- getJson token url
    rs            <- parseSheetJson jsonValueForm

    let 
      rows
        | skip q == 0 && limit q == 0 = rs
        | skip q >  0 && limit q == 0 = drop (skip  q) rs
        | skip q == 0 && limit q >  0 = take (limit q) rs
        | otherwise = take (limit q) $ drop (skip q) rs
    
    return rows
      


-- Used to send a delete request to the Google Sheet.
remove :: Sheet -> Row -> API ()
remove sheet r = do
    url    <- look idKey r
    token  <- io $ tokenOf sheet
    status <- delete token (T.unpack url)
    
    when (status /= status200) $ do
        die ["Status returned:", show status]

  -- print status

-- Adds a new row to google sheet after validation of data to be inserted
insert :: Row -> Sheet -> API ()
insert rawRow sheet = do
    let cols = columns sheet
    
    when (not $ isEqualLength rawRow cols) $ do
        die ["Length is different"]

    when (not $ isvalid rawRow cols) $ do
        die ["Columns mismatch, required", show cols, "got", show rawRow]

    let xml = rowToXml rawRow
    
    url    <- formURL (key sheet) (worksheetId sheet) []
    token  <- io $ tokenOf sheet
    status <- post token (exportURL url) xml
    
    when (status /= status201) $ do
        die ["Status returned: ", show status]

update :: Sheet -> Row -> API ()
update sheet toRow = do
    val           <- look idKey toRow
    token         <- io $ tokenOf sheet
    jsonValueForm <- getJson token (T.unpack val ++ "?alt=json")
    url           <- parseEditURL jsonValueForm

    let cols = idKey : columns sheet

    when (not $ isEqualLength toRow cols) $ do
        die ["Length is different"]

    when (not $ isvalid toRow cols) $ do
        die ["Columns mismatch, required", show cols, "got", show toRow]

    let xml = rowToXml toRow
    
    token  <- io $ tokenOf sheet
    status <- put token url xml

    when (status /= status200) $ do
        die ["Status returned: ", show status]

-- Checks if Row has all the Columns of the corresponding spreadsheet
isvalid :: Row -> [ColName] -> Bool
isvalid row cols = 
    flip all row $ \(key := _) ->
        key `elem` cols

isEqualLength :: [a] -> [b] -> Bool
isEqualLength x y = length x == length y

xmlns = Attr
  { attrKey = unqual "xmlns"
  , attrVal = "http://www.w3.org/2005/Atom"
  }

xmlnsgsx = Attr
  { attrKey = unqual "xmlns:gsx"
  , attrVal = "http://schemas.google.com/spreadsheets/2006/extended"
  }

cellToXml :: Cell -> Element
cellToXml (key := v) = do
  let
    valstring = case v of
      ST.String x -> T.unpack x
      _           -> show v
  
  unode ("gsx:" ++ T.unpack key) valstring

-- Create xml string from list of key value tuples used for insert method
rowToXml :: Row -> String
rowToXml row
  = showElement 
  $ add_attrs [xmlns, xmlnsgsx]
  $ unode "entry"
  $ map cellToXml row


-- Converts the Aeson Value of the sheet to list of rows.
-- Parses the json tree and iterates through it to produce the list.
parseSheetJson :: A.Value -> API [Row]
parseSheetJson json =
    case parseMaybe parseSheet json of
        Just rows -> return rows
        Nothing   -> die ["Parse error", take 100 $ show json]
    where
      parseSheet = withObject "value" $ \obj -> do
          feed    <- obj  .: "feed"
          entry   <- feed .: "entry"
          columns <- getColumnHelper entry

          let parseRows = withObject "object" $ \obj2 -> do
                let 
                  makePairs :: T.Text -> Parser Cell
                  makePairs key = do
                    valObject <-  obj2      .: T.append (T.pack "gsx$") key
                    val       <- (valObject .: "$t") :: Parser A.Value
                    let
                      cell = case val of
                        AT.String x -> key =: x
                        AT.Bool   b -> key =: b
                        AT.Number n -> key =: n
                        _           -> key =: (Nothing :: Maybe ST.Value)
                    return cell

                currRow <- mapM makePairs columns
                linkId  <-  obj2   .: "id"
                link    <- (linkId .: "$t" ) :: Parser A.Value
                
                let
                  rowid = case link of
                    AT.String u -> idKey =:  u
                    _           -> idKey =: (Nothing :: Maybe ST.Value)
                
                return $ rowid : currRow

          let parseEntry = withArray "array" $ \arr ->return $ map parseRows (V.toList arr)
          rows <-parseEntry entry
          sequence rows

parseEditURL :: A.Value -> API String
parseEditURL json =
   case parseMaybe parseRow json of
      Just (AT.String url) -> return (T.unpack url)
      _                    -> die ["Cannot parse url:", take 100 $ show json]
  where
    parseRow = withObject "value" $ \obj -> do
        entry <- obj   .: "entry"
        link  <- entry .: "link" :: Parser A.Value
        
        let 
          isEdit = withObject "object" $ \rowobj -> do
            rel <- rowobj .: "rel" :: Parser A.Value
            return $ rel == AT.String "edit"

          parseURLSObjs = withArray "array" $ \arr ->
            filterM isEdit (V.toList arr)

        editurlobjlist <- parseURLSObjs link

        let 
          editurlobj = head editurlobjlist
          extract    = withObject "object" $ \obj ->
            obj .: "href" :: Parser A.Value
        
        extract editurlobj

-- Takes spreadsheet key and worksheet id along with parameters to construct URL
formURL :: String -> String -> [(String,String)] -> API URL
formURL key worksheetid params = do
    let template           = T.pack urlTemplate
    let keyPattern         = T.pack "${key}"
    let worksheetidPattern = T.pack "${worksheetid}"
    let keyUrl             = T.replace keyPattern (T.pack key) template
    let keyWorksheetUrl    = T.replace worksheetidPattern (T.pack worksheetid) keyUrl

    case importURL $ T.unpack keyWorksheetUrl of
        Just url ->  
            return $ foldl add_param url params

        Nothing ->
            die ["Not an URL:", T.unpack keyWorksheetUrl]

 -- forms the post URL without json parameter.
formPostURL :: String -> String -> String
formPostURL key worksheetid = T.unpack $ T.replace (T.pack "${key}") (T.pack key) (T.pack urlTemplate)

getJson :: String -> String -> API AT.Value
getJson token url = do
    urlJSON <- get token url
    let jsonValueForm = A.decode urlJSON :: Maybe A.Value
    case jsonValueForm of
        Just value -> return value
        Nothing    -> die ["Not a valid json:", take 100 $ show jsonValueForm]

getColumns :: A.Value -> Parser [ColName]
getColumns = withObject "value" $ \obj -> do
    feed  <- obj  .: "feed"
    entry <- feed .: "entry"
    getColumnHelper entry


getColumnHelper  = withArray "array" $ \arr -> do
    let 
      obj     = V.head arr
      extract = withObject "object" (return . HM.keys)

    allkeys <- extract obj

    let columns = flip filter allkeys $ T.isPrefixOf (T.pack "gsx$")
    
    return $ map (T.drop 4) columns
