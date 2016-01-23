{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SheetTypes
  ( -- * Row
    Row, look, include, exclude
  , -- * Cell
    Cell(..), (=:), (=?)
  , ColName
  , Value(..), Val(..), fval, typeOfVal
  , Sheet(..)
  , p
  , tokenOf
  , createToken
  ) where

import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Int
import           Data.List              (find, findIndex)
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text              as T
import           Data.Time.Clock        (UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Time.Format       ()
import           Data.Typeable          hiding (cast)
import           Network.HTTP.Types     (hAuthorization, Status, Method)
import           Network.Google.OAuth2  (OAuth2Client(OAuth2Client), getAccessToken)
import           Prelude                hiding (lookup)

import           API

-- Data representation taken from bson

type Row = [Cell]
-- ^ A row is a list of 'Cell's

look :: Val v => ColName -> Row -> API v
-- ^ Value of cell in row, or fail (Nothing) if cell not found
look k row = do
    case find (eq colname k) row of
        Just it -> cast (value it)
        Nothing -> die ["expected", show k, "in", show row]

eq proj x thing = x == proj thing

include, exclude :: [ColName] -> Row -> Row
-- ^ Only include cells of row in label list
include keys = filter ((`elem` keys)    . colname)
-- ^ Exclude cells from row in label list
exclude keys = filter ((`notElem` keys) . colname)


p = T.pack

infix 0 :=, =:, =?

data Cell 
  = (:=) 
    { colname :: !ColName
    , value   :: Value
    }
  deriving (Typeable, Eq)
-- colname is label

(=:) :: (Val v) => ColName -> v -> Cell
-- ^ cell with given label and typed value
k =: v = k := val v

(=?) :: (Val a) => ColName -> Maybe a -> Row
-- ^ If Just value then return one cell row, otherwise return empty row
k =? ma = maybeToList (fmap (k =:) ma)

instance Show Cell where
    showsPrec d (k := v) = 
        showParen (d > 0)
          $ showString (' ' : T.unpack k) 
          . showString ": " 
          . showsPrec 1 v

data Value
  = Number !Scientific
  | String !T.Text 
  | Bool   !Bool
  | UTC    !UTCTime 
  | Null
  deriving (Typeable, Eq)

instance Show Value where
    showsPrec d = fval (showsPrec d)

fval :: (forall a . (Val a) => a -> b) -> Value -> b
-- ^ Apply generic function to typed value
fval f v = case v of
  Number x -> f x
  String x -> f x
  Bool   x -> f x
  UTC    x -> f x
  Null     -> f (Nothing :: Maybe Value)

-- | Haskell types of this class correspond to Cell value types
class (Typeable a, Show a, Eq a) => Val a where
    val   :: a -> Value
    cast  :: Value -> API a

instance Val T.Text where
    val = String
    
    cast  (String x) = return x
    cast   it        = die ["Not a string:", show it]

instance Val String where
    val = String . T.pack
    
    cast  (String x) = return (T.unpack x)
    cast   it        = die ["Not a string:", show it]

instance Val Scientific where
    val = Number

    cast  (Number x) = return x
    cast   it        = die ["Not a number:", show it]

instance Val Float where
  val = Number . fromFloatDigits

  cast  (Number x) = return (toRealFloat x)
  cast   it        = die ["Not a number:", show it]

instance Val Double where
  val = Number . fromFloatDigits
  
  cast  (Number x) = return (toRealFloat x)
  cast   it        = die ["Not a number:", show it]

instance Val Bool where
  val = Bool

  cast  (Bool x) = return x
  cast   it      = die ["Not a boolean:", show it]

instance Val UTCTime where
  val = UTC . posixSecondsToUTCTime . roundTo (1/1000) . utcTimeToPOSIXSeconds
  
  cast  (UTC x) = return x
  cast   it     = die ["Not a utc time:", show it]

instance Val POSIXTime where
  val = UTC . posixSecondsToUTCTime . roundTo (1/1000)
  
  cast  (UTC x) = return (utcTimeToPOSIXSeconds x)
  cast   it     = die ["Not a utc time:", show it]

instance Val (Maybe Value) where
  val  Nothing = Null
  val (Just v) = v

  cast  Null = return Nothing
  cast  v    = return (Just v)

roundTo :: (RealFrac a) => a -> a -> a
-- ^ Round second number to nearest multiple of first number. Eg: roundTo (1/1000) 0.12345 = 0.123
roundTo mult n = fromIntegral (round (n / mult)) * mult

--cast :: forall a. (Val a) => Value -> API a
---- ^ Convert Value to expected type, or fail (Nothing) if not of that type
--cast v = 
--    maybe (throwError uncastable) 
--      return (cast  v)
--  where
--    uncastable = "expected " ++ show (typeOf (undefined :: a)) ++ ": " ++ show v

typeOfVal :: Value -> TypeRep
-- ^ Type of typed value
typeOfVal = fval typeOf

data Sheet = Sheet 
  { key         :: String
  , worksheetId :: String
  , columns     :: [ColName]
  , cid         :: String
  , csecret     :: String
  } deriving (Show,Eq)

type ColName = T.Text
-- ^ The name of a Cell

tokenOf :: Sheet -> IO String
tokenOf sheet =
    createToken $ OAuth2Client (cid sheet) (csecret sheet)

scopes = ["https://spreadsheets.google.com/feeds"]

createToken client = getAccessToken client scopes (Just "./key.txt")
