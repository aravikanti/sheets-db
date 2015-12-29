{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SheetTypes
  (
  -- * Row
  Row, look, lookup, valueAt, at, include, exclude,
  -- * Cell
  Cell(..), (=:), (=?),
  ColName,
  Value(..), Val(..), fval, cast, typed, typeOfVal,
  Sheet(..),
  p
  ) where

import           Control.Monad.Identity
import           Data.Int
import           Data.List              (find, findIndex)
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text              as T
import           Data.Time.Clock        (UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Time.Format       ()
import           Data.Typeable          hiding (cast)
import           Network.Google.OAuth2  (OAuth2Client)
import           Prelude                hiding (lookup)

type Row = [Cell]
-- ^ A row is a list of 'Cell's

look :: (Monad m) => ColName -> Row -> m Value
-- ^ Value of cell in row, or fail (Nothing) if cell not found
look k row = maybe notFound (return . value) (find ((k ==) . colname) row) where
    notFound = fail $ "expected " ++ show k ++ " in " ++ show row

lookup :: (Val v, Monad m) => ColName -> Row -> m v
-- ^ Lookup value of cell in row and cast to expected type. Fail (Nothing) if cell not found or value not of expected type.
lookup k row = cast =<< look k row

valueAt :: ColName -> Row -> Value
-- ^ Value of cell in row. Error if missing.
valueAt k = runIdentity . look k

at :: forall v. (Val v) => ColName -> Row -> v
-- ^ Typed value of cell in row. Error if missing or wrong type.
at k row = fromMaybe err (lookup k row)  where
  err = error $ "expected (" ++ show k ++ " :: " ++ show (typeOf (undefined :: v)) ++ ") in " ++ show row

include :: [ColName] -> Row -> Row
-- ^ Only include cells of row in label list
include keys row = mapMaybe (\k -> find ((k ==) . colname) row) keys

exclude :: [ColName] -> Row -> Row
-- ^ Exclude cells from row in label list
exclude keys = filter (\(k := _) -> notElem k keys)


type ColName = T.Text
-- ^ The name of a Cell
p = T.pack
infix 0 :=, =:, =?

data Cell = (:=) {colname :: !ColName, value :: Value}  deriving (Typeable, Eq)
-- colname is label

(=:) :: (Val v) => ColName -> v -> Cell
-- ^ cell with given label and typed value
k =: v = k := val v

(=?) :: (Val a) => ColName -> Maybe a -> Row
-- ^ If Just value then return one cell row, otherwise return empty row
k =? ma = maybeToList (fmap (k =:) ma)

instance Show Cell where
    showsPrec d (k := v) = showParen (d > 0) $ showString (' ' : T.unpack k) . showString ": " . showsPrec 1 v


data Value =
    Number !Scientific |
    String !T.Text |
    Bool !Bool |
    UTC !UTCTime |
    Null
  deriving (Typeable, Eq)

instance Show Value where
    showsPrec d = fval (showsPrec d)



fval :: (forall a . (Val a) => a -> b) -> Value -> b
-- ^ Apply generic function to typed value
fval f v = case v of
  Number x -> f x
  String x -> f x
  Bool x -> f x
  UTC x -> f x
  Null -> f (Nothing :: Maybe Value)

-- | Haskell types of this class correspond to Cell value types
class (Typeable a, Show a, Eq a) => Val a where
    val :: a -> Value
    cast' :: Value -> Maybe a


instance Val T.Text where
    val = String
    cast' (String x) = Just x
    cast' _ = Nothing

instance Val String where
    val = String . T.pack
    cast' (String x) = Just (T.unpack x)
    cast' _ = Nothing

instance Val Scientific where
    val = Number
    cast' (Number x) = Just x
    cast' _ = Nothing

instance Val Float where
  val = Number . fromFloatDigits
  cast' (Number x) = Just $ toRealFloat x
  cast' _ = Nothing

instance Val Double where
  val = Number . fromFloatDigits
  cast' (Number x) = Just $ toRealFloat x
  cast' _ = Nothing

instance Val Bool where
  val = Bool
  cast' (Bool x) = Just x
  cast' _ = Nothing

instance Val UTCTime where
    val = UTC . posixSecondsToUTCTime . roundTo (1/1000) . utcTimeToPOSIXSeconds
    cast' (UTC x) = Just x
    cast' _ = Nothing

instance Val POSIXTime where
    val = UTC . posixSecondsToUTCTime . roundTo (1/1000)
    cast' (UTC x) = Just (utcTimeToPOSIXSeconds x)
    cast' _ = Nothing

instance Val (Maybe Value) where
    val Nothing = Null
    val (Just v) = v
    cast' Null = Just Nothing
    cast' v = Just (Just v)


roundTo :: (RealFrac a) => a -> a -> a
-- ^ Round second number to nearest multiple of first number. Eg: roundTo (1/1000) 0.12345 = 0.123
roundTo mult n = fromIntegral (round (n / mult)) * mult


cast :: forall m a. (Val a, Monad m) => Value -> m a
-- ^ Convert Value to expected type, or fail (Nothing) if not of that type
cast v = maybe notType return (cast' v) where
  notType = fail $ "expected " ++ show (typeOf (undefined :: a)) ++ ": " ++ show v

typed :: (Val a) => Value -> a
-- ^ Convert Value to expected type. Error if not that type.
typed = runIdentity . cast

typeOfVal :: Value -> TypeRep
-- ^ Type of typed value
typeOfVal = fval typeOf


data Sheet = Sheet {
                        key     :: String
                      , url     :: String
                      , columns :: [ColName]
                      , cid  :: String
                      , csecret :: String
                   } deriving (Show,Eq)
