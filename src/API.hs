
module API where

import           Control.Monad.Reader
import           Control.Monad.Writer.Strict
import           Control.Monad.Except

import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Builder      as B
import qualified Data.ByteString.Lazy         as L

import           Data.Time.Clock

import           Network.HTTP.Conduit
import           Network.HTTP.Types           (hAuthorization, Status, Method)
import           Network.Google.OAuth2

import           Prelude                   hiding (log)

type API 
  = ExceptT  String 
  ( WriterT [String] 
  ( ReaderT  Manager
    IO ))

type Token  = OAuth2Token
type Client = OAuth2Client
type URL    = String

useAPIWith :: API a -> Manager -> IO (Either String a, [String])
useAPIWith api manager =
    runWriterT (runExceptT api) `runReaderT` manager

runAPI :: API a -> IO (Either String a)
runAPI api = do
    manager        <- newManager tlsManagerSettings
    (result, logs) <- api `useAPIWith` manager
    mapM_ putStrLn logs
    return result

io :: IO a -> API a
io = liftIO

runAuthorized :: Token -> Request -> API (Response L.ByteString)
runAuthorized token request = do
    manager <- ask
    io $ authorize token request `httpLbs` manager

authorize token request = request 
    { requestHeaders = 
      [ (hAuthorization, B8.pack $ "Bearer " ++ token) ] 
      ++ requestHeaders request }

log :: String -> API ()
log line = do
    time <- io getCurrentTime
    tell [show time ++ ": " ++ line ++ "\n"]

die :: [String] -> API a
die pack = do
    let msg = unwords pack
    log $ unwords ["ERROR:", msg]
    throwError msg
