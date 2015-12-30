{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GoogleRequest
  (
      get
    , delete
    , post
    , put
  )where

import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import Network.Google.OAuth2
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)



scopes = ["https://spreadsheets.google.com/feeds"]

createToken client = getAccessToken client scopes (Just "./key.txt")

authorize token request = request
      { requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ token)] }

get :: String -> OAuth2Client -> IO L.ByteString
get url client= runResourceT $ do
  token <- liftIO $ createToken client
  request <- parseUrl url
  manager <- liftIO $ newManager tlsManagerSettings
  res <- httpLbs (authorize token request) manager
  -- print res
  return $ responseBody res

delete url client = runResourceT $ do
  token <- liftIO $ createToken client
  request <- parseUrl url
  let delReq = request{
                  method = "DELETE"
                }
  manager <- liftIO $ newManager tlsManagerSettings
  res <- httpLbs (authorize token delReq) manager
  return $ responseStatus  res

post url client postData = runResourceT $ do
  token <- liftIO $ createToken client
  request <- parseUrl url
  let pd = B.toLazyByteString $ B.stringUtf8 postData
  -- print pd
  -- print url
  let req = request{
                    method = "POST"
                  , requestHeaders = [("content-type", "application/atom+xml"),
                                      ("Gdata-version", "1.0"),
                                      (hAuthorization, B8.pack $ "Bearer " ++ token)]
                  , requestBody = RequestBodyLBS pd
                  }
  -- print req
  manager <- liftIO $ newManager tlsManagerSettings
  res <- httpLbs req manager
  -- print  res
  return $ responseStatus  res


put url client postData = runResourceT $ do
  token <- liftIO $ createToken client
  request <- parseUrl url
  let pd = B.toLazyByteString $ B.stringUtf8 postData
  -- print pd
  -- print url
  let req = request{
                    method = "PUT"
                  , requestHeaders = [("content-type", "application/atom+xml"),
                                      ("Gdata-version", "1.0"),
                                      (hAuthorization, B8.pack $ "Bearer " ++ token)]
                  , requestBody = RequestBodyLBS pd
                  }
  -- print req
  manager <- liftIO $ newManager tlsManagerSettings
  res <- httpLbs req manager
  -- print  res
  return $ responseStatus  res
