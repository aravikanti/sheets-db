{-# LANGUAGE OverloadedStrings #-}
module GoogleRequest
  (
      get
    , delete
    , post
  )where

import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import Network.Google.OAuth2
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L


scopes = ["https://spreadsheets.google.com/feeds"]

createToken client = getAccessToken client scopes (Just "./key.txt")

authorize token request = request
      { requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ token)] }

-- get :: String -> OAuth2Client -> IO L8.ByteString
get url client= do
  token <- createToken client
  request <- parseUrl url
  manager <- newManager tlsManagerSettings
  res <- httpLbs (authorize token request) manager
  -- print res
  return $ responseBody res

delete url client = do
  token <- createToken client
  request <- parseUrl url
  let delReq = request{
                  method = "DELETE"
                }
  manager <- newManager tlsManagerSettings
  res <- httpLbs (authorize token delReq) manager
  return $ responseStatus  res

-- post::String -> OAuth2Client ->String -> IO Status
-- $ "<?xml version='1.0' encoding='utf-8'?>" ++,
--("Content-Length",B.toLazyByteString $ B.int8 $ length postData)
post url client postData = do
  print "inside post request"
  -- print postData
  token <- createToken client
  request <- parseUrl url

  let pd = B.toLazyByteString $ B.stringUtf8  $ "<?xml version='1.0' encoding='utf-8'?>" ++ postData
  print pd
  print url
  let req = request{
                    method = "POST"
                  , requestHeaders = [("Content-Type", "application/atom+xml"),
                                      ("Content-Length", B8.pack $ show $ length $ "<?xml version='1.0' encoding='utf-8'?>" ++ postData)]
                  , requestBody = RequestBodyLBS pd
                  }
  let postReq = authorize token postReq
  -- print postReq
  -- manager <- newManager tlsManagerSettings
  print "sending request"
  res <- withManager $ httpLbs req
  -- res <- httpLbs postReq manager
  print  res
  return $ responseStatus  res
