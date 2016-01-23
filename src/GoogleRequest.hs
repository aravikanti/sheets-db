
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GoogleRequest
  ( get
  , delete
  , post
  , put
  , io
  ) where

import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Builder      as B
import qualified Data.ByteString.Lazy         as L

import           Network.HTTP.Conduit
import           Network.HTTP.Types           (hAuthorization, Status, Method)
import           Network.Google.OAuth2

import           API

get :: Token -> URL -> API L.ByteString
get token url = do
    request <- parseUrl url
    result  <- runAuthorized token request
    return (responseBody result)

delete :: Token -> URL -> API Status
delete token url = do
    request <- parseUrl url
    result  <- runAuthorized token request { method = "DELETE" }
    return (responseStatus result)

post, put :: Token -> URL -> String -> API Status
post = send "POST"
put  = send "PUT"

send :: Method -> Token -> URL -> String -> API Status
send method token url data_ = do
    request <- parseUrl url

    let packedData = B.toLazyByteString (B.stringUtf8 data_)

    result <- runAuthorized token request
      { method = method
      , requestHeaders = 
          [ ("content-type" , "application/atom+xml")
          , ("Gdata-version", "1.0")
          ]
      , requestBody = RequestBodyLBS packedData
      }

    return (responseStatus result)
