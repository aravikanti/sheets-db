module Main where
{-# LANGUAGE OverloadedStrings #-}

import Lib
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import Data.Aeson.Types
import Network.URL
import Control.Applicative    ((<$>), (<*>))

-- test x = case formURL x of
--            Just u  -> print $ exportURL u
--            Nothing -> print "invalid URL"

main = do
    rows <- fetch "https://spreadsheets.google.com/feeds/list/1hIEq4AAauzI8INelQRIvgxBhmzX44qAB_1QQpFJQ2Xo/od6/public/values?alt=json"
    putStrLn $ show rows
