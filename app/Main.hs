module Main where
{-# LANGUAGE OverloadedStrings #-}

import Lib
import qualified Data.Text                 as T

-- clientid = "1086783968140-bupmbbqo8chsm2tej5ldgfci496a1lvr.apps.googleusercontent.com"
-- clientsecret = "cCle7aP41aEF9fNlVkUoUY69"
-- clientid = "1086783968140-onqtrp6v9dccglnokffuobpna1sk1eo7.apps.googleusercontent.com"
-- clientsecret = "zlpVYkY-_Dy5cruMS8YE7mbX"
clientid = "1086783968140-knfg08qu9onnn5b485veaskt2flr0loa.apps.googleusercontent.com"
clientsecret ="wRktcD6xif1_z6Xv-RhCatBB"


dd :: [(T.Text, CellType)]
dd= [(T.pack "name",Lib.String $ T.pack  "papaya"),
         (T.pack "category",Lib.String $ T.pack "fruit"),
         (T.pack "healthiness",Lib.String $ T.pack "adequate"),
          (T.pack "type", Lib.Number 3)]

main = do
    mySheet <- initialize "1hIEq4AAauzI8INelQRIvgxBhmzX44qAB_1QQpFJQ2Xo" clientid clientsecret

    case mySheet of
      Just sheet -> do
         rows <- getRows sheet
        --  print rows
        --  status <- addRow dd sheet
        --  print status
        --  return ()
         case rows of
           Just rs -> do
             status <- updateRow sheet (head rs) dd
             print status
           Nothing -> putStrLn "Nothing_there"
      Nothing -> putStrLn "Nothing_here"
