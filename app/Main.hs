

module Main where


import SheetDB
import SheetTypes
import qualified Data.Text                 as T
-- import           Data.Scientific

-- clientid = "1086783968140-bupmbbqo8chsm2tej5ldgfci496a1lvr.apps.googleusercontent.com"
-- clientsecret = "cCle7aP41aEF9fNlVkUoUY69"
-- clientid = "1086783968140-onqtrp6v9dccglnokffuobpna1sk1eo7.apps.googleusercontent.com"
-- clientsecret = "zlpVYkY-_Dy5cruMS8YE7mbX"
clientid = "1086783968140-knfg08qu9onnn5b485veaskt2flr0loa.apps.googleusercontent.com"
clientsecret ="wRktcD6xif1_z6Xv-RhCatBB"


dd :: Row
dd= [p "name" =:  "papaya",
         p "category" =: "fruit",
         p "healthiness" =: "adequate",
          p "type" =: (3 :: Float)]

main = do
    mySheet <- access "1hIEq4AAauzI8INelQRIvgxBhmzX44qAB_1QQpFJQ2Xo" clientid clientsecret

    case mySheet of
      Just sheet -> do
         rows <- find (query ((p "type" ~< (2 :: Float)) ~||~ (p "category" ~= "meat" ) ) sheet)
         print rows
        --  status <- insert dd sheet
        --  print status
        --  return ()
        --  case rows of
        --    Just rs -> do
        --      let upd = (T.pack "id" =: (at (T.pack "id") (last rs) :: T.Text)) : dd
        --      status <- update sheet upd
        --      print status
        --    Nothing -> putStrLn "Nothing_there"
      Nothing -> putStrLn "Nothing_here"
