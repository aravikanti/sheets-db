

module Main where


import qualified Data.Text  as T
import           SheetDB
import           SheetTypes
-- import           Data.Scientific

-- clientid = "1086783968140-bupmbbqo8chsm2tej5ldgfci496a1lvr.apps.googleusercontent.com"
-- clientsecret = "cCle7aP41aEF9fNlVkUoUY69"
-- clientid = "1086783968140-onqtrp6v9dccglnokffuobpna1sk1eo7.apps.googleusercontent.com"
-- clientsecret = "zlpVYkY-_Dy5cruMS8YE7mbX"
clientid = "1086783968140-knfg08qu9onnn5b485veaskt2flr0loa.apps.googleusercontent.com"
clientsecret ="wRktcD6xif1_z6Xv-RhCatBB"


toInsert :: Row
toInsert= [p "name"        =:  "papaya",
     p "category"    =:  "fruit",
     p "healthiness" =:  "adequate",
     p "type"        =:  (3 :: Float)]

main = do
    mySheet <- access "1hIEq4AAauzI8INelQRIvgxBhmzX44qAB_1QQpFJQ2Xo" "od6" clientid clientsecret

    case mySheet of
      Just sheet -> do
         rows <- find (Query (Select Empty sheet) 0 0 (Order (p "category") False))
         print rows
        --  status <- insert toInsert sheet
        --  print status
        --  return ()
        --  case rows of
        --    Just rs -> do
        --      let upd = (p "id" =: (at (p "id") (last rs) :: T.Text)) : toInsert
        --      status <- update sheet upd
        --      print status
        --    Nothing -> putStrLn "Nothing_there"
      Nothing -> putStrLn "Nothing_here"
