

module Main where


import qualified Data.Text  as T
import           GoogleSheet
-- import           Data.Scientific

-- clientid = "1086783968140-bupmbbqo8chsm2tej5ldgfci496a1lvr.apps.googleusercontent.com"
-- clientsecret = "cCle7aP41aEF9fNlVkUoUY69"
-- clientid = "1086783968140-onqtrp6v9dccglnokffuobpna1sk1eo7.apps.googleusercontent.com"
-- clientsecret = "zlpVYkY-_Dy5cruMS8YE7mbX"
clientid = "1086783968140-knfg08qu9onnn5b485veaskt2flr0loa.apps.googleusercontent.com"
clientsecret ="wRktcD6xif1_z6Xv-RhCatBB"


toInsert :: Row
toInsert =
    [p "name"        =:  ("papaya" :: String),
     p "category"    =:  ("fruit" :: String),
     p "healthiness" =:  ("adequate" :: String),
     p "type"        =:  (3 :: Float)]

main = do
    let key = "1hIEq4AAauzI8INelQRIvgxBhmzX44qAB_1QQpFJQ2Xo"
    let doc = "od6"

    let firstDocument = access key doc clientid clientsecret

    runAPI $ do
        sheet1 <- firstDocument
        --rows <- find (Query (Select Empty sheet1) 0 0 (Order (p "category") False))
        insert toInsert sheet1
        --io (mapM_ print rows)
