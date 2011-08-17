{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Application.Xournal.Server.Yesod where 

import Yesod hiding (update)

data XournalWeb = XournalWeb

mkYesod "XournalWeb" [parseRoutes|
/ HomeR GET
/xoj/#Int/page/#Int XojPageR GET
|]

instance Yesod XournalWeb where
  approot _ = ""

type Handler = GHandler XournalWeb XournalWeb

getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [hamlet|Hello World!|]

getXojPageR :: Int -> Int -> Handler RepHtml
getXojPageR filenum pagenum = do 
  defaultLayout [hamlet| <h1> File #{filenum}
                         <p
                            Page #{pagenum} |]

