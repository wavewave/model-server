{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}



module HEP.Automation.Model.Server.Yesod where 

import Yesod hiding (update)

data ModelServer = ModelServer

mkYesod "ModelServer" [parseRoutes|
/ HomeR GET
/model/#String ModelR GET
|]

instance Yesod ModelServer where
  approot _ = ""

type Handler = GHandler ModelServer ModelServer

getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [hamlet|Hello World!|]

getModelR :: String -> Handler RepHtml
getModelR modelname = do 
  defaultLayout [hamlet| <h1> File #{modelname}
|]



