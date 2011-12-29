{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HEP.Automation.Model.Server.Yesod where 

import Yesod hiding (update)

import Network.Wai
import Network.Wai.Parse

import qualified Data.Enumerator as E
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

import qualified Data.ByteString.Lazy as LS

import HEP.Automation.Model.Type
import Data.Acid

import Data.Attoparsec as P
import Data.Aeson as A
import Data.Aeson.Parser

import Control.Applicative
import Data.Text hiding (concat)

import Data.UUID

import HEP.Automation.Model.Server.Type
import HEP.Automation.Model.Server.Form 


mkYesod "ModelServer" [parseRoutes|
/ HomeR GET
/listmodel  ListModelR GET
/uploadmodel  UploadModelR POST
/model/#UUID ModelR 
|]

instance Yesod ModelServer where
  approot _ = ""
  maximumContentLength _ _ = 100000000

instance RenderMessage ModelServer FormMessage where
  renderMessage _ _ = defaultFormMessage

{-
makeRepHtmlFromHamlet :: HtmlUrl (Route ModelServer) -> Handler RepHtml
makeRepHtmlFromHamlet hlet = do
  RepHtml rhtml <- hamletToRepHtml hlet 
  return (RepHtml rhtml) 

makeRepHtmlJsonFromHamletJson :: HtmlUrl (Route ModelServer) -> Value -> Handler RepHtmlJson
makeRepHtmlJsonFromHamletJson hlet j = do
  RepHtml rhtml <- hamletToRepHtml hlet 
  RepJson json <- jsonToRepJson j 
  return (RepHtmlJson rhtml json) 
-}

getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [whamlet|
!!!
<html>
  <head> 
    <title> test 
  <body> 
    <h1> hello world 
|]



defhlet = [whamlet| <h1> HTML output not supported |]


getListModelR :: Handler RepHtmlJson
getListModelR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))


postUploadModelR :: Handler RepHtmlJson
postUploadModelR = do 
  liftIO $ putStrLn "postQueueR called" 
  acid <- return.server_acid =<< getYesod
  _ <- getRequest
  bs' <- lift E.consume
  let bs = S.concat bs' 
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result ModelInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddModel minfo)
          liftIO $ print (Just r)
          liftIO $ print (A.toJSON (Just r))
          defaultLayoutJson defhlet (A.toJSON (Just r))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe ModelInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe ModelInfo))
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe ModelInfo))



handleModelR :: UUID -> Handler RepHtmlJson
handleModelR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getModelR name
    "PUT" -> putModelR name
    "DELETE" -> deleteModelR name

getModelR :: UUID -> Handler RepHtmlJson
getModelR idee = do 
  liftIO $ putStrLn "getModelR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryModel idee)
  liftIO $ putStrLn $ show r 
  let hlet = [whamlet| <h1> File #{idee}|]
  defaultLayoutJson hlet (A.toJSON (Just r))


putModelR :: UUID -> Handler RepHtmlJson
putModelR idee = do 
  liftIO $ putStrLn "putModelR called"
  acid <- return.server_acid =<< getYesod
  wr <- return.reqWaiRequest =<< getRequest
  bs' <- lift E.consume
  let bs = S.concat bs'
  let parsed = parse json bs 
  liftIO $ print parsed 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result ModelInfo) of 
        Success minfo -> do 
          if idee == model_uuid minfo
            then do r <- liftIO $ update acid (UpdateModel minfo)
                    defaultLayoutJson defhlet (A.toJSON (Just r))
            else do liftIO $ putStrLn "modelname mismatched"
                    defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe ModelInfo))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe ModelInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe ModelInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe ModelInfo))

deleteModelR :: UUID -> Handler RepHtmlJson
deleteModelR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteModel idee)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))

