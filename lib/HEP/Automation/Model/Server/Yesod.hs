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

import HEP.Automation.Model.Type
import Data.Acid

import Data.Attoparsec as P
import Data.Aeson as A
import Data.Aeson.Parser
import qualified Data.Aeson.Generic as G

data ModelServer = ModelServer {
  server_acid :: AcidState ModelInfoRepository
}

mkYesod "ModelServer" [parseRoutes|
/ HomeR GET
/listmodel  ListModelR GET
/uploadmodel  UploadModelR POST
/model/#String ModelR 
|]

instance Yesod ModelServer where
  approot _ = ""

-- type Handler = GHandler ModelServer ModelServer

makeRepHtmlFromHamlet hlet = do
  RepHtml rhtml <- hamletToRepHtml hlet 
--  RepJson json <- jsonToRepJson 
  return (RepHtml rhtml) 


getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  makeRepHtmlFromHamlet [hamlet|
!!!
<html>
  <head> 
    <title> test 
  <body> 
    <h1> hello world 
|]

--  defaultLayout rhtml 


-- Hello World!|]


getListModelR :: Handler RepHtml
getListModelR = undefined

postUploadModelR :: Handler RepHtmlJson
postUploadModelR = undefined

handleModelR :: String -> Handler RepHtmlJson
handleModelR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getModelR name
    "PUT" -> putModelR name
    "DELETE" -> deleteModelR name

getModelR :: String -> Handler RepHtmlJson
getModelR modelname = do 
  liftIO $ putStrLn "getModelR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryModel modelname)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson [hamlet| <h1> File #{modelname}|] (G.toJSON r)

putModelR :: String -> Handler RepHtmlJson
putModelR modelname = do 
  liftIO $ putStrLn "putModelR called"
  acid <- return.server_acid =<< getYesod
  wr <- return.reqWaiRequest =<< getRequest
  bs' <- lift E.consume
  let bs = S.concat bs'
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (G.fromJSON parsedjson :: A.Result ModelInfo) of 
        Success minfo -> do 
          if modelname == model_name minfo
            then do r <- liftIO $ update acid (UpdateModel minfo)
                    defaultLayoutJson [hamlet| <h1> File #{modelname}|] 
                                      (G.toJSON r)
            else do liftIO $ putStrLn "modelname mismatched"
                    defaultLayoutJson [hamlet| <h1> File #{modelname}|] 
                                      (G.toJSON (Nothing :: Maybe ModelInfo))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson [hamlet| <h1> File #{modelname}|] 
                            (G.toJSON (Nothing :: Maybe ModelInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson [hamlet| <h1> File #{modelname}|] 
                        (G.toJSON (Nothing :: Maybe ModelInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson [hamlet| <h1> File #{modelname}|] 
                        (G.toJSON (Nothing :: Maybe ModelInfo))

deleteModelR :: String -> Handler RepHtmlJson
deleteModelR modelname = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteModel modelname)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson [hamlet| <h1> File #{modelname}|] (G.toJSON r)

postModelUploadR :: Handler RepHtmlJson
postModelUploadR = do 
  liftIO $ putStrLn "postModelUpdateR called"
  acid <- return.server_acid =<< getYesod
  wr <- return.reqWaiRequest =<< getRequest
  bs' <- lift E.consume
  let bs = S.concat bs'
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (G.fromJSON parsedjson :: A.Result ModelInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddModel minfo)
          defaultLayoutJson [hamlet| <h1> Posted|] 
                            (G.toJSON r)
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson [hamlet| <h1> Not posted|] 
                            (G.toJSON (Nothing :: Maybe ModelInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson [hamlet| <h1> Not posted|] 
                        (G.toJSON (Nothing :: Maybe ModelInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson [hamlet| <h1> Not posted|] 
                        (G.toJSON (Nothing :: Maybe ModelInfo))



