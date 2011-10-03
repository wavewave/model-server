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

import Control.Applicative
import Data.Text hiding (concat)

import HEP.Automation.Model.Server.Form 

data ModelServer = ModelServer {
  server_acid :: AcidState ModelInfoRepository
}

mkYesod "ModelServer" [parseRoutes|
/ HomeR GET
/listmodel  ListModelR GET
/uploadmodel  UploadModelR POST
/model/#String ModelR 
/uploadmodelform UploadModelFormR GET POST
|]

instance Yesod ModelServer where
  approot _ = ""
  maximumContentLength _ _ = 100000000

instance RenderMessage ModelServer FormMessage where
  renderMessage _ _ = defaultFormMessage

makeRepHtmlFromHamlet :: HtmlUrl (Route ModelServer) -> Handler RepHtml
makeRepHtmlFromHamlet hlet = do
  RepHtml rhtml <- hamletToRepHtml hlet 
  return (RepHtml rhtml) 

makeRepHtmlJsonFromHamletJson :: HtmlUrl (Route ModelServer) -> Value -> Handler RepHtmlJson
makeRepHtmlJsonFromHamletJson hlet j = do
  RepHtml rhtml <- hamletToRepHtml hlet 
  RepJson json <- jsonToRepJson j 
  return (RepHtmlJson rhtml json) 


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
defhlet = [hamlet| <h1> HTML output not supported |]


getListModelR :: Handler RepHtmlJson
getListModelR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Just r))


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
      case (G.fromJSON parsedjson :: A.Result ModelInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddModel minfo)
          makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Just r))
        Error err -> do 
          liftIO $ putStrLn err 
          makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Nothing :: Maybe ModelInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Nothing :: Maybe ModelInfo))
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Nothing :: Maybe ModelInfo))



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
  let hlet = [hamlet| <h1> File #{modelname}|]
  makeRepHtmlJsonFromHamletJson hlet (G.toJSON (Just r))


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
                    makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Just r))
            else do liftIO $ putStrLn "modelname mismatched"
                    makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Nothing :: Maybe ModelInfo))
        Error err -> do 
          liftIO $ putStrLn err 
          makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Nothing :: Maybe ModelInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Nothing :: Maybe ModelInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Nothing :: Maybe ModelInfo))

deleteModelR :: String -> Handler RepHtmlJson
deleteModelR modelname = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteModel modelname)
  liftIO $ putStrLn $ show r 
  makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Just r))

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
          makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Just r))
        Error err -> do 
          liftIO $ putStrLn err 
          makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Nothing :: Maybe ModelInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Nothing :: Maybe ModelInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      makeRepHtmlJsonFromHamletJson defhlet (G.toJSON (Nothing :: Maybe ModelInfo))


getUploadModelFormR :: Handler RepHtml
getUploadModelFormR = do 
  ((_,widget),enctype) <- generateFormPost modelForm 
  defaultLayout [whamlet| 
<form method=post action=@{UploadModelFormR} enctype=#{enctype}>
  ^{widget}
  <input type=submit >
test 
|] 



postUploadModelFormR :: Handler RepHtml
postUploadModelFormR = do 
  liftIO $ putStrLn "postUploadModelFormR called"
  ((result,widget),enctype) <- runFormPost modelForm 
 
  case result of 
    FormSuccess r -> defaultLayout [whamlet| 
<p> postUploadModelR called #{show (modelName r)}
|]
    _ -> defaultLayout [whamlet|
<p> postUploadModelR called, not Success
|] 
  