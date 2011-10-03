{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module HEP.Automation.Model.Server.Form where

import Data.Text
-- import Yesod 
import Yesod.Form
import Yesod.Message
import Yesod.Widget
import Text.Blaze
import Control.Applicative 


data ModelForm  = ModelForm { modelName :: Text
                            , modelFile :: Text } 
         deriving Show

modelAForm :: (RenderMessage s FormMessage, RenderMessage m FormMessage) => AForm s m ModelForm
modelAForm = ModelForm
  <$> areq textField "ModelName" Nothing
  <*> areq textField "ModelFile" Nothing 

modelForm :: (RenderMessage s FormMessage, RenderMessage m FormMessage) => 
             Html 
          -> Form s m (FormResult ModelForm, GWidget s m ()) 
modelForm = renderTable modelAForm 
