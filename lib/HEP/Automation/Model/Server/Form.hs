{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module HEP.Automation.Model.Server.Form where

import Data.Text
-- import Yesod 
import Yesod.Request
import Yesod.Form
import Yesod.Form.Fields
import Yesod.Form.MassInput
import Yesod.Message
import Yesod.Widget
import Text.Blaze
import Control.Applicative 


data ModelForm  = ModelForm { modelName :: Text
                            , modelFeynRulesFile :: FileInfo } 
         deriving Show

modelAForm :: (RenderMessage s FormMessage, RenderMessage m FormMessage) => AForm s m ModelForm
modelAForm = ModelForm
  <$> areq textField "Model Name" Nothing
  <*> fileAFormReq "FeynRules file" 

modelForm :: (RenderMessage s FormMessage, RenderMessage m FormMessage) => 
             Html 
          -> Form s m (FormResult ModelForm, GWidget s m ()) 
modelForm = renderTable modelAForm 
