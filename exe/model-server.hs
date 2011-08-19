module Main where

import HEP.Automation.Model.Server.Yesod

import Yesod

import HEP.Automation.Model.Type 

import qualified Data.Map as M
import Data.Acid 

newmodel = ModelInfo { 
  model_name = "newmodel", 
  model_baseurl = "http://susy.physics.lsa.umich.edu/newmodel", 
  model_feynrules = "newmodel.fr", 
  model_ufo = "newmodel_UFO"
}

testm = M.insert "newmodel" newmodel M.empty 

main = do 
  putStrLn "xournal-web"
  acid <- openAcidState testm
  
  warpDebug 7800 (ModelServer acid)
