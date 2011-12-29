{-# LANGUAGE OverloadedStrings #-}

module Main where

import HEP.Automation.Model.Server.Type
import HEP.Automation.Model.Server.Yesod
import Yesod
import HEP.Automation.Model.Type 
import Data.UUID.V5 
import qualified Data.Map as M
import Data.Acid 
import qualified Data.ByteString as B

newmodel = ModelInfo { 
  model_uuid = generateNamed namespaceURL (B.unpack "test") ,
  model_name = "newmodel"
}

testm = M.insert (model_uuid newmodel) newmodel M.empty 

main = do 
  putStrLn "model-server"
  acid <- openLocalState testm
  
  warpDebug 7800 (ModelServer acid)
