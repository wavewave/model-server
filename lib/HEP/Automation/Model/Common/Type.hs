module HEP.Automation.Model.Common.Type where

import System.FilePath

data FeynRulesModel = FeynRulesModel { 
  fr_modelName :: String, 
  fr_modelDir :: FilePath
} deriving Show



