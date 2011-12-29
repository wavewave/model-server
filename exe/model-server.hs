{-# LANGUAGE OverloadedStrings #-}

module Main where

import HEP.Automation.Model.Server.Type
import HEP.Automation.Model.Server.Yesod ()
import Yesod
import qualified Data.Map as M
import Data.Acid 

main :: IO ()
main = do 
  putStrLn "model-server"
  acid <- openLocalState M.empty 
  warpDebug 7800 (ModelServer acid)
