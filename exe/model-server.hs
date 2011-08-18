module Main where

import HEP.Automation.Model.Server.Yesod

import Yesod

main = do 
  putStrLn "xournal-web"
  warpDebug 7800 ModelServer
