module Main where

import Application.Xournal.Server.Yesod

import Yesod

main = do 
  putStrLn "xournal-web"
  warpDebug 4500 XournalWeb
