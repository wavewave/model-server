{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.Model.Server.Type where

import Control.Applicative
import Data.Text.Encoding as E
import Data.UUID
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Yesod.Dispatch
import Text.Blaze
import HEP.Automation.Model.Type
-- import Debug.Trace 
import Data.Acid

instance SinglePiece UUID where
  fromSinglePiece = fromString . C.unpack . E.encodeUtf8
  toSinglePiece = E.decodeUtf8 . C.pack . toString 

instance ToHtml UUID where
  toHtml = toHtml . toString 

data ModelServer = ModelServer {
  server_acid :: AcidState ModelInfoRepository
}

