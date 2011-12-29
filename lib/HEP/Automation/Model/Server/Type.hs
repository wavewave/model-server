module HEP.Automation.Model.Server.Type where

import Data.Text.Encoding as E
import Data.UUID
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Yesod.Dispatch
import Text.Blaze

instance SinglePiece UUID where
  fromSinglePiece = fromByteString . L.fromChunks . return . E.encodeUtf8
  toSinglePiece = E.decodeUtf8 . B.concat . L.toChunks . toByteString 

instance ToHtml UUID where
  toHtml = toHtml . toString 
