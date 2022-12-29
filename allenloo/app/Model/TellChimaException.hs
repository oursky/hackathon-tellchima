module Model.TellChimaException where

import Control.Exception (Exception)
import Data.Data (Typeable)

data TellChimaException = BadRequest {message :: String} deriving (Show, Typeable)

instance Exception TellChimaException
