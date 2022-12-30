module Model.TellChimaException where

import Control.Exception (Exception)
import Data.Data (Typeable)

data TellChimaException
  = BadRequest {message :: String}
  | ApiError {message :: String}
  deriving (Show, Typeable)

instance Exception TellChimaException
