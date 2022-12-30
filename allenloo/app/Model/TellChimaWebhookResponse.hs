{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.TellChimaWebhookResponse where

import Data.Aeson
import GHC.Generics

data TellChimaWebhookResponse = TellChimaWebhookResponse
  { text :: String
  }
  deriving (Generic, ToJSON, Show)
