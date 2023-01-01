{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.PostSlackMessageRequest where

import Data.Aeson
import GHC.Generics
import Web.FormUrlEncoded

data PostSlackMessageRequest = PostSlackMessageRequest
  { channel :: String,
    text :: String
  }
  deriving (Generic, ToJSON, Show)
