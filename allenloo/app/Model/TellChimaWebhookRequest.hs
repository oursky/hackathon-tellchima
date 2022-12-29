{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.TellChimaWebhookRequest where

import Data.Aeson
import GHC.Generics
import Web.FormUrlEncoded

data TellChimaWebhookRequest = TellChimaWebhookRequest
  { token :: String,
    team_id :: String,
    channel_name :: String,
    command :: String,
    text :: String,
    api_app_id :: String,
    response_url :: String,
    trigger_id :: String
    -- Unused fields which present in request
    -- user_id :: String,
    -- user_name :: String,
    -- team_domain :: String,
    -- is_enterprise_install :: Bool,
  }
  deriving (Generic, ToJSON, FromForm, Show)
