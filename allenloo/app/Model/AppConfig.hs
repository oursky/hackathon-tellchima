module Model.AppConfig where

data AppConfig = AppConfig
  { slackCommandSigningSecret :: String
  }
  deriving (Show)
