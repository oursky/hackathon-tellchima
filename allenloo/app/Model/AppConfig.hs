module Model.AppConfig where

data AppConfig = AppConfig
  { slackCommandSigningSecret :: String,
    dbConnectionStr :: String
  }
  deriving (Show)
