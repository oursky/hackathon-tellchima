module Model.AppConfig where

data AppConfig = AppConfig
  { apiKey :: String,
    slackCommandSigningSecret :: String,
    dbConnectionStr :: String
  }
  deriving (Show)
