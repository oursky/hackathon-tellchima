module Model.AppConfig where

data AppConfig = AppConfig
  { slackCommandSigningSecret :: String,
    dbHost :: String,
    dbPort :: String,
    dbUser :: String,
    dbPass :: String,
    dbName :: String
  }
  deriving (Show)
