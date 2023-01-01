module Model.AppConfig where

data AppConfig = AppConfig
  { apiKey :: String,
    dbConnectionStr :: String,
    slackCommandSigningSecret :: String,
    slackCommandName :: String,
    slackPostMessageApiEndpoint :: String,
    slackChannelId :: String,
    slackApiToken :: String
  }
  deriving (Show)
