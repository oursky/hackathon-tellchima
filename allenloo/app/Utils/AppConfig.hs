module Utils.AppConfig where

import Configuration.Dotenv (parseFile)
import Data.List (unwords)
import qualified Data.Map as Map
import Model.AppConfig

loadRequiredConfigFromMap :: String -> Map.Map String String -> String
loadRequiredConfigFromMap key map = case Map.lookup key map of
  Nothing -> error $ key ++ " is missing from .env file"
  Just v -> v

getAppConfig :: IO AppConfig
getAppConfig = do
  let envFilePath = ".env"
  configMap <- Map.fromList <$> parseFile envFilePath
  let dbHost = loadRequiredConfigFromMap "DB_HOST" configMap
  let dbPort = loadRequiredConfigFromMap "DB_PORT" configMap
  let dbUser = loadRequiredConfigFromMap "DB_USER" configMap
  let dbPass = loadRequiredConfigFromMap "DB_PASS" configMap
  let dbName = loadRequiredConfigFromMap "DB_NAME" configMap
  let dbConnectionStr =
        unwords
          [ "host=" ++ dbHost,
            "port=" ++ dbPort,
            "dbname=" ++ dbName,
            "user=" ++ dbUser,
            "password=" ++ dbPass
          ]
  return $
    AppConfig
      { apiKey = loadRequiredConfigFromMap "API_KEY" configMap,
        dbConnectionStr = dbConnectionStr,
        slackCommandSigningSecret = loadRequiredConfigFromMap "SLACK_COMMAND_SIGNING_SECRET" configMap,
        slackCommandName = loadRequiredConfigFromMap "SLACK_COMMAND_NAME" configMap,
        slackPostMessageApiEndpoint = loadRequiredConfigFromMap "SLACK_POST_MESSAGE_API_ENDPOINT" configMap,
        slackChannelId = loadRequiredConfigFromMap "SLACK_CHANNEL_ID" configMap,
        slackApiToken = loadRequiredConfigFromMap "SLACK_API_TOKEN" configMap
      }
