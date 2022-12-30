module Utils.AppConfig where

import Configuration.Dotenv (parseFile)
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
  return $
    AppConfig
      { slackCommandSigningSecret = loadRequiredConfigFromMap "SLACK_COMMAND_SIGNING_SECRET" configMap,
        dbHost = loadRequiredConfigFromMap "DB_HOST" configMap,
        dbPort = loadRequiredConfigFromMap "DB_PORT" configMap,
        dbUser = loadRequiredConfigFromMap "DB_USER" configMap,
        dbPass = loadRequiredConfigFromMap "DB_PASS" configMap,
        dbName = loadRequiredConfigFromMap "DB_NAME" configMap
      }
