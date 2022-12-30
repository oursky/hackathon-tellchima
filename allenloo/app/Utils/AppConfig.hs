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
      { slackCommandSigningSecret = loadRequiredConfigFromMap "SLACK_COMMAND_SIGNING_SECRET" configMap
      }
