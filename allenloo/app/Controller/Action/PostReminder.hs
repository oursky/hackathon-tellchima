{-# LANGUAGE OverloadedStrings #-}

module Controller.Action.PostReminder where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Model.AppConfig (AppConfig (..))
import Model.AppDependencies (AppDependencies (..))
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (responseLBS)
import qualified Service.MessageBuilder as MessageBuilder
import qualified Service.Slack as SlackService
import Types (AppEndpointHandler)
import Utils.VerifyApiKey (applyVerifyApiKey)

postReminderHandler :: AppEndpointHandler
postReminderHandler = applyVerifyApiKey postReminder

postReminder :: AppEndpointHandler
postReminder req = do
  appConfig <- asks config
  let messageBody = MessageBuilder.buildReminderTextBody (slackCommandName appConfig)
  liftIO $ SlackService.postSlackMessage appConfig messageBody

  return $
    responseLBS
      status200
      [(hContentType, "text/plain")]
      "OK"
