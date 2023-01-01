{-# LANGUAGE OverloadedStrings #-}

module Controller.Action.PostReminder where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Model.AppConfig (AppConfig (..))
import Model.AppDependencies (AppDependencies (..))
import Model.ErrorResponse (forbiddenResponse)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
  ( Request (requestHeaders),
    Response,
    responseLBS,
  )
import qualified Service.MessageBuilder as MessageBuilder
import qualified Service.Slack as SlackService
import Utils.VerifyApiKey (verifyApiKey)

postReminderHandler :: Request -> ReaderT AppDependencies IO Response
postReminderHandler req = do
  appApiKey <- asks (apiKey . config)
  -- verify request API key
  let isVerified = verifyApiKey appApiKey (requestHeaders req)
  if not isVerified
    then return forbiddenResponse
    else postReminder req

postReminder :: Request -> ReaderT AppDependencies IO Response
postReminder req = do
  appConfig <- asks config
  let messageBody = MessageBuilder.buildReminderTextBody (slackCommandName appConfig)
  liftIO $ SlackService.postSlackMessage appConfig messageBody

  return $
    responseLBS
      status200
      [(hContentType, "text/plain")]
      "OK"
