{-# LANGUAGE OverloadedStrings #-}

module Controller.Message.Publish where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Database.Entity.Message as MessageEntity
import Database.Repository.MessageRepository
  ( GetMessagesOption (..),
    getMessages,
  )
import qualified Database.Repository.MessageRepository as MessageRepo
import Model.AppConfig (AppConfig (..))
import Model.AppDependencies (AppDependencies (..))
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (responseLBS)
import qualified Service.MessageBuilder as MessageBuilder
import qualified Service.Slack as SlackService
import Types (AppEndpointHandler)
import Utils.VerifyApiKey (applyVerifyApiKey)

publishMessagesHandler :: AppEndpointHandler
publishMessagesHandler = applyVerifyApiKey publishMessages

publishMessages :: AppEndpointHandler
publishMessages req = do
  appConfig <- asks config

  -- get unpublished messages
  let getMessagesOption =
        GetMessagesOption
          { published = Just False
          }
  unpublishedMessages <- liftIO $ getMessages appConfig getMessagesOption

  -- publish on Slack by posting summary
  let messageText = MessageBuilder.buildPublishMessageTextBody (slackCommandName appConfig) unpublishedMessages
  liftIO $ SlackService.postSlackMessage appConfig messageText

  -- update published flag in database
  liftIO $ MessageRepo.markMessagesPublished appConfig (map MessageEntity.id unpublishedMessages)

  return $
    responseLBS
      status200
      [(hContentType, "text/plain")]
      "OK"
