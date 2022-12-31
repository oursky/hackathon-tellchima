{-# LANGUAGE OverloadedStrings #-}

module Controller.Message.Publish where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import qualified Database.Entity.Message as MessageEntity
import Database.Repository.MessageRepository
  ( GetMessagesOption (..),
    getMessages,
  )
import qualified Database.Repository.MessageRepository as MessageRepo
import Model.AppConfig (AppConfig (..))
import Model.AppDependencies (AppDependencies (..))
import Model.ErrorResponse (forbiddenResponse)
import Network.HTTP.Types (status200, statusIsSuccessful)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
  ( Request (requestHeaders),
    Response,
    responseLBS,
  )
import qualified Service.MessageBuilder as MessageBuilder
import qualified Service.Slack as SlackService
import Utils.VerifyApiKey (verifyApiKey)

publishMessagesHandler :: Request -> ReaderT AppDependencies IO Response
publishMessagesHandler req = do
  appApiKey <- asks (apiKey . config)
  -- verify request API key
  let isVerified = verifyApiKey appApiKey (requestHeaders req)
  if not isVerified
    then return forbiddenResponse
    else publishMessages req

publishMessages :: Request -> ReaderT AppDependencies IO Response
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
