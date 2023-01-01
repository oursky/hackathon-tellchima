{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller.Webhook.TellChima where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (encode)
import Data.ByteString (toStrict)
import Database.Entity.Message (MessageEntity (..))
import qualified Database.Repository.MessageRepository as MessageRepo
  ( CreateMessagesArgs (..),
    createMessage,
  )
import Model.AppConfig (AppConfig (..))
import Model.AppDependencies (AppDependencies (..))
import Model.ErrorResponse (badRequestResponse)
import Model.TellChimaWebhookRequest (TellChimaWebhookRequest)
import qualified Model.TellChimaWebhookRequest as WebhookRequest
import Model.TellChimaWebhookResponse (TellChimaWebhookResponse (..))
import Network.HTTP.Types (status200, status400)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
  ( Request (requestHeaders),
    Response,
    lazyRequestBody,
    responseLBS,
  )
import Types (AppContainer, AppEndpointHandler)
import Utils.VerifySlackWebhookSignature (verifySlackWebhookSignature)
import Web.FormUrlEncoded (urlDecodeAsForm)

tellChimaWebhookHandler :: AppEndpointHandler
tellChimaWebhookHandler req = do
  appConfig <- asks config
  let signingSecret = slackCommandSigningSecret appConfig
  let headers = requestHeaders req
  bodyStr <- liftIO $ lazyRequestBody req
  if verifySlackWebhookSignature signingSecret headers (toStrict bodyStr)
    then do
      case urlDecodeAsForm bodyStr of
        Left _ -> return badRequestResponse
        Right parsed -> processParsedWebhookRequest parsed
    else
      return $
        responseLBS
          status400
          [(hContentType, "application/json")]
          "Failed to verify webhook signature"

processParsedWebhookRequest :: TellChimaWebhookRequest -> AppContainer Response
processParsedWebhookRequest requestBody = do
  appConfig <- asks config
  let message =
        MessageRepo.CreateMessagesArgs
          { text = WebhookRequest.text requestBody,
            published = False
          }
  liftIO $ MessageRepo.createMessage appConfig message

  return $
    responseLBS
      status200
      [(hContentType, "application/json")]
      (encode $ TellChimaWebhookResponse "Message received, it will be published at 5pm.")
