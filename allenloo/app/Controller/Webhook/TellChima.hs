{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller.Webhook.TellChima where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson
import Data.ByteString (toStrict)
import Data.Data (Typeable)
import Database.Entity.Message (MessageEntity (..))
import Database.PostgreSQL.Simple
import Database.Repository.MessageRepository
  ( CreateMessagesArgs (..),
    createMessage,
  )
import GHC.Generics
import Model.AppConfig
import Model.AppDependencies
import Model.ErrorResponse (badRequestResponse)
import Model.TellChimaException
import Model.TellChimaWebhookRequest as WebhookRequest
import Model.TellChimaWebhookResponse
import Network.HTTP.Types (status200, status400)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
  ( Request (requestHeaders),
    Response,
    lazyRequestBody,
    responseLBS,
  )
import Utils.VerifySlackWebhookSignature
import Web.FormUrlEncoded

tellChimaWebhookHandler :: Request -> ReaderT AppDependencies IO Response
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

processParsedWebhookRequest :: TellChimaWebhookRequest -> ReaderT AppDependencies IO Response
processParsedWebhookRequest requestBody = do
  appConfig <- asks config
  let message =
        CreateMessagesArgs
          { text = WebhookRequest.text requestBody,
            published = False
          }
  liftIO $ createMessage appConfig message

  return $
    responseLBS
      status200
      [(hContentType, "application/json")]
      (encode $ TellChimaWebhookResponse "Message received, it will be published at 5pm.")
