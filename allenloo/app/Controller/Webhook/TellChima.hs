{-# LANGUAGE OverloadedStrings #-}

module Controller.Webhook.TellChima where

import Control.Exception (SomeException, throw, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson
import Data.ByteString (toStrict)
import Data.Data (Typeable)
import GHC.Generics
import Model.AppConfig
import Model.AppDependencies
import Model.ErrorResponse (badRequestResponse)
import Model.TellChimaException
import Model.TellChimaWebhookRequest
import Network.HTTP.Types (status200, status400)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request (requestBody, requestHeaders), Response, lazyRequestBody, responseLBS)
import Utils.VerifySlackWebhookSignature
import Web.FormUrlEncoded

tellChimaWebhookHandler :: Request -> ReaderT AppDependencies IO Response
tellChimaWebhookHandler req = do
  dep <- ask
  let signingSecret = slackCommandSigningSecret $ config dep
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
  -- DEBUG
  liftIO $ print requestBody
  return $
    responseLBS
      status200
      [(hContentType, "application/json")]
      (encode requestBody)
