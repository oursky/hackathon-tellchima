{-# LANGUAGE OverloadedStrings #-}

module Controller.Webhook.TellChima where

import Control.Exception (SomeException, throw, try)
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Data (Typeable)
import GHC.Generics
import Model.ErrorResponse (badRequestResponse)
import Model.TellChimaException
import Model.TellChimaWebhookRequest
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, lazyRequestBody, responseLBS)
import Web.FormUrlEncoded

parseTellChimaWebhookRequest :: Request -> IO TellChimaWebhookRequest
parseTellChimaWebhookRequest req = do
  bodyStr <- lazyRequestBody req
  case urlDecodeAsForm bodyStr of
    Left _ -> throw (BadRequest "Failed to parse url encoded form")
    Right body -> return body

tellChimaWebhookHandler :: Request -> IO Response
tellChimaWebhookHandler req = do
  result <- try (parseTellChimaWebhookRequest req) :: IO (Either SomeException TellChimaWebhookRequest)
  case result of
    Left ex -> return badRequestResponse
    Right body ->
      return $
        responseLBS
          status200
          [(hContentType, "application/json")]
          (encode body)
