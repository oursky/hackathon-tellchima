{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller.Webhook.TellChima where

import Control.Exception (Exception, SomeException, throw, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Data (Typeable)
import GHC.Generics
import Network.HTTP.Types (status200, status400, status404, status405)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, lazyRequestBody, responseBuilder, responseLBS)

data Test = Test
  { id :: Int,
    content :: String
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data TellChimaException = BadRequest {message :: String} deriving (Show, Typeable)

instance Exception TellChimaException

parseTellChimaWebhookRequest :: Request -> IO Test
parseTellChimaWebhookRequest req = do
  bodyStr <- lazyRequestBody req
  case decode bodyStr of
    Just body -> return body
    Nothing -> throw (BadRequest "Failed to parse JSON")

tellChimaWebhookHandler :: Request -> IO Response
tellChimaWebhookHandler req = do
  result <- try (parseTellChimaWebhookRequest req) :: IO (Either SomeException Test)
  case result of
    Left ex ->
      return $
        responseLBS
          status400
          [(hContentType, "application/json")]
          "400 - Bad Request"
    Right body ->
      return $
        responseLBS
          status200
          [(hContentType, "application/json")]
          (encode body)
