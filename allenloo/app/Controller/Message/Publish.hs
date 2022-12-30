{-# LANGUAGE OverloadedStrings #-}

module Controller.Message.Publish where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Database.Repository.MessageRepository
  ( GetMessagesOption (..),
    getMessages,
  )
import Model.AppDependencies
import Network.HTTP.Types (status200, status403)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
  ( Request (requestHeaders),
    Response,
    lazyRequestBody,
    responseLBS,
  )

publishMessageHandler :: Request -> ReaderT AppDependencies IO Response
publishMessageHandler req = do
  appConfig <- asks config
  let getMessagesOption =
        GetMessagesOption
          { published = False
          }
  unpublishedMessages <- liftIO $ getMessages appConfig getMessagesOption
  liftIO $ putStrLn "Got unpublished messages:"
  liftIO $ print unpublishedMessages
  return $
    responseLBS
      status200
      [(hContentType, "text/plain")]
      "OK"
