{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (runReaderT)
import Controller.Action.PostReminder (postReminderHandler)
import Controller.Message.Publish (publishMessagesHandler)
import Controller.Webhook.TellChima (tellChimaWebhookHandler)
import Model.AppDependencies (AppDependencies (..))
import Model.ErrorResponse (methodNotAllowedResponse, notFoundResponse, serverErrorResponse)
import Network.Wai (Application, Response, rawPathInfo, requestMethod)
import Network.Wai.Handler.Warp (run)
import Types (AppEndpointHandler)
import Utils.AppConfig (getAppConfig)

main :: IO ()
main = do
  putStrLn "[Main]: Loading env variable..."
  loadedAppConfig <- getAppConfig
  let appDep =
        AppDependencies
          { config = loadedAppConfig
          }
  let port = 3000
  putStrLn $ "[Main]: Listening on port " ++ show port
  run port (app appDep)

app :: AppDependencies -> Application
app dep req mapResponse = do
  result <- try (runReaderT (rootHandler req) dep) :: IO (Either SomeException Response)
  case result of
    Left ex -> do
      putStrLn $ "[Main]: Unhandled exception - " ++ show ex
      mapResponse serverErrorResponse
    Right resp -> mapResponse resp

rootHandler :: AppEndpointHandler
rootHandler req = do
  case rawPathInfo req of
    "/webhook/tell-chima" -> webhookTellChimaRoute req
    "/message/publish" -> messagePublishRoute req
    "/action/post-reminder" -> actionPostReminderRoute req
    _ -> return notFoundResponse

webhookTellChimaRoute :: AppEndpointHandler
webhookTellChimaRoute req =
  case requestMethod req of
    "POST" -> tellChimaWebhookHandler req
    _ -> return methodNotAllowedResponse

messagePublishRoute :: AppEndpointHandler
messagePublishRoute req =
  case requestMethod req of
    "POST" -> publishMessagesHandler req
    _ -> return methodNotAllowedResponse

actionPostReminderRoute :: AppEndpointHandler
actionPostReminderRoute req =
  case requestMethod req of
    "POST" -> postReminderHandler req
    _ -> return methodNotAllowedResponse
