{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, runReaderT)
import Controller.Webhook.TellChima
import Model.AppConfig
import Model.AppDependencies
import Model.ErrorResponse (methodNotAllowedResponse, notFoundResponse, serverErrorResponse)
import Network.Wai (Application, Request, Response, rawPathInfo, requestMethod)
import Network.Wai.Handler.Warp (run)
import Utils.AppConfig

main :: IO ()
main = do
  putStrLn "Loading env variable..."
  loadedAppConfig <- getAppConfig
  let appDep =
        AppDependencies
          { config = loadedAppConfig
          }
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port (app appDep)

app :: AppDependencies -> Application
app dep req mapResponse = do
  result <- try (runReaderT (rootHandler req) dep) :: IO (Either SomeException Response)
  case result of
    Left ex -> mapResponse serverErrorResponse
    Right resp -> mapResponse resp

rootHandler :: Request -> ReaderT AppDependencies IO Response
rootHandler req = do
  case rawPathInfo req of
    "/webhook/tell-chima" -> webhookTellChimaRoute req
    _ -> notFoundRoute

webhookTellChimaRoute :: Request -> ReaderT AppDependencies IO Response
webhookTellChimaRoute req =
  case requestMethod req of
    "POST" -> tellChimaWebhookHandler req
    _ -> return methodNotAllowedResponse

notFoundRoute :: ReaderT AppDependencies IO Response
notFoundRoute = return notFoundResponse
