{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, try)
import Controller.Webhook.TellChima
import Model.ErrorResponse (methodNotAllowedResponse, notFoundResponse, serverErrorResponse)
import Network.Wai (Application, Request, Response, rawPathInfo, requestMethod)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app

app :: Application
app req mapResponse = do
  result <-
    try
      ( case rawPathInfo req of
          "/webhook/tell-chima" -> webhookTellChimaRoute req
          _ -> notFoundRoute
      ) ::
      IO (Either SomeException Response)
  case result of
    Left ex -> mapResponse serverErrorResponse
    Right resp -> mapResponse resp

webhookTellChimaRoute :: Request -> IO Response
webhookTellChimaRoute req =
  case requestMethod req of
    "POST" -> tellChimaWebhookHandler req
    _ -> do return methodNotAllowedResponse

notFoundRoute :: IO Response
notFoundRoute = do return notFoundResponse
