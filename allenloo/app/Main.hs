{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, try)
import Controller.Webhook.TellChima
import Data.Aeson
import GHC.Generics
import Network.HTTP.Types (status200, status404, status405, status500)
import Network.HTTP.Types.Header (hAllow, hContentType)
import Network.Wai (Application, Request, Response, rawPathInfo, requestMethod, responseLBS)
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
    Left ex ->
      mapResponse $
        responseLBS
          status500
          [(hContentType, "application/json")]
          "500 - Server Error"
    Right resp -> mapResponse resp

webhookTellChimaRoute :: Request -> IO Response
webhookTellChimaRoute req =
  case requestMethod req of
    "POST" -> tellChimaWebhookHandler req
    _ -> do
      return $
        responseLBS
          status405
          [(hAllow, "POST"), (hContentType, "application/json")]
          "405 - Method Not Allowed"

notFoundRoute :: IO Response
notFoundRoute =
  return $
    responseLBS
      status404
      [(hContentType, "application/json")]
      "404 - Not Found"
