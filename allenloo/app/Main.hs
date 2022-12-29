{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Types (status200, status404, status405)
import Network.HTTP.Types.Header (hAllow, hContentType)
import Network.Wai (Application, Request, Response, rawPathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)

newtype Test = Test {content :: String} deriving (Generic, ToJSON)

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app

app :: Application
app req res =
  res $ case rawPathInfo req of
    "/" -> rootRoute req
    _ -> notFoundRoute

rootRoute :: Request -> Response
rootRoute req =
  case requestMethod req of
    "POST" -> do
      responseLBS
        status200
        [(hContentType, "application/json")]
        . encode
        $ Test "World"
    _ -> do
      responseLBS
        status405
        [(hAllow, "POST"), (hContentType, "application/json")]
        "405 - Method Not Allowed"

notFoundRoute :: Response
notFoundRoute =
  responseLBS
    status404
    [(hContentType, "application/json")]
    "404 - Not Found"
