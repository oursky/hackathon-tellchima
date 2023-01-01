{-# LANGUAGE OverloadedStrings #-}

module Service.Slack where

import Data.ByteString.UTF8 (fromString)
import Data.Aeson (encode)
import Model.PostSlackMessageRequest (PostSlackMessageRequest (..))
import Model.AppConfig (AppConfig (..))
import Model.TellChimaException (TellChimaException (..))
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Simple as HttpSimple
import qualified Network.HTTP.Conduit as HttpConduit
import Network.HTTP.Types (statusIsSuccessful)
import Network.HTTP.Types.Header (hContentType)
import Control.Exception (throw)
import Control.Monad (unless)

postSlackMessage :: AppConfig -> String -> IO ()
postSlackMessage appConfig messageText = do
  manager <- HttpClient.newManager HttpConduit.tlsManagerSettings
  let requestBody = PostSlackMessageRequest (slackChannelId appConfig) messageText
  initialRequest <- HttpClient.parseRequest $ slackPostMessageApiEndpoint appConfig
  {- ORMOLU_DISABLE -}
  let request = HttpSimple.setRequestBearerAuth (fromString $ slackApiToken appConfig)
        $ HttpSimple.setRequestHeader hContentType ["application/json"]
        $ HttpSimple.setRequestMethod "POST"
        $ HttpSimple.setRequestBodyLBS (encode requestBody)
        $ initialRequest
  {- ORMOLU_ENABLE -}
  response <- HttpClient.httpLbs request manager
  let isSuccess = statusIsSuccessful $ HttpSimple.getResponseStatus response
  unless isSuccess
    $ throw (ApiError "Failed to post message via Slack API")
