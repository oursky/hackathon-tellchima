{-# LANGUAGE OverloadedStrings #-}

module Utils.VerifySlackWebhookSignature where

import Crypto.Hash.SHA256
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import qualified Data.ByteString.UTF8 as BS (fromString, toString)
import Data.List (intercalate)
import Data.Maybe
import Network.HTTP.Types (HeaderName, RequestHeaders)

-- Message is constructed by taking header `X-Slack-Request-Timestamp`
-- and request body combine with colon `:` in format {version}:{timestamp}:{body}
-- where version is always 'v0'
-- Signature can be retrived from header `X-Slack-Signature`, prepended by `{version}=`
computeSlackWebhookSignature :: String -> String -> String -> String
computeSlackWebhookSignature secret timestamp body = do
  let message = intercalate ":" ["v0", timestamp, body]
  let digestBytes = hmac (BS.fromString secret) (BS.fromString message)
  let hex = BS.toString $ encode digestBytes
  "v0=" ++ hex

verifySlackWebhookSignature :: String -> RequestHeaders -> ByteString -> Bool
verifySlackWebhookSignature secret headers body = do
  let requestSignature = lookup "X-Slack-Signature" headers
  let timestamp = lookup "X-Slack-Request-Timestamp" headers
  isJust requestSignature
    && isJust timestamp
    && do
      let timestampStr = BS.toString $ fromJust timestamp
      let requestSignatureStr = BS.toString $ fromJust requestSignature
      let computedSignature = computeSlackWebhookSignature secret timestampStr (BS.toString body)
      computedSignature == requestSignatureStr
