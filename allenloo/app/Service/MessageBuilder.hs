{-# LANGUAGE QuasiQuotes #-}

module Service.MessageBuilder where

import Control.Monad (unless, when)
import qualified Data.List as List
import Data.String.Interpolate (i)
import Database.Entity.Message (MessageEntity)
import qualified Database.Entity.Message as MessageEntity
import Model.TellChimaWebhookRequest (TellChimaWebhookRequest (command))

buildPublishMessageTextBodyRow :: MessageEntity -> String
buildPublishMessageTextBodyRow message =
  [i|`\##{MessageEntity.id message}` #{MessageEntity.text message}|]

buildPublishMessageTextBody :: String -> [MessageEntity] -> String
buildPublishMessageTextBody commandName messages =
  if not (null messages)
    then
      [i|Chima Summary (`/#{commandName}` to add)|]
        ++ "\n"
        ++ List.intercalate "\n" (map buildPublishMessageTextBodyRow messages)
    else "No message is received today"
