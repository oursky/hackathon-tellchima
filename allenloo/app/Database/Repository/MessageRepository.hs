{-# LANGUAGE OverloadedStrings #-}

module Database.Repository.MessageRepository where

import Data.ByteString.UTF8 (fromString)
import Database.Entity.Message (MessageEntity)
import Database.PostgreSQL.Simple
import Model.AppConfig (AppConfig, dbConnectionStr)

data GetMessagesOption = GetMessagesOption
  { published :: Bool
  }

getMessages :: AppConfig -> GetMessagesOption -> IO [MessageEntity]
getMessages appConfig option = do
  dbConn <- connectPostgreSQL $ fromString $ dbConnectionStr appConfig
  messages <- query_ dbConn "SELECT * FROM messages" :: IO [MessageEntity]
  close dbConn
  return messages
