{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Repository.MessageRepository where

import Data.ByteString.UTF8 (fromString)
import Data.Int
import Database.Entity.Message (MessageEntity)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics
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

data CreateMessagesArgs = CreateMessagesArgs
  { text :: String,
    published :: Bool
  }
  deriving (Generic, ToRow)

createMessage :: AppConfig -> CreateMessagesArgs -> IO Int64
createMessage appConfig message = do
  dbConn <- connectPostgreSQL $ fromString $ dbConnectionStr appConfig
  insertedCount <-
    withTransaction
      dbConn
      (execute dbConn "INSERT INTO messages (text, published) VALUES (?, ?)" message)
  close dbConn
  return insertedCount

markMessagesPublished :: AppConfig -> [Int] -> IO Int64
markMessagesPublished appConfig ids = do
  dbConn <- connectPostgreSQL $ fromString $ dbConnectionStr appConfig
  let query =
        [sql|
          UPDATE messages
            SET published = TRUE
          FROM (VALUES (?)) as update(id)
          WHERE messages.id = update.id
        |]
  updatedCount <-
    withTransaction
      dbConn
      (executeMany dbConn query (map Only ids))
  close dbConn
  return updatedCount
