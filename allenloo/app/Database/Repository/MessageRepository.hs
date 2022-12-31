{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Repository.MessageRepository where

import Data.Int (Int64)
import Data.List (intercalate)
import Database.Entity.Message (MessageEntity)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics
import Model.AppConfig (AppConfig)
import Utils.Database (connectPsqlDb, toPsqlQuery, toSqlValue)

data GetMessagesOption = GetMessagesOption
  { published :: Maybe Bool
  }

getMessages :: AppConfig -> GetMessagesOption -> IO [MessageEntity]
getMessages appConfig option = do
  dbConn <- connectPsqlDb appConfig
  let queryStringExprList =
        [ "SELECT * FROM messages",
          case published (option :: GetMessagesOption) of
            Just p -> "WHERE published = " ++ toSqlValue p
            Nothing -> "",
          "ORDER BY id ASC"
        ]
  let queryStr = intercalate "\n" $ filter (not . null) queryStringExprList
  messages <- query_ dbConn $ toPsqlQuery queryStr :: IO [MessageEntity]
  close dbConn
  return messages

data CreateMessagesArgs = CreateMessagesArgs
  { text :: String,
    published :: Bool
  }
  deriving (Generic, ToRow)

createMessage :: AppConfig -> CreateMessagesArgs -> IO Int64
createMessage appConfig message = do
  dbConn <- connectPsqlDb appConfig
  insertedCount <-
    withTransaction
      dbConn
      (execute dbConn "INSERT INTO messages (text, published) VALUES (?, ?)" message)
  close dbConn
  return insertedCount

markMessagesPublished :: AppConfig -> [Int] -> IO Int64
markMessagesPublished appConfig ids = do
  dbConn <- connectPsqlDb appConfig
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
