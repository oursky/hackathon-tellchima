{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Repository.MessageRepository where

import Data.Int (Int64)
import Data.List (intercalate)
import Database.Entity.Message (MessageEntity)
import qualified Database.PostgreSQL.Simple as Psql
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
  messages <- Psql.query_ dbConn $ toPsqlQuery queryStr :: IO [MessageEntity]
  Psql.close dbConn
  return messages

data CreateMessagesArgs = CreateMessagesArgs
  { text :: String,
    published :: Bool
  }
  deriving (Generic, Psql.ToRow)

createMessage :: AppConfig -> CreateMessagesArgs -> IO Int64
createMessage appConfig message = do
  dbConn <- connectPsqlDb appConfig
  insertedCount <-
    Psql.withTransaction
      dbConn
      (Psql.execute dbConn "INSERT INTO messages (text, published) VALUES (?, ?)" message)
  Psql.close dbConn
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
    Psql.withTransaction
      dbConn
      (Psql.executeMany dbConn query (map Psql.Only ids))
  Psql.close dbConn
  return updatedCount
