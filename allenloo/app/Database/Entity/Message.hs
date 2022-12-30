{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Entity.Message where

import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics

data MessageEntity = MessageEntity
  { id :: Int,
    text :: String,
    published :: Bool
  }
  deriving (Generic, FromRow, Show)
