module Utils.Database where

import Data.ByteString.UTF8 (fromString)
import Data.Char (toUpper)
import qualified Database.PostgreSQL.Simple as Psql
import qualified Database.PostgreSQL.Simple.Types as PsqlType
import Model.AppConfig (AppConfig (..))

connectPsqlDb :: AppConfig -> IO Psql.Connection
connectPsqlDb appConfig = Psql.connectPostgreSQL $ fromString $ dbConnectionStr appConfig

toPsqlQuery :: String -> PsqlType.Query
toPsqlQuery queryStr = PsqlType.Query $ fromString queryStr

class CanConvertToSqlValue v where
  toSqlValue :: v -> String

instance CanConvertToSqlValue Bool where
  toSqlValue b = map toUpper $ show b
