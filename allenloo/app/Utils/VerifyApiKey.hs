module Utils.VerifyApiKey where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.ByteString.UTF8 (toString)
import Data.Char (toLower)
import Model.AppConfig (AppConfig (..))
import Model.AppDependencies (AppDependencies (..))
import Model.ErrorResponse
import Network.HTTP.Types (HeaderName, RequestHeaders, hAuthorization)
import Network.Wai
  ( Request (requestHeaders),
    Response,
    responseLBS,
  )
import Types (AppEndpointHandler)

verifyApiKey :: String -> RequestHeaders -> Bool
verifyApiKey apiKey headers =
  case lookup hAuthorization headers of
    Nothing -> False
    Just authHeader -> do
      let authHeaderStr = toString authHeader
      -- "Bearer " is 7 character long
      let bearerTokenPrefixLength = 7
      let authHeaderWithoutBearer = drop bearerTokenPrefixLength authHeaderStr
      let tokenPrefix = take bearerTokenPrefixLength authHeaderStr
      authHeaderWithoutBearer == apiKey
        && map toLower tokenPrefix == "bearer "

applyVerifyApiKey :: AppEndpointHandler -> AppEndpointHandler
applyVerifyApiKey innerHandler req = do
  appApiKey <- asks (apiKey . config)
  -- verify request API key
  let isVerified = verifyApiKey appApiKey (requestHeaders req)
  if not isVerified
    then return forbiddenResponse
    else innerHandler req
