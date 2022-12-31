module Utils.VerifyApiKey where

import Data.ByteString.UTF8 (toString)
import Data.Char (toLower)
import Network.HTTP.Types (HeaderName, RequestHeaders, hAuthorization)

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
