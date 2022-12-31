{-# LANGUAGE OverloadedStrings #-}

module Model.ErrorResponse where

import Network.HTTP.Types (status400, status404, status405, status500)
import Network.HTTP.Types.Header (hAllow, hContentType)
import Network.Wai (Request, Response, lazyRequestBody, responseBuilder, responseLBS)

badRequestResponse =
  responseLBS
    status400
    [(hContentType, "text/plain")]
    "400 - Bad Request"

notFoundResponse =
  responseLBS
    status404
    [(hContentType, "text/plain")]
    "404 - Not Found"

methodNotAllowedResponse =
  responseLBS
    status405
    [(hAllow, "POST"), (hContentType, "text/plain")]
    "405 - Method Not Allowed"

serverErrorResponse =
  responseLBS
    status500
    [(hContentType, "text/plain")]
    "500 - Server Error"
