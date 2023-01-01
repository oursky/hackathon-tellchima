module Types where

import Control.Monad.Reader (ReaderT)
import Model.AppDependencies (AppDependencies (..))
import Network.Wai (Request, Response)

type AppContainer = ReaderT AppDependencies IO

type AppEndpointHandler = Request -> AppContainer Response
