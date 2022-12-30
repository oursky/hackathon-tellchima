module Model.AppDependencies where

import Model.AppConfig

data AppDependencies = AppDependencies
  { config :: AppConfig
  }
  deriving (Show)
