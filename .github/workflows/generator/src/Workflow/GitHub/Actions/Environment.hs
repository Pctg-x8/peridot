module Workflow.GitHub.Actions.Environment (Environment (..), EnvironmentalElement (..)) where

import Data.Aeson (ToJSON (..), object, (.=))

data Environment = RepositoryEnvironment String | ForeignEnvironment String String

instance ToJSON Environment where
  toJSON (RepositoryEnvironment s) = toJSON s
  toJSON (ForeignEnvironment s url) = object ["name" .= s, "url" .= url]

class EnvironmentalElement e where
  runInEnvironment :: Environment -> e -> e
