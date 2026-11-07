module Workflow.GitHub.Actions.Environment (Environment (..), EnvironmentalElement (..)) where

import Data.Aeson (ToJSON (..), Value (String), object, (.=))
import Data.String (IsString (fromString))

data Environment = RepositoryEnvironment String | ForeignEnvironment String String

instance ToJSON Environment where
  toJSON (RepositoryEnvironment s) = String $ fromString s
  toJSON (ForeignEnvironment s url) = object ["name" .= s, "url" .= url]

class EnvironmentalElement e where
  runInEnvironment :: Environment -> e -> e
