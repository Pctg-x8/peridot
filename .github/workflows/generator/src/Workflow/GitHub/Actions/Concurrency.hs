module Workflow.GitHub.Actions.Concurrency (Concurrency (..), ConcurrentlyElement (..)) where

import Data.Aeson (ToJSON (..), Value (String), object, (.=))
import Data.String (IsString (fromString))

data Concurrency = ConcurrentQueuedGroup String | ConcurrentCancelledGroup String

instance ToJSON Concurrency where
  toJSON (ConcurrentQueuedGroup g) = String $ fromString g
  toJSON (ConcurrentCancelledGroup g) = object ["group" .= g, "cancel-in-progress" .= True]

class ConcurrentlyElement e where
  concurrentPolicy :: Concurrency -> e -> e
