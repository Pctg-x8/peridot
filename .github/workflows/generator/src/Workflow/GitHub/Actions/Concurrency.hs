module Workflow.GitHub.Actions.Concurrency (Concurrency (..), ConcurrentlyElement (..)) where

import Data.Aeson (ToJSON (..), object, (.=))

data Concurrency = ConcurrentQueuedGroup String | ConcurrentCancelledGroup String

instance ToJSON Concurrency where
  toJSON (ConcurrentQueuedGroup g) = toJSON g
  toJSON (ConcurrentCancelledGroup g) = object ["group" .= g, "cancel-in-progress" .= True]

class ConcurrentlyElement e where
  concurrentPolicy :: Concurrency -> e -> e
