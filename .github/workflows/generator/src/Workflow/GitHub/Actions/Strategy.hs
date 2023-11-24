module Workflow.GitHub.Actions.Strategy (Strategy (..), strategy, strategyMatrixEntry) where

import Data.Aeson (ToJSON (toJSON), Value, object, (.=))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyMap)

newtype Strategy = Strategy
  { strategyMatrix :: Map String Value
  }

strategy :: Strategy
strategy = Strategy {strategyMatrix = mempty}

instance ToJSON Strategy where
  toJSON Strategy {..} =
    object $
      catMaybes
        [ ("matrix" .=) <$> maybeNonEmptyMap strategyMatrix
        ]

strategyMatrixEntry :: String -> Value -> Strategy -> Strategy
strategyMatrixEntry key value self = self {strategyMatrix = M.insert key value $ strategyMatrix self}
