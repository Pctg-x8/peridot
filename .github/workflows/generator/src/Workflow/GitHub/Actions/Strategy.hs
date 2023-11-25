module Workflow.GitHub.Actions.Strategy
  ( Strategy (..),
    emptyStrategy,
    strategyMatrixAddEntry,
    StrategyElement (..),
    maybeNonEmptyStrategy,
  )
where

import Data.Aeson (ToJSON (toJSON), Value, object, (.=))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyMap)

newtype Strategy = Strategy
  { strategyMatrix :: Map String Value
  }

emptyStrategy :: Strategy
emptyStrategy = Strategy {strategyMatrix = mempty}

maybeNonEmptyStrategy :: Strategy -> Maybe Strategy
maybeNonEmptyStrategy Strategy {..} | M.null strategyMatrix = Nothing
maybeNonEmptyStrategy s = Just s

instance ToJSON Strategy where
  toJSON Strategy {..} =
    object $
      catMaybes
        [ ("matrix" .=) <$> maybeNonEmptyMap strategyMatrix
        ]

strategyMatrixAddEntry :: (ToJSON v) => String -> v -> Strategy -> Strategy
strategyMatrixAddEntry key value self = self {strategyMatrix = M.insert key (toJSON value) $ strategyMatrix self}

class StrategyElement e where
  addStrategyMatrixEntry :: (ToJSON v) => String -> v -> e -> e
