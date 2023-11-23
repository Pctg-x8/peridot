module Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyMap, maybeNonEmptyList, applyModifiers) where

import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M

maybeNonEmptyMap :: Map k v -> Maybe (Map k v)
maybeNonEmptyMap m = if M.null m then Nothing else Just m

maybeNonEmptyList :: [a] -> Maybe [a]
maybeNonEmptyList xs = if L.null xs then Nothing else Just xs

applyModifiers :: (Foldable f) => f (a -> a) -> a -> a
applyModifiers = foldr (.) id
