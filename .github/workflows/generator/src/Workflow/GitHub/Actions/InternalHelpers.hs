module Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyMap, maybeNonEmptyList, applyModifiers, updateLens) where

import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M

maybeNonEmptyMap :: Map k v -> Maybe (Map k v)
maybeNonEmptyMap m
  | M.null m = Nothing
  | otherwise = Just m

maybeNonEmptyList :: [a] -> Maybe [a]
maybeNonEmptyList xs
  | L.null xs = Nothing
  | otherwise = Just xs

applyModifiers :: (Foldable f) => f (a -> a) -> a -> a
applyModifiers = foldr (.) id

-- | updating object with lens(in(extractor), out(combiner))
--
-- > updateLens extractor combiner f
updateLens :: (a -> b) -> (a -> b -> a) -> (b -> b) -> a -> a
updateLens in' out' f x = out' x $ f $ in' x
