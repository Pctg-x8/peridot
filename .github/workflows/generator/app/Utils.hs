module Utils (applyModifiers, runOnFailure) where

import Workflow.GitHub.Actions qualified as GHA

applyModifiers :: (Foldable f) => f (a -> a) -> a -> a
applyModifiers = flip $ foldl (flip ($))

runOnFailure :: (GHA.Conditional c) => c -> c
runOnFailure = GHA.withCondition "failure()"
