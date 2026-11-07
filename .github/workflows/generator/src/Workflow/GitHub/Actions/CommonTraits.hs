module Workflow.GitHub.Actions.CommonTraits
  ( ConditionalElement (..),
    IdentifiedElement (..),
    NamedElement (..),
    HasEnvironmentVariables (..),
    DirectoryWorker (..),
  )
where

class ConditionalElement a where
  withCondition :: String -> a -> a

class IdentifiedElement a where
  identifiedAs :: String -> a -> a

class NamedElement a where
  namedAs :: String -> a -> a
  nameOf :: a -> Maybe String

class HasEnvironmentVariables a where
  env :: String -> String -> a -> a

class DirectoryWorker e where
  workAt :: String -> e -> e
