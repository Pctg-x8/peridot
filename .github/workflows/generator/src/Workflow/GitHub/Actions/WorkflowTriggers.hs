{-# LANGUAGE OverloadedStrings #-}

module Workflow.GitHub.Actions.WorkflowTriggers
  ( WorkflowTrigger (..),
    onPullRequest,
    onPush,
    scheduled,
    onWorkflowDispatch,
    WorkflowPullRequestTrigger (..),
    workflowPullRequestTrigger,
    WorkflowPushTrigger (..),
    workflowPushTrigger,
    WorkflowScheduleTrigger (..),
    WorkflowScheduleTimer (..),
    PathFilteredTrigger (..),
    BranchFilteredTrigger (..),
    TypeFilteredTrigger (..),
  )
where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Aeson.Types (emptyObject)
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyList)

class PathFilteredTrigger t where
  filterPath :: String -> t -> t
  ignorePath :: String -> t -> t

class BranchFilteredTrigger t where
  filterBranch :: String -> t -> t
  ignoreBranch :: String -> t -> t

class TypeFilteredTrigger t where
  filterType :: String -> t -> t

data WorkflowTrigger = WorkflowTrigger
  { workflowTriggerOnPullRequest :: Maybe WorkflowPullRequestTrigger,
    workflowTriggerOnPullRequestTarget :: Maybe WorkflowPullRequestTrigger,
    workflowTriggerOnPush :: Maybe WorkflowPushTrigger,
    workflowTriggerOnSchedule :: Maybe WorkflowScheduleTrigger,
    workflowTriggerOnWorkflowDispatch :: Maybe WorkflowDispatchTrigger
  }

instance ToJSON WorkflowTrigger where
  toJSON WorkflowTrigger {..} =
    object $
      catMaybes
        [ ("pull_request" .=) <$> workflowTriggerOnPullRequest,
          ("pull_request_target" .=) <$> workflowTriggerOnPullRequestTarget,
          ("push" .=) <$> workflowTriggerOnPush,
          ("schedule" .=) <$> workflowTriggerOnSchedule,
          ("workflow_dispatch" .=) <$> workflowTriggerOnWorkflowDispatch
        ]

_emptyWorkflowTrigger :: WorkflowTrigger
_emptyWorkflowTrigger =
  WorkflowTrigger
    { workflowTriggerOnPullRequest = Nothing,
      workflowTriggerOnPullRequestTarget = Nothing,
      workflowTriggerOnPush = Nothing,
      workflowTriggerOnSchedule = Nothing,
      workflowTriggerOnWorkflowDispatch = Nothing
    }

onPullRequest :: WorkflowPullRequestTrigger -> WorkflowTrigger
onPullRequest details = _emptyWorkflowTrigger {workflowTriggerOnPullRequest = Just details}

onPush :: WorkflowPushTrigger -> WorkflowTrigger
onPush details = _emptyWorkflowTrigger {workflowTriggerOnPush = Just details}

scheduled :: [WorkflowScheduleTimer] -> WorkflowTrigger
scheduled timers = _emptyWorkflowTrigger {workflowTriggerOnSchedule = Just $ WorkflowScheduleTrigger timers}

onWorkflowDispatch :: WorkflowTrigger
onWorkflowDispatch = _emptyWorkflowTrigger {workflowTriggerOnWorkflowDispatch = Just WorkflowDispatchTrigger}

data WorkflowPullRequestTrigger = WorkflowPullRequestTrigger
  { prTriggerBranches :: [String],
    prTriggerBranchesIgnore :: [String],
    prTriggerPaths :: [String],
    prTriggerPathsIgnore :: [String],
    prTriggerTypes :: [String]
  }

instance ToJSON WorkflowPullRequestTrigger where
  toJSON WorkflowPullRequestTrigger {..} =
    object $
      catMaybes
        [ ("branches" .=) <$> maybeNonEmptyList prTriggerBranches,
          ("branches-ignore" .=) <$> maybeNonEmptyList prTriggerBranchesIgnore,
          ("paths" .=) <$> maybeNonEmptyList prTriggerPaths,
          ("paths-ignore" .=) <$> maybeNonEmptyList prTriggerPathsIgnore,
          ("types" .=) <$> maybeNonEmptyList prTriggerTypes
        ]

workflowPullRequestTrigger :: WorkflowPullRequestTrigger
workflowPullRequestTrigger =
  WorkflowPullRequestTrigger
    { prTriggerBranches = [],
      prTriggerBranchesIgnore = [],
      prTriggerPaths = [],
      prTriggerPathsIgnore = [],
      prTriggerTypes = []
    }

instance PathFilteredTrigger WorkflowPullRequestTrigger where
  filterPath path w = w {prTriggerPaths = path : prTriggerPaths w}
  ignorePath path w = w {prTriggerPathsIgnore = path : prTriggerPathsIgnore w}

instance BranchFilteredTrigger WorkflowPullRequestTrigger where
  filterBranch branch w = w {prTriggerBranches = branch : prTriggerBranches w}
  ignoreBranch branch w = w {prTriggerBranchesIgnore = branch : prTriggerBranchesIgnore w}

instance TypeFilteredTrigger WorkflowPullRequestTrigger where
  filterType t w = w {prTriggerTypes = t : prTriggerTypes w}

data WorkflowPushTrigger = WorkflowPushTrigger
  { pushTriggerBranches :: [String],
    pushTriggerBranchesIgnore :: [String],
    pushTriggerTags :: [String],
    pushTriggerTagsIgnore :: [String],
    pushTriggerPaths :: [String],
    pushTriggerPathsIgnore :: [String]
  }

instance ToJSON WorkflowPushTrigger where
  toJSON WorkflowPushTrigger {..} =
    object $
      catMaybes
        [ ("branches" .=) <$> maybeNonEmptyList pushTriggerBranches,
          ("branches-ignore" .=) <$> maybeNonEmptyList pushTriggerBranchesIgnore,
          ("tags" .=) <$> maybeNonEmptyList pushTriggerTags,
          ("tags-ignore" .=) <$> maybeNonEmptyList pushTriggerTagsIgnore,
          ("paths" .=) <$> maybeNonEmptyList pushTriggerPaths,
          ("pathsIgnore" .=) <$> maybeNonEmptyList pushTriggerPathsIgnore
        ]

workflowPushTrigger :: WorkflowPushTrigger
workflowPushTrigger =
  WorkflowPushTrigger
    { pushTriggerBranches = [],
      pushTriggerBranchesIgnore = [],
      pushTriggerTags = [],
      pushTriggerTagsIgnore = [],
      pushTriggerPaths = [],
      pushTriggerPathsIgnore = []
    }

instance PathFilteredTrigger WorkflowPushTrigger where
  filterPath path self = self {pushTriggerPaths = path : pushTriggerPaths self}
  ignorePath path self = self {pushTriggerPathsIgnore = path : pushTriggerPathsIgnore self}

instance BranchFilteredTrigger WorkflowPushTrigger where
  filterBranch branch self = self {pushTriggerBranches = branch : pushTriggerBranches self}
  ignoreBranch branch self = self {pushTriggerBranchesIgnore = branch : pushTriggerBranchesIgnore self}

newtype WorkflowScheduleTrigger = WorkflowScheduleTrigger [WorkflowScheduleTimer]

instance ToJSON WorkflowScheduleTrigger where
  toJSON (WorkflowScheduleTrigger timers) = toJSON timers

newtype WorkflowScheduleTimer = CronTimer String

instance ToJSON WorkflowScheduleTimer where
  toJSON (CronTimer timer) = object ["cron" .= timer]

data WorkflowDispatchTrigger = WorkflowDispatchTrigger

instance ToJSON WorkflowDispatchTrigger where
  toJSON WorkflowDispatchTrigger = emptyObject
