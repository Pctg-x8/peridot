{-# LANGUAGE OverloadedStrings #-}

module Workflow.GitHub.Actions.WorkflowTriggers
  ( WorkflowTrigger (..),
    onPullRequest,
    onPush,
    scheduled,
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
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyList)

data WorkflowTrigger = WorkflowTrigger
  { workflowTriggerOnPullRequest :: Maybe WorkflowPullRequestTrigger,
    workflowTriggerOnPullRequestTarget :: Maybe WorkflowPullRequestTrigger,
    workflowTriggerOnPush :: Maybe WorkflowPushTrigger,
    workflowTriggerOnSchedule :: Maybe WorkflowScheduleTrigger
  }

instance ToJSON WorkflowTrigger where
  toJSON t =
    object $
      catMaybes
        [ ("pull_request" .=) <$> workflowTriggerOnPullRequest t,
          ("pull_request_target" .=) <$> workflowTriggerOnPullRequestTarget t,
          ("push" .=) <$> workflowTriggerOnPush t,
          ("schedule" .=) <$> workflowTriggerOnSchedule t
        ]

_emptyWorkflowTrigger :: WorkflowTrigger
_emptyWorkflowTrigger =
  WorkflowTrigger
    { workflowTriggerOnPullRequest = Nothing,
      workflowTriggerOnPullRequestTarget = Nothing,
      workflowTriggerOnPush = Nothing,
      workflowTriggerOnSchedule = Nothing
    }

onPullRequest :: WorkflowPullRequestTrigger -> WorkflowTrigger
onPullRequest details = _emptyWorkflowTrigger {workflowTriggerOnPullRequest = Just details}

onPush :: WorkflowPushTrigger -> WorkflowTrigger
onPush details = _emptyWorkflowTrigger {workflowTriggerOnPush = Just details}

scheduled :: [WorkflowScheduleTimer] -> WorkflowTrigger
scheduled timers = _emptyWorkflowTrigger {workflowTriggerOnSchedule = Just $ WorkflowScheduleTrigger timers}

data WorkflowPullRequestTrigger = WorkflowPullRequestTrigger
  { prTriggerBranches :: [String],
    prTriggerBranchesIgnore :: [String],
    prTriggerPaths :: [String],
    prTriggerPathsIgnore :: [String],
    prTriggerTypes :: [String]
  }

instance ToJSON WorkflowPullRequestTrigger where
  toJSON t =
    object $
      catMaybes
        [ ("branches" .=) <$> maybeNonEmptyList (prTriggerBranches t),
          ("branches-ignore" .=) <$> maybeNonEmptyList (prTriggerBranchesIgnore t),
          ("paths" .=) <$> maybeNonEmptyList (prTriggerPaths t),
          ("paths-ignore" .=) <$> maybeNonEmptyList (prTriggerPathsIgnore t),
          ("types" .=) <$> maybeNonEmptyList (prTriggerTypes t)
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

data WorkflowPushTrigger = WorkflowPushTrigger [String] [String] [String] [String] [String] [String]

instance ToJSON WorkflowPushTrigger where
  toJSON (WorkflowPushTrigger branches branchesIgnore tags tagsIgnore paths pathsIgnore) =
    object $
      catMaybes
        [ ("branches" .=) <$> maybeNonEmptyList branches,
          ("branches-ignore" .=) <$> maybeNonEmptyList branchesIgnore,
          ("tags" .=) <$> maybeNonEmptyList tags,
          ("tags-ignore" .=) <$> maybeNonEmptyList tagsIgnore,
          ("paths" .=) <$> maybeNonEmptyList paths,
          ("pathsIgnore" .=) <$> maybeNonEmptyList pathsIgnore
        ]

workflowPushTrigger :: WorkflowPushTrigger
workflowPushTrigger = WorkflowPushTrigger [] [] [] [] [] []

newtype WorkflowScheduleTrigger = WorkflowScheduleTrigger [WorkflowScheduleTimer]

instance ToJSON WorkflowScheduleTrigger where
  toJSON (WorkflowScheduleTrigger timers) = toJSON timers

newtype WorkflowScheduleTimer = CronTimer String

instance ToJSON WorkflowScheduleTimer where
  toJSON (CronTimer timer) = object ["cron" .= timer]

class PathFilteredTrigger t where
  filterPath :: String -> t -> t
  ignorePath :: String -> t -> t

class BranchFilteredTrigger t where
  filterBranch :: String -> t -> t
  ignoreBranch :: String -> t -> t

class TypeFilteredTrigger t where
  filterType :: String -> t -> t

instance PathFilteredTrigger WorkflowPushTrigger where
  filterPath path (WorkflowPushTrigger b bi t ti paths pi') = WorkflowPushTrigger b bi t ti (paths ++ [path]) pi'
  ignorePath path (WorkflowPushTrigger b bi t ti p pathsIgnore) = WorkflowPushTrigger b bi t ti p $ pathsIgnore ++ [path]

instance BranchFilteredTrigger WorkflowPushTrigger where
  filterBranch branch (WorkflowPushTrigger branches bi t ti p pi') = WorkflowPushTrigger (branches ++ [branch]) bi t ti p pi'
  ignoreBranch branch (WorkflowPushTrigger b branchesIgnore t ti p pi') = WorkflowPushTrigger b (branchesIgnore ++ [branch]) t ti p pi'

instance PathFilteredTrigger WorkflowPullRequestTrigger where
  filterPath path w = w {prTriggerPaths = prTriggerPaths w ++ [path]}
  ignorePath path w = w {prTriggerPathsIgnore = prTriggerPathsIgnore w ++ [path]}

instance BranchFilteredTrigger WorkflowPullRequestTrigger where
  filterBranch branch w = w {prTriggerBranches = prTriggerBranches w ++ [branch]}
  ignoreBranch branch w = w {prTriggerBranchesIgnore = prTriggerBranchesIgnore w ++ [branch]}

instance TypeFilteredTrigger WorkflowPullRequestTrigger where
  filterType t w = w {prTriggerTypes = prTriggerTypes w ++ [t]}
