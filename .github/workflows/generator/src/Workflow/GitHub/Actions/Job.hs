module Workflow.GitHub.Actions.Job
  ( Job (..),
    job,
    jobModifySteps,
    jobAppendSteps,
    jobOutput,
    jobForwardingStepOutput,
    jobRunsOn,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Composition (compose2)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions.CommonTraits
import Workflow.GitHub.Actions.Concurrency (Concurrency, ConcurrentlyElement (..))
import Workflow.GitHub.Actions.Environment (Environment, EnvironmentalElement (..))
import Workflow.GitHub.Actions.ExpressionBuilder (mkRefStepOutputExpression)
import Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyMap, updateLens)
import Workflow.GitHub.Actions.Permissions
  ( PermissionControlledElement (..),
    PermissionTable (..),
    maybeNonEmptyPermissionTable,
    setPermissionTableEntry,
  )
import Workflow.GitHub.Actions.Step (Step)
import Workflow.GitHub.Actions.Strategy (Strategy, StrategyElement (..), emptyStrategy, maybeNonEmptyStrategy, strategyMatrixAddEntry)

data Job = Job
  { jobName :: Maybe String,
    jobPermissions :: PermissionTable,
    jobSteps :: [Step],
    jobNeeds :: Maybe [String],
    jobIf :: Maybe String,
    jobRunsOn' :: [String],
    jobEnvironment :: Maybe Environment,
    jobConcurrency :: Maybe Concurrency,
    jobOutputs :: Map String String,
    jobEnv :: Map String String,
    jobStrategy :: Strategy
  }

job :: [Step] -> Job
job steps =
  Job
    { jobName = Nothing,
      jobPermissions = PermissionTable mempty,
      jobNeeds = Nothing,
      jobSteps = steps,
      jobIf = Nothing,
      jobRunsOn' = ["ubuntu-latest"],
      jobEnvironment = Nothing,
      jobConcurrency = Nothing,
      jobOutputs = mempty,
      jobEnv = mempty,
      jobStrategy = emptyStrategy
    }

instance ToJSON Job where
  toJSON Job {..} =
    object $
      catMaybes
        [ Just ("steps" .= jobSteps),
          ("needs" .=) <$> jobNeeds,
          ("name" .=) <$> jobName,
          ("permissions" .=) <$> maybeNonEmptyPermissionTable jobPermissions,
          ("if" .=) <$> jobIf,
          Just (if length jobRunsOn' == 1 then "runs-on" .= head jobRunsOn' else "runs-on" .= jobRunsOn'),
          ("environment" .=) <$> jobEnvironment,
          ("concurrency" .=) <$> jobConcurrency,
          ("outputs" .=) <$> maybeNonEmptyMap jobOutputs,
          ("env" .=) <$> maybeNonEmptyMap jobEnv,
          ("strategy" .=) <$> maybeNonEmptyStrategy jobStrategy
        ]

instance PermissionControlledElement Job where
  permit = updateLens jobPermissions (\s x -> s {jobPermissions = x}) `compose2` setPermissionTableEntry
  grantAll perm self = self {jobPermissions = GrantAll perm}

instance EnvironmentalElement Job where
  runInEnvironment e self = self {jobEnvironment = Just e}

instance ConcurrentlyElement Job where
  concurrentPolicy c self = self {jobConcurrency = Just c}

instance ConditionalElement Job where
  withCondition cond x = x {jobIf = Just cond}

instance NamedElement Job where
  namedAs newName x = x {jobName = Just newName}
  nameOf = jobName

instance HasEnvironmentVariables Job where
  env = updateLens jobEnv (\s x -> s {jobEnv = x}) `compose2` M.insert

instance StrategyElement Job where
  addStrategyMatrixEntry = updateLens jobStrategy (\s x -> s {jobStrategy = x}) `compose2` strategyMatrixAddEntry

jobModifySteps :: ([Step] -> [Step]) -> Job -> Job
jobModifySteps = updateLens jobSteps (\s x -> s {jobSteps = x})

jobAppendSteps :: [Step] -> Job -> Job
jobAppendSteps = jobModifySteps . flip (<>)

jobOutput :: String -> String -> Job -> Job
jobOutput = updateLens jobOutputs (\s x -> s {jobOutputs = x}) `compose2` M.insert

jobForwardingStepOutput :: String -> String -> Job -> Job
jobForwardingStepOutput stepName key = jobOutput key $ mkRefStepOutputExpression stepName key

jobRunsOn :: [String] -> Job -> Job
jobRunsOn platform self = self {jobRunsOn' = platform}
