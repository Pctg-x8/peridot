module Workflow.GitHub.Actions.Job
  ( Job (..),
    job,
    jobModifySteps,
    jobOutput,
    jobForwardingStepOutput,
    jobRunsOn,
    jobAddStrategyMatrix,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Workflow.GitHub.Actions.CommonTraits
import Workflow.GitHub.Actions.Concurrency (Concurrency, ConcurrentlyElement (..))
import Workflow.GitHub.Actions.Environment (Environment, EnvironmentalElement (..))
import Workflow.GitHub.Actions.ExpressionBuilder (mkRefStepOutputExpression)
import Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyMap)
import Workflow.GitHub.Actions.Permissions
  ( PermissionControlledElement (..),
    PermissionTable (..),
    maybeNonEmptyPermissionTable,
    permissionTable,
  )
import Workflow.GitHub.Actions.Step (Step)
import Workflow.GitHub.Actions.Strategy (Strategy, strategy, strategyMatrixEntry)

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
    jobStrategy :: Maybe Strategy
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
      jobStrategy = Nothing
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
          ("strategy" .=) <$> jobStrategy
        ]

instance PermissionControlledElement Job where
  permit key value self = self {jobPermissions = PermissionTable $ M.insert key value $ permissionTable $ jobPermissions self}
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
  env k v self = self {jobEnv = M.insert k v $ jobEnv self}

jobModifySteps :: ([Step] -> [Step]) -> Job -> Job
jobModifySteps f self = self {jobSteps = f $ jobSteps self}

jobOutput :: String -> String -> Job -> Job
jobOutput k v self = self {jobOutputs = M.insert k v $ jobOutputs self}

jobForwardingStepOutput :: String -> String -> Job -> Job
jobForwardingStepOutput stepName key = jobOutput key $ mkRefStepOutputExpression stepName key

jobRunsOn :: [String] -> Job -> Job
jobRunsOn platform self = self {jobRunsOn' = platform}

jobAddStrategyMatrix :: (ToJSON v) => String -> v -> Job -> Job
jobAddStrategyMatrix key values self = self {jobStrategy = Just $ strategyMatrixEntry key values $ fromMaybe strategy $ jobStrategy self}
