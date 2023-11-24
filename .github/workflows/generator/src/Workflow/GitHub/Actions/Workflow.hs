module Workflow.GitHub.Actions.Workflow
  ( Workflow (..),
    emptyWorkflow,
    simpleWorkflow,
    buildWorkflow,
    workflowRunName,
    workflowMergeJobs,
    workflowMergeEnvs,
    workflowEnvironmentMap,
    workflowJob,
    workflowReplaceJobs,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions.CommonTraits
import Workflow.GitHub.Actions.Concurrency (Concurrency, ConcurrentlyElement (..))
import Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyMap)
import Workflow.GitHub.Actions.Job (Job)
import Workflow.GitHub.Actions.Permissions
  ( PermissionControlledElement (..),
    PermissionTable (..),
    maybeNonEmptyPermissionTable,
    permissionTable,
  )
import Workflow.GitHub.Actions.WorkflowTriggers (WorkflowTrigger)

data Workflow = Workflow
  { workflowName :: Maybe String,
    workflowRunName' :: Maybe String,
    workflowOn :: WorkflowTrigger,
    workflowPermissions :: PermissionTable,
    workflowEnv :: Map String String,
    workflowConcurrency :: Maybe Concurrency,
    workflowJobs :: Map String Job
  }

emptyWorkflow :: WorkflowTrigger -> Workflow
emptyWorkflow on =
  Workflow
    { workflowName = Nothing,
      workflowRunName' = Nothing,
      workflowOn = on,
      workflowPermissions = PermissionTable M.empty,
      workflowEnv = M.empty,
      workflowConcurrency = Nothing,
      workflowJobs = M.empty
    }

simpleWorkflow :: WorkflowTrigger -> Map String Job -> Workflow
simpleWorkflow on jobs = (emptyWorkflow on) {workflowJobs = jobs}

buildWorkflow :: [Workflow -> Workflow] -> WorkflowTrigger -> Workflow
buildWorkflow modifiers on = foldr ($) (emptyWorkflow on) modifiers

instance ToJSON Workflow where
  toJSON Workflow {..} =
    object $
      catMaybes
        [ ("name" .=) <$> workflowName,
          ("run-name" .=) <$> workflowRunName',
          Just ("on" .= workflowOn),
          ("permissions" .=) <$> maybeNonEmptyPermissionTable workflowPermissions,
          ("env" .=) <$> maybeNonEmptyMap workflowEnv,
          ("concurrency" .=) <$> workflowConcurrency,
          Just ("jobs" .= workflowJobs)
        ]

instance PermissionControlledElement Workflow where
  permit name p = workflowModifyPermissionTable $ PermissionTable . M.insert name p . permissionTable
  grantAll = workflowModifyPermissionTable . const . GrantAll

instance ConcurrentlyElement Workflow where
  concurrentPolicy c self = self {workflowConcurrency = Just c}

instance NamedElement Workflow where
  namedAs name self = self {workflowName = Just name}
  nameOf = workflowName

instance HasEnvironmentVariables Workflow where
  env k v = workflowModifyEnvironmentMap $ M.insert k v

workflowRunName :: String -> Workflow -> Workflow
workflowRunName runName self = self {workflowRunName' = Just runName}

workflowModifyPermissionTable :: (PermissionTable -> PermissionTable) -> Workflow -> Workflow
workflowModifyPermissionTable f self = self {workflowPermissions = f $ workflowPermissions self}

workflowModifyEnvironmentMap :: (Map String String -> Map String String) -> Workflow -> Workflow
workflowModifyEnvironmentMap f self = self {workflowEnv = f $ workflowEnv self}

workflowMergeEnvs :: Map String String -> Workflow -> Workflow
workflowMergeEnvs = workflowModifyEnvironmentMap . (<>)

workflowEnvironmentMap :: Map String String -> Workflow -> Workflow
workflowEnvironmentMap = workflowModifyEnvironmentMap . const

workflowModifyJobs :: (Map String Job -> Map String Job) -> Workflow -> Workflow
workflowModifyJobs f self = self {workflowJobs = f $ workflowJobs self}

workflowJob :: String -> Job -> Workflow -> Workflow
workflowJob name j = workflowModifyJobs $ M.insert name j

workflowMergeJobs :: Map String Job -> Workflow -> Workflow
workflowMergeJobs = workflowModifyJobs . (<>)

workflowReplaceJobs :: Map String Job -> Workflow -> Workflow
workflowReplaceJobs = workflowModifyJobs . const
