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
    workflowJobs,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions.CommonTraits
import Workflow.GitHub.Actions.Concurrency (Concurrency, ConcurrentlyElement (..))
import Workflow.GitHub.Actions.InternalHelpers (applyModifiers, maybeNonEmptyMap)
import Workflow.GitHub.Actions.Job (Job)
import Workflow.GitHub.Actions.Permissions
  ( PermissionControlledElement (..),
    PermissionTable (..),
    maybePermissionTable,
    permissionTable,
  )
import Workflow.GitHub.Actions.WorkflowTriggers (WorkflowTrigger)

data Workflow = Workflow
  { workflowName' :: Maybe String,
    workflowRunName' :: Maybe String,
    workflowOn :: WorkflowTrigger,
    workflowPermissions :: PermissionTable,
    workflowEnv :: Map String String,
    workflowConcurrency :: Maybe Concurrency,
    workflowJobs' :: Map String Job
  }

emptyWorkflow :: WorkflowTrigger -> Workflow
emptyWorkflow on = Workflow Nothing Nothing on (PermissionTable M.empty) M.empty Nothing M.empty

simpleWorkflow :: WorkflowTrigger -> Map String Job -> Workflow
simpleWorkflow on = Workflow Nothing Nothing on (PermissionTable M.empty) M.empty Nothing

buildWorkflow :: [Workflow -> Workflow] -> WorkflowTrigger -> Workflow
buildWorkflow modifiers = applyModifiers modifiers . emptyWorkflow

instance ToJSON Workflow where
  toJSON Workflow {..} =
    object $
      catMaybes
        [ ("name" .=) <$> workflowName',
          ("run-name" .=) <$> workflowRunName',
          Just ("on" .= workflowOn),
          ("permissions" .=) <$> maybePermissionTable workflowPermissions,
          ("env" .=) <$> maybeNonEmptyMap workflowEnv,
          ("concurrency" .=) <$> workflowConcurrency,
          Just ("jobs" .= workflowJobs')
        ]

instance PermissionControlledElement Workflow where
  permit name p = workflowModifyPermissionTable $ PermissionTable . M.insert name p . permissionTable
  grantAll = workflowModifyPermissionTable . const . GrantAll

instance ConcurrentlyElement Workflow where
  concurrentPolicy c self = self {workflowConcurrency = Just c}

instance NamedElement Workflow where
  namedAs = workflowName
  nameOf (Workflow n _ _ _ _ _ _) = n

instance HasEnvironmentVariables Workflow where
  env k v = workflowModifyEnvironmentMap $ M.insert k v

workflowName :: String -> Workflow -> Workflow
workflowName name (Workflow _ rn on pt wEnv c jobs) = Workflow (Just name) rn on pt wEnv c jobs

workflowRunName :: String -> Workflow -> Workflow
workflowRunName runName (Workflow n _ on pt wEnv c jobs) = Workflow n (Just runName) on pt wEnv c jobs

workflowModifyPermissionTable :: (PermissionTable -> PermissionTable) -> Workflow -> Workflow
workflowModifyPermissionTable f (Workflow n rn on pt wEnv c jobs) = Workflow n rn on (f pt) wEnv c jobs

workflowModifyEnvironmentMap :: (Map String String -> Map String String) -> Workflow -> Workflow
workflowModifyEnvironmentMap f (Workflow n rn on pt wEnv c jobs) = Workflow n rn on pt (f wEnv) c jobs

workflowMergeEnvs :: Map String String -> Workflow -> Workflow
workflowMergeEnvs = workflowModifyEnvironmentMap . (<>)

workflowEnvironmentMap :: Map String String -> Workflow -> Workflow
workflowEnvironmentMap = workflowModifyEnvironmentMap . const

workflowModifyJobs :: (Map String Job -> Map String Job) -> Workflow -> Workflow
workflowModifyJobs f (Workflow n rn on pt wEnv c jobs) = Workflow n rn on pt wEnv c $ f jobs

workflowJob :: String -> Job -> Workflow -> Workflow
workflowJob name j = workflowModifyJobs $ M.insert name j

workflowMergeJobs :: Map String Job -> Workflow -> Workflow
workflowMergeJobs = workflowModifyJobs . (<>)

workflowJobs :: Map String Job -> Workflow -> Workflow
workflowJobs = workflowModifyJobs . const
