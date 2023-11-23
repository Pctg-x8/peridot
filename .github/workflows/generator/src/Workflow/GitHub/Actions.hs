{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Workflow.GitHub.Actions
  ( mkExpression,
    mkRefStepOutputExpression,
    mkNeedsOutputPath,
    mkNeedsOutputExpression,
    Step (..),
    emptyStep,
    runStep,
    actionStep,
    StepModifier,
    stepUseShell,
    modifyStepWith,
    stepSetWithParam,
    Job (..),
    job,
    jobModifySteps,
    jobOutput,
    jobForwardingStepOutput,
    jobRunsOn,
    jobUseEnvironment,
    Environment (..),
    Workflow (..),
    PermissionTable (..),
    PermissionKey (..),
    Permission (..),
    Concurrency (..),
    simpleWorkflow,
    emptyWorkflow,
    applyWorkflowModifiers,
    buildWorkflow,
    workflowName,
    workflowRunName,
    workflowModifyPermissionTable,
    workflowModifyEnvironmentMap,
    workflowMergeEnvs,
    workflowEnvironmentMap,
    workflowConcurrency,
    workflowModifyJobs,
    workflowJob,
    workflowJobs,
    workflowMergeJobs,
    PermissionControlledElement (..),
    Conditional (..),
    IdentifiableElement (..),
    NamedElement (..),
    HasEnvironmentVariables (..),
    DirectoryWorker (..),
    module Workflow.GitHub.Actions.WorkflowTriggers,
  )
where

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.String (IsString (fromString))
import Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyMap)
import Workflow.GitHub.Actions.WorkflowTriggers (WorkflowTrigger)

mkExpression :: String -> String
mkExpression expr = "${{ " <> expr <> " }}"

mkRefStepOutputExpression :: String -> String -> String
mkRefStepOutputExpression stepId name = mkExpression $ "steps." <> stepId <> ".outputs." <> name

mkNeedsOutputPath :: String -> String -> String
mkNeedsOutputPath jobId name = intercalate "." ["needs", jobId, "outputs", name]

mkNeedsOutputExpression :: String -> String -> String
mkNeedsOutputExpression jobId name = mkExpression $ mkNeedsOutputPath jobId name

data Step = Step
  { stepId :: Maybe String,
    stepIf :: Maybe String,
    stepName :: Maybe String,
    stepUses :: Maybe String,
    stepRun :: Maybe String,
    stepShell :: Maybe String,
    stepWith :: Map String Value,
    stepEnv :: Map String String,
    stepWorkingDirectory :: Maybe String
  }

emptyStep :: Step
emptyStep =
  Step
    { stepId = Nothing,
      stepIf = Nothing,
      stepName = Nothing,
      stepUses = Nothing,
      stepRun = Nothing,
      stepShell = Nothing,
      stepWith = M.empty,
      stepEnv = M.empty,
      stepWorkingDirectory = Nothing
    }

runStep :: String -> Step
runStep command = emptyStep {stepRun = Just command}

actionStep :: String -> Map String Value -> Step
actionStep name withArgs = emptyStep {stepUses = Just name, stepWith = withArgs}

type StepModifier = Step -> Step

modifyStepEnv :: (Map String String -> Map String String) -> StepModifier
modifyStepEnv f x@(Step {stepEnv}) = x {stepEnv = f stepEnv}

modifyStepWith :: (Map String Value -> Map String Value) -> StepModifier
modifyStepWith f x = x {stepWith = f $ stepWith x}

stepSetWithParam :: (ToJSON v) => String -> v -> StepModifier
stepSetWithParam k v = modifyStepWith $ M.insert k $ toJSON v

stepUseShell :: String -> StepModifier
stepUseShell name x = x {stepShell = Just name}

instance ToJSON Step where
  toJSON self =
    object $
      catMaybes
        [ ("id" .=) <$> stepId self,
          ("if" .=) <$> stepIf self,
          ("name" .=) <$> stepName self,
          ("uses" .=) <$> stepUses self,
          ("run" .=) <$> stepRun self,
          ("shell" .=) <$> stepShell self,
          ("with" .=) <$> maybeNonEmptyMap (stepWith self),
          ("env" .=) <$> maybeNonEmptyMap (stepEnv self),
          ("working-directory" .=) <$> stepWorkingDirectory self
        ]

data Environment = RepositoryEnvironment String | ForeignEnvironment String String

instance ToJSON Environment where
  toJSON (RepositoryEnvironment s) = toJSON s
  toJSON (ForeignEnvironment s url) = object ["name" .= s, "url" .= url]

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
    jobEnv :: Map String String
  }

job :: [Step] -> Job
job steps =
  Job
    { jobName = Nothing,
      jobPermissions = PermissionTable M.empty,
      jobNeeds = Nothing,
      jobSteps = steps,
      jobIf = Nothing,
      jobRunsOn' = ["ubuntu-latest"],
      jobEnvironment = Nothing,
      jobConcurrency = Nothing,
      jobOutputs = M.empty,
      jobEnv = M.empty
    }

instance ToJSON Job where
  toJSON Job {..} =
    object $
      catMaybes
        [ Just ("steps" .= jobSteps),
          ("needs" .=) <$> jobNeeds,
          ("name" .=) <$> jobName,
          ("permissions" .=) <$> maybePermissionTable jobPermissions,
          ("if" .=) <$> jobIf,
          Just (if length jobRunsOn' == 1 then "runs-on" .= head jobRunsOn' else "runs-on" .= jobRunsOn'),
          ("environment" .=) <$> jobEnvironment,
          ("concurrency" .=) <$> jobConcurrency,
          ("outputs" .=) <$> maybeNonEmptyMap jobOutputs,
          ("env" .=) <$> maybeNonEmptyMap jobEnv
        ]

jobModifySteps :: ([Step] -> [Step]) -> Job -> Job
jobModifySteps f self = self {jobSteps = f $ jobSteps self}

jobOutput :: String -> String -> Job -> Job
jobOutput k v self = self {jobOutputs = M.insert k v $ jobOutputs self}

jobForwardingStepOutput :: String -> String -> Job -> Job
jobForwardingStepOutput stepName key = jobOutput key $ mkRefStepOutputExpression stepName key

jobRunsOn :: [String] -> Job -> Job
jobRunsOn platform self = self {jobRunsOn' = platform}

jobUseEnvironment :: Environment -> Job -> Job
jobUseEnvironment e self = self {jobEnvironment = Just e}

data Permission = PermWrite | PermRead | PermNone

instance ToJSON Permission where
  toJSON PermWrite = toJSON ("write" :: String)
  toJSON PermRead = toJSON ("read" :: String)
  toJSON PermNone = toJSON ("none" :: String)

data PermissionKey
  = ActionsPermission
  | ChecksPermission
  | ContentsPermission
  | DeploymentsPermission
  | DiscussionsPermission
  | IDTokenPermission
  | IssuesPermission
  | PackagesPermission
  | PagesPermission
  | PullRequestsPermission
  | RepositoryProjectsPermission
  | SecurityEventsPermission
  | StatusesPermission
  deriving (Eq, Ord)

instance Show PermissionKey where
  show = \case
    ActionsPermission -> "actions"
    ChecksPermission -> "checks"
    ContentsPermission -> "contents"
    DeploymentsPermission -> "deployments"
    DiscussionsPermission -> "discussions"
    IDTokenPermission -> "id-token"
    IssuesPermission -> "issues"
    PackagesPermission -> "packages"
    PagesPermission -> "pages"
    PullRequestsPermission -> "pull-requests"
    RepositoryProjectsPermission -> "repository-projects"
    SecurityEventsPermission -> "security-events"
    StatusesPermission -> "statuses"

instance ToJSON PermissionKey where
  toJSON = toJSON . show

instance ToJSONKey PermissionKey where
  toJSONKey = toJSONKeyText $ fromString . show

allPermissionKeys :: [PermissionKey]
allPermissionKeys =
  [ ActionsPermission,
    ChecksPermission,
    ContentsPermission,
    DeploymentsPermission,
    DiscussionsPermission,
    IDTokenPermission,
    IssuesPermission,
    PackagesPermission,
    PagesPermission,
    PullRequestsPermission,
    RepositoryProjectsPermission,
    SecurityEventsPermission,
    StatusesPermission
  ]

data PermissionTable = PermissionTable (Map PermissionKey Permission) | GrantAll Permission

instance ToJSON PermissionTable where
  toJSON (PermissionTable p) = toJSON p
  toJSON (GrantAll PermRead) = toJSON ("read-all" :: String)
  toJSON (GrantAll PermWrite) = toJSON ("write-all" :: String)
  toJSON (GrantAll PermNone) = object []

permissionTable :: PermissionTable -> Map PermissionKey Permission
permissionTable (PermissionTable t) = t
permissionTable (GrantAll g) = M.fromList $ map (,g) allPermissionKeys

maybePermissionTable :: PermissionTable -> Maybe PermissionTable
maybePermissionTable (PermissionTable p) = PermissionTable <$> maybeNonEmptyMap p
maybePermissionTable p = Just p

data Concurrency = ConcurrentQueuedGroup String | ConcurrentCancelledGroup String

instance ToJSON Concurrency where
  toJSON (ConcurrentQueuedGroup g) = toJSON g
  toJSON (ConcurrentCancelledGroup g) = object ["group" .= g, "cancel-in-progress" .= True]

data Workflow = Workflow
  { workflowName' :: Maybe String,
    workflowRunName' :: Maybe String,
    workflowOn :: WorkflowTrigger,
    workflowPermissions :: PermissionTable,
    workflowEnv :: Map String String,
    workflowConcurrency' :: Maybe Concurrency,
    workflowJobs' :: Map String Job
  }

instance ToJSON Workflow where
  toJSON Workflow {..} =
    object $
      catMaybes
        [ ("name" .=) <$> workflowName',
          ("run-name" .=) <$> workflowRunName',
          Just ("on" .= workflowOn),
          ("permissions" .=) <$> maybePermissionTable workflowPermissions,
          ("env" .=) <$> maybeNonEmptyMap workflowEnv,
          ("concurrency" .=) <$> workflowConcurrency',
          Just ("jobs" .= workflowJobs')
        ]

emptyWorkflow :: WorkflowTrigger -> Workflow
emptyWorkflow on = Workflow Nothing Nothing on (PermissionTable M.empty) M.empty Nothing M.empty

simpleWorkflow :: WorkflowTrigger -> Map String Job -> Workflow
simpleWorkflow on = Workflow Nothing Nothing on (PermissionTable M.empty) M.empty Nothing

applyWorkflowModifiers :: [Workflow -> Workflow] -> Workflow -> Workflow
applyWorkflowModifiers = flip $ foldl (flip ($))

buildWorkflow :: [Workflow -> Workflow] -> WorkflowTrigger -> Workflow
buildWorkflow modifiers = applyWorkflowModifiers modifiers . emptyWorkflow

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

workflowConcurrency :: Concurrency -> Workflow -> Workflow
workflowConcurrency c (Workflow n rn on pt wEnv _ jobs) = Workflow n rn on pt wEnv (Just c) jobs

workflowModifyJobs :: (Map String Job -> Map String Job) -> Workflow -> Workflow
workflowModifyJobs f (Workflow n rn on pt wEnv c jobs) = Workflow n rn on pt wEnv c $ f jobs

workflowJob :: String -> Job -> Workflow -> Workflow
workflowJob name j = workflowModifyJobs $ M.insert name j

workflowMergeJobs :: Map String Job -> Workflow -> Workflow
workflowMergeJobs = workflowModifyJobs . (<>)

workflowJobs :: Map String Job -> Workflow -> Workflow
workflowJobs = workflowModifyJobs . const

class PermissionControlledElement e where
  permit :: PermissionKey -> Permission -> e -> e
  grantAll :: Permission -> e -> e

  grantWritable :: PermissionKey -> e -> e
  grantWritable = flip permit PermWrite
  grantReadable :: PermissionKey -> e -> e
  grantReadable = flip permit PermRead

instance PermissionControlledElement Job where
  permit key value self = self {jobPermissions = PermissionTable $ M.insert key value $ permissionTable $ jobPermissions self}
  grantAll perm self = self {jobPermissions = GrantAll perm}

instance PermissionControlledElement Workflow where
  permit name p = workflowModifyPermissionTable $ PermissionTable . M.insert name p . permissionTable
  grantAll = workflowModifyPermissionTable . const . GrantAll

class Conditional a where
  withCondition :: String -> a -> a

instance Conditional Step where
  withCondition cond x = x {stepIf = Just cond}

instance Conditional Job where
  withCondition cond x = x {jobIf = Just cond}

class IdentifiableElement a where
  identifiedAs :: String -> a -> a

instance IdentifiableElement Step where
  identifiedAs newId x = x {stepId = Just newId}

class NamedElement a where
  namedAs :: String -> a -> a
  nameOf :: a -> Maybe String

instance NamedElement Step where
  namedAs newName x = x {stepName = Just newName}
  nameOf = stepName

instance NamedElement Job where
  namedAs newName x = x {jobName = Just newName}
  nameOf = jobName

instance NamedElement Workflow where
  namedAs = workflowName
  nameOf (Workflow n _ _ _ _ _ _) = n

class HasEnvironmentVariables a where
  env :: String -> String -> a -> a

instance HasEnvironmentVariables Step where
  env k v = modifyStepEnv $ M.insert k v

instance HasEnvironmentVariables Job where
  env k v self@(Job {jobEnv}) = self {jobEnv = M.insert k v jobEnv}

instance HasEnvironmentVariables Workflow where
  env k v = workflowModifyEnvironmentMap $ M.insert k v

class DirectoryWorker e where
  workAt :: String -> e -> e

instance DirectoryWorker Step where
  workAt path self = self {stepWorkingDirectory = Just path}
