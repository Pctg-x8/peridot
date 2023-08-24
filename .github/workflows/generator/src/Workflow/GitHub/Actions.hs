{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Workflow.GitHub.Actions
  ( mkExpression,
    mkRefStepOutputExpression,
    mkNeedsOutputPath,
    mkNeedsOutputExpression,
    Step (..),
    step,
    runStep,
    actionStep,
    stepUseShell,
    Job (..),
    job,
    jobModifySteps,
    jobOutput,
    Workflow (..),
    WorkflowTrigger (..),
    PermissionTable (..),
    PermissionKey (..),
    Permission (..),
    Concurrency (..),
    workflowPullRequestTrigger,
    workflowPushTrigger,
    filterPath,
    ignorePath,
    filterBranch,
    ignoreBranch,
    TypeFilteredTrigger (..),
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
  )
where

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.List (intercalate)
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.String (IsString (fromString))

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
    uses :: Maybe String,
    run :: Maybe String,
    shell :: Maybe String,
    with :: Map String Value,
    stepEnv :: Map String String,
    stepWorkingDirectory :: Maybe String
  }

step :: Step
step =
  Step
    { stepId = Nothing,
      stepIf = Nothing,
      stepName = Nothing,
      uses = Nothing,
      run = Nothing,
      shell = Nothing,
      with = M.empty,
      stepEnv = M.empty,
      stepWorkingDirectory = Nothing
    }

runStep :: String -> Step
runStep command = step {run = Just command}

actionStep :: String -> Map String Value -> Step
actionStep name withArgs = step {uses = Just name, with = withArgs}

modifyStepEnv :: (Map String String -> Map String String) -> Step -> Step
modifyStepEnv f x@(Step {stepEnv}) = x {stepEnv = f stepEnv}

stepUseShell :: String -> Step -> Step
stepUseShell name x = x {shell = Just name}

instance ToJSON Step where
  toJSON self =
    object $
      catMaybes
        [ ("id" .=) <$> stepId self,
          ("if" .=) <$> stepIf self,
          ("name" .=) <$> stepName self,
          ("uses" .=) <$> uses self,
          ("run" .=) <$> run self,
          ("shell" .=) <$> shell self,
          ("with" .=) <$> maybeNonEmptyMap (with self),
          ("env" .=) <$> maybeNonEmptyMap (stepEnv self),
          ("working-directory" .=) <$> stepWorkingDirectory self
        ]

data Environment = LocalEnvironment String | RemoteEnvironment String String

instance ToJSON Environment where
  toJSON (LocalEnvironment s) = toJSON s
  toJSON (RemoteEnvironment s url) = object ["name" .= s, "url" .= url]

data Job = Job
  { name :: Maybe String,
    permissions :: PermissionTable,
    steps :: [Step],
    needs :: Maybe [String],
    jobIf :: Maybe String,
    runsOn :: [String],
    environment :: Maybe Environment,
    concurrency :: Maybe Concurrency,
    outputs :: Map String String,
    jobEnv :: Map String String
  }

job :: [Step] -> Job
job steps =
  Job
    { name = Nothing,
      permissions = PermissionTable M.empty,
      needs = Nothing,
      steps = steps,
      jobIf = Nothing,
      runsOn = pure "ubuntu-latest",
      environment = Nothing,
      concurrency = Nothing,
      outputs = M.empty,
      jobEnv = M.empty
    }

instance ToJSON Job where
  toJSON (Job {name, permissions, steps, needs, jobIf, runsOn, environment, concurrency, outputs, jobEnv}) =
    object $
      catMaybes
        [ Just ("steps" .= steps),
          ("needs" .=) <$> needs,
          ("name" .=) <$> name,
          ("permissions" .=) <$> maybePermissionTable permissions,
          ("if" .=) <$> jobIf,
          Just (if length runsOn == 1 then "runs-on" .= head runsOn else "runs-on" .= runsOn),
          ("environment" .=) <$> environment,
          ("concurrency" .=) <$> concurrency,
          ("outputs" .=) <$> maybeNonEmptyMap outputs,
          ("env" .=) <$> maybeNonEmptyMap jobEnv
        ]

jobModifySteps :: ([Step] -> [Step]) -> Job -> Job
jobModifySteps f self@(Job {steps}) = self {steps = f steps}

jobOutput :: String -> String -> Job -> Job
jobOutput k v self = self {outputs = M.insert k v $ outputs self}

data WorkflowTrigger = OnEvent String | OnEvents [String] | OnEventsDetailed (Maybe WorkflowPullRequestTrigger) (Maybe WorkflowPullRequestTrigger) (Maybe WorkflowPushTrigger)

instance ToJSON WorkflowTrigger where
  toJSON (OnEvent event) = toJSON event
  toJSON (OnEvents events) = toJSON events
  toJSON (OnEventsDetailed onPullRequest onPullRequestTarget onPush) =
    object $
      catMaybes
        [ ("pull_request" .=) <$> onPullRequest,
          ("pull_request_target" .=) <$> onPullRequestTarget,
          ("push" .=) <$> onPush
        ]

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
workflowPullRequestTrigger = WorkflowPullRequestTrigger [] [] [] [] []

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

maybePermissionTable :: PermissionTable -> Maybe PermissionTable
maybePermissionTable (PermissionTable p) = PermissionTable <$> maybeNonEmptyMap p
maybePermissionTable p = Just p

maybeNonEmptyMap :: Map k v -> Maybe (Map k v)
maybeNonEmptyMap m = if M.null m then Nothing else Just m

maybeNonEmptyList :: [a] -> Maybe [a]
maybeNonEmptyList xs = if L.null xs then Nothing else Just xs

data Concurrency = ConcurrentQueuedGroup String | ConcurrentCancelledGroup String

instance ToJSON Concurrency where
  toJSON (ConcurrentQueuedGroup g) = toJSON g
  toJSON (ConcurrentCancelledGroup g) = object ["group" .= g, "cancel-in-progress" .= True]

data Workflow = Workflow (Maybe String) (Maybe String) WorkflowTrigger PermissionTable (Map String String) (Maybe Concurrency) (Map String Job)

instance ToJSON Workflow where
  toJSON (Workflow name runName on permissions wEnv concurrency jobs) =
    object $
      catMaybes
        [ ("name" .=) <$> name,
          ("run-name" .=) <$> runName,
          Just ("on" .= on),
          ("permissions" .=) <$> maybePermissionTable permissions,
          ("env" .=) <$> maybeNonEmptyMap wEnv,
          ("concurrency" .=) <$> concurrency,
          Just ("jobs" .= jobs)
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

permissionTable :: PermissionTable -> Map PermissionKey Permission
permissionTable (PermissionTable t) = t
permissionTable (GrantAll g) = M.fromList $ map (,g) allPermissionKeys

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

instance PermissionControlledElement Job where
  permit key value self@(Job {permissions}) = self {permissions = PermissionTable $ M.insert key value $ permissionTable permissions}
  grantAll perm self = self {permissions = GrantAll perm}

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

instance NamedElement Step where
  namedAs newName x = x {stepName = Just newName}

instance NamedElement Job where
  namedAs newName x = x {name = Just newName}

instance NamedElement Workflow where
  namedAs = workflowName

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
