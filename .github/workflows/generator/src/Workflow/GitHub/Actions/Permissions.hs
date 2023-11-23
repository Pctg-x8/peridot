module Workflow.GitHub.Actions.Permissions
  ( Permission (..),
    PermissionKey (..),
    PermissionTable (..),
    permissionTable,
    maybePermissionTable,
    PermissionControlledElement (..),
  )
where

import Data.Aeson (ToJSON (..), ToJSONKey (..), object)
import Data.Aeson.Types (toJSONKeyText)
import Data.Map (Map)
import Data.Map qualified as M
import Data.String (IsString (fromString))
import Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyMap)

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
  show ActionsPermission = "actions"
  show ChecksPermission = "checks"
  show ContentsPermission = "contents"
  show DeploymentsPermission = "deployments"
  show DiscussionsPermission = "discussions"
  show IDTokenPermission = "id-token"
  show IssuesPermission = "issues"
  show PackagesPermission = "packages"
  show PagesPermission = "pages"
  show PullRequestsPermission = "pull-requests"
  show RepositoryProjectsPermission = "repository-projects"
  show SecurityEventsPermission = "security-events"
  show StatusesPermission = "statuses"

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

class PermissionControlledElement e where
  permit :: PermissionKey -> Permission -> e -> e
  grantAll :: Permission -> e -> e

  grantWritable :: PermissionKey -> e -> e
  grantWritable = flip permit PermWrite
  grantReadable :: PermissionKey -> e -> e
  grantReadable = flip permit PermRead
