{-# LANGUAGE NamedFieldPuns #-}

module CustomAction.PostCINotifications (Status (..), Mode (..), DiffHashPair (..), PullRequestInfo (..), currentPullRequestDiffMode, Params (..), step) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA

data Status = SuccessStatus | FailureStatus String

data DiffHashPair = DiffHashPair {diffBase :: String, diffHead :: String}

data PullRequestInfo = PullRequestInfo {prTitle :: String, prNumber :: String}

data Mode = DiffMode DiffHashPair PullRequestInfo | BranchMode

currentPullRequestDiffMode :: Mode
currentPullRequestDiffMode = DiffMode hashPair pr
  where
    hashPair =
      DiffHashPair
        { diffBase = GHA.mkExpression "github.event.pull_request.base.sha",
          diffHead = GHA.mkExpression "github.event.pull_request.head.sha"
        }
    pr =
      PullRequestInfo
        { prTitle = GHA.mkExpression "github.event.pull_request.title",
          prNumber = GHA.mkExpression "github.event.number"
        }

data Params = Params {status :: Status, beginTime :: String, reportName :: String, mode :: Mode}

step :: Params -> GHA.Step
step params = GHA.namedAs "Notify" $ GHA.actionStep "Pctg-x8/ci-notifications-post-invoker@master" withArgs
  where
    withArgs =
      let statusFields = M.fromList $ case status params of
            SuccessStatus -> [("status", toJSON "success")]
            FailureStatus jobName -> [("status", toJSON "failure"), ("failure_step", toJSON jobName)]
          modeFields = M.fromList $ case mode params of
            DiffMode hashPair pr ->
              [ ("mode", toJSON "diff"),
                ("head_sha", toJSON $ diffHead hashPair),
                ("base_sha", toJSON $ diffBase hashPair),
                ("pr_number", toJSON $ prNumber pr),
                ("pr_title", toJSON $ prTitle pr)
              ]
            BranchMode -> [("mode", toJSON "branch")]
          commonParams = M.fromList [("begintime", toJSON $ beginTime params), ("report_name", toJSON $ reportName params)]
       in mconcat [commonParams, statusFields, modeFields]
