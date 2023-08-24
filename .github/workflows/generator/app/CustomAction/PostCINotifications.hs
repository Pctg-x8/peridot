{-# LANGUAGE NamedFieldPuns #-}

module CustomAction.PostCINotifications (Status (..), Mode (..), Params (..), step) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA

data Status = SuccessStatus | FailureStatus String

data Mode = DiffMode {diffHeadSHA :: String, diffBaseSHA :: String, diffPRNumber :: String, diffPRTitle :: String} | BranchMode

data Params = Params {status :: Status, beginTime :: String, reportName :: String, mode :: Mode}

step :: Params -> GHA.Step
step params = GHA.namedAs "Notify" $ GHA.actionStep "Pctg-x8/ci-notifications-post-invoker@master" withArgs
  where
    withArgs =
      let statusFields = case status params of
            SuccessStatus -> [("status", toJSON "success")]
            FailureStatus jobName -> [("status", toJSON "failure"), ("failure-step", toJSON jobName)]
          modeFields = case mode params of
            DiffMode {diffHeadSHA, diffBaseSHA, diffPRNumber, diffPRTitle} ->
              [ ("mode", toJSON "diff"),
                ("head_sha", toJSON diffHeadSHA),
                ("base_sha", toJSON diffBaseSHA),
                ("pr_number", toJSON diffPRNumber),
                ("pr_title", toJSON diffPRTitle)
              ]
            BranchMode -> [("mode", toJSON "branch")]
       in M.fromList $
            [("begintime", toJSON $ beginTime params), ("report_name", toJSON $ reportName params)]
              <> statusFields
              <> modeFields
