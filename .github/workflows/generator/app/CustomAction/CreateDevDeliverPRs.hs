module CustomAction.CreateDevDeliverPRs (step) where

import Workflow.GitHub.Actions qualified as GHA

step :: GHA.Step
step =
  GHA.namedAs "Create PullRequest" $
    GHA.env "GITHUB_TOKEN" GHA.githubToken $
      GHA.actionStep "./.github/actions/create-dev-delivery-prs" mempty
