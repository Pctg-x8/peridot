module Workflow.GitHub.Actions.ExpressionBuilder
  ( mkExpression,
    mkRefStepOutputExpression,
    mkNeedsOutputExpression,
    mkNeedsOutputPath,
    mkRefMatrixValueExpression,
    runnerOsExpr,
    runnerOs,
    githubTokenExpr,
    githubToken,
  )
where

import Data.Composition (compose2)
import Data.List (intercalate)

mkExpression :: String -> String
mkExpression expr = "${{ " <> expr <> " }}"

objectPathExpr :: [String] -> String
objectPathExpr = intercalate "."

mkRefStepOutputExpression :: String -> String -> String
mkRefStepOutputExpression stepId name = mkExpression $ objectPathExpr ["steps", stepId, "outputs", name]

mkNeedsOutputPath :: String -> String -> String
mkNeedsOutputPath jobId name = objectPathExpr ["needs", jobId, "outputs", name]

mkNeedsOutputExpression :: String -> String -> String
mkNeedsOutputExpression = mkExpression `compose2` mkNeedsOutputPath

mkRefMatrixValueExpression :: String -> String
mkRefMatrixValueExpression key = mkExpression $ objectPathExpr ["matrix", key]

runnerOsExpr :: String
runnerOsExpr = "runner.os"

runnerOs :: String
runnerOs = mkExpression runnerOsExpr

githubTokenExpr :: String
githubTokenExpr = "secrets.GITHUB_TOKEN"

githubToken :: String
githubToken = mkExpression githubTokenExpr
