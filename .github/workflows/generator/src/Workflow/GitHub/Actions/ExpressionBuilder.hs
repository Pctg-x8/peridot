module Workflow.GitHub.Actions.ExpressionBuilder
  ( mkExpression,
    mkRefStepOutputExpression,
    mkNeedsOutputExpression,
    mkNeedsOutputPath,
    mkRefMatrixValueExpression,
  )
where

import Data.List (intercalate)

mkExpression :: String -> String
mkExpression expr = "${{ " <> expr <> " }}"

mkRefStepOutputExpression :: String -> String -> String
mkRefStepOutputExpression stepId name = mkExpression $ "steps." <> stepId <> ".outputs." <> name

mkNeedsOutputPath :: String -> String -> String
mkNeedsOutputPath jobId name = intercalate "." ["needs", jobId, "outputs", name]

mkNeedsOutputExpression :: String -> String -> String
mkNeedsOutputExpression jobId name = mkExpression $ mkNeedsOutputPath jobId name

mkRefMatrixValueExpression :: String -> String
mkRefMatrixValueExpression key = mkExpression $ "matrix." <> key
