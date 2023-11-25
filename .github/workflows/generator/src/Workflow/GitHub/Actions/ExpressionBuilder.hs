module Workflow.GitHub.Actions.ExpressionBuilder
  ( mkExpression,
    mkRefStepOutputExpression,
    mkNeedsOutputExpression,
    mkNeedsOutputPath,
    mkRefMatrixValueExpression,
  )
where

import Data.Composition (compose2)
import Data.List (intercalate)

mkExpression :: String -> String
mkExpression expr = "${{ " <> expr <> " }}"

objectRefExpr :: [String] -> String
objectRefExpr = intercalate "."

mkRefStepOutputExpression :: String -> String -> String
mkRefStepOutputExpression stepId name = mkExpression $ objectRefExpr ["steps", stepId, "outputs", name]

mkNeedsOutputPath :: String -> String -> String
mkNeedsOutputPath jobId name = objectRefExpr ["needs", jobId, "outputs", name]

mkNeedsOutputExpression :: String -> String -> String
mkNeedsOutputExpression = mkExpression `compose2` mkNeedsOutputPath

mkRefMatrixValueExpression :: String -> String
mkRefMatrixValueExpression key = mkExpression $ objectRefExpr ["matrix", key]
