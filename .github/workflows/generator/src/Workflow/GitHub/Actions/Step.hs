module Workflow.GitHub.Actions.Step
  ( Step (..),
    emptyStep,
    runStep,
    actionStep,
    StepModifier,
    modifyStepEnv,
    modifyStepWith,
    stepSetWithParam,
    stepUseShell,
  )
where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Composition (compose2)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions.CommonTraits
import Workflow.GitHub.Actions.InternalHelpers (maybeNonEmptyMap, updateLens)

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
modifyStepEnv = updateLens stepEnv $ \s x -> s {stepEnv = x}

modifyStepWith :: (Map String Value -> Map String Value) -> StepModifier
modifyStepWith = updateLens stepWith $ \s x -> s {stepWith = x}

stepSetWithParam :: (ToJSON v) => String -> v -> StepModifier
stepSetWithParam k = modifyStepWith . M.insert k . toJSON

stepUseShell :: String -> StepModifier
stepUseShell name self = self {stepShell = Just name}

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

instance ConditionalElement Step where
  withCondition cond x = x {stepIf = Just cond}

instance IdentifiedElement Step where
  identifiedAs newId x = x {stepId = Just newId}

instance NamedElement Step where
  namedAs newName x = x {stepName = Just newName}
  nameOf = stepName

instance HasEnvironmentVariables Step where
  env = modifyStepEnv `compose2` M.insert

instance DirectoryWorker Step where
  workAt path self = self {stepWorkingDirectory = Just path}
