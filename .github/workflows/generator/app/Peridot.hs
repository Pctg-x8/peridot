-- | Peridot specific funcs
module Peridot (setBuildEnv, setBuildEnvSwitch, setCradleBasePath) where

import Data.Char (toUpper)
import Workflow.GitHub.Actions qualified as GHA

-- | set specific `PERIDOT_BUILD_XXX` environment variable
setBuildEnv :: (GHA.HasEnvironmentVariables e) => String -> String -> e -> e
setBuildEnv name = GHA.env ("PERIDOT_BUILD_" <> fmap toUpper name)

-- | set specific `PERIDOT_BUILD_XXX` environment variable switch to "1"
setBuildEnvSwitch :: (GHA.HasEnvironmentVariables e) => String -> e -> e
setBuildEnvSwitch name = setBuildEnv name "1"

setCradleBasePath :: (GHA.HasEnvironmentVariables e) => String -> e -> e
setCradleBasePath = GHA.env "PERIDOT_CLI_CRADLE_BASE"
