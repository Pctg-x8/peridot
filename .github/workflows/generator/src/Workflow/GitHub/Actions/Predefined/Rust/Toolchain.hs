module Workflow.GitHub.Actions.Predefined.Rust.Toolchain (Params (..), defaultParams, step) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions qualified as GHA

data Params = Params
  { toolchain :: Maybe String,
    target :: Maybe String,
    _default :: Maybe String,
    override :: Maybe String,
    profile :: Maybe String,
    components :: [String]
  }

defaultParams :: Params
defaultParams =
  Params
    { toolchain = Nothing,
      target = Nothing,
      _default = Nothing,
      override = Nothing,
      profile = Nothing,
      components = []
    }

step :: Params -> GHA.Step
step params =
  GHA.namedAs "Install Rust Toolchain" $
    GHA.actionStep "actions-rs/toolchain@v1" $
      M.fromList $
        catMaybes
          [ ("toolchain",) . toJSON <$> toolchain params,
            ("target",) . toJSON <$> target params,
            ("default",) . toJSON <$> _default params,
            ("override",) . toJSON <$> override params,
            ("profile",) . toJSON <$> profile params,
            ("components",) . toJSON
              <$> if null $ components params then Nothing else Just $ intercalate "," $ components params
          ]
