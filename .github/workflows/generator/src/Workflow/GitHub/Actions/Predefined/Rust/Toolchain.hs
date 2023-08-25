module Workflow.GitHub.Actions.Predefined.Rust.Toolchain
  ( step,
    useToolchain,
    useStable,
    useNightly,
    forTarget,
  )
where

import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA

step :: GHA.Step
step = GHA.actionStep "actions-rs/toolchain@v1" M.empty

useToolchain :: String -> GHA.Step -> GHA.Step
useToolchain = GHA.stepSetWithParam "toolchain"

useStable, useNightly :: GHA.Step -> GHA.Step
useStable = useToolchain "stable"
useNightly = useToolchain "nightly"

forTarget :: String -> GHA.Step -> GHA.Step
forTarget = GHA.stepSetWithParam "target"
