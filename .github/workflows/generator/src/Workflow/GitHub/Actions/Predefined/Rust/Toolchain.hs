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

useToolchain :: String -> GHA.StepModifier
useToolchain = GHA.stepSetWithParam "toolchain"

useStable, useNightly :: GHA.StepModifier
useStable = useToolchain "stable"
useNightly = useToolchain "nightly"

forTarget :: String -> GHA.StepModifier
forTarget = GHA.stepSetWithParam "target"
