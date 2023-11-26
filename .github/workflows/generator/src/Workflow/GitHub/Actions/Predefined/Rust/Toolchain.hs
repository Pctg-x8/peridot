module Workflow.GitHub.Actions.Predefined.Rust.Toolchain
  ( step,
    useToolchain,
    useStable,
    useNightly,
    forTarget,
  )
where

import Workflow.GitHub.Actions qualified as GHA

step :: GHA.Step
step = GHA.actionStep "actions-rs/toolchain@v1" mempty

useToolchain :: String -> GHA.StepModifier
useToolchain = GHA.stepSetWithParam "toolchain"

useStable, useNightly :: GHA.StepModifier
useStable = useToolchain "stable"
useNightly = useToolchain "nightly"

forTarget :: String -> GHA.StepModifier
forTarget = GHA.stepSetWithParam "target"
