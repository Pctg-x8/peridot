module Workflow.GitHub.Actions.JobGroupComposer (depends, JobGroup (..), (~=>)) where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Workflow.GitHub.Actions.Job (Job, jobNeeds)

depends :: [String] -> Job -> Job
depends deps x = x {jobNeeds = Just $ fromMaybe [] (jobNeeds x) <> deps}

type JobMap = Map String Job

class JobGroup a where
  requireBefore :: (JobGroup b) => a -> b -> JobMap
  concurrent :: a -> JobMap

instance JobGroup JobMap where
  requireBefore self = requireJobsBefore self . concurrent
  concurrent = id

instance JobGroup [JobMap] where
  requireBefore self = requireJobsBefore (concurrent self) . concurrent
  concurrent = mconcat

requireJobsBefore :: JobMap -> JobMap -> JobMap
requireJobsBefore preJobs afterJobs = (depends requiredJobNames <$> afterJobs) <> preJobs
  where
    requiredJobNames = M.keys preJobs

(~=>) :: (JobGroup a, JobGroup b) => a -> b -> JobMap
(~=>) = requireBefore

infixl 5 ~=>
