
let TypedTrigger = \(t: Type) ->
    { Type = { types : Optional (List t) }
    , default = { types = None (List t) }
    }
let BranchTargetTriggerCommonParams =
    { Type =
        { branches : Optional (List Text)
        , branches-ignore : Optional (List Text)
        , tags : Optional (List Text)
        , tags-ignore : Optional (List Text)
        }
    , default =
        { branches = None (List Text)
        , branches-ignore = None (List Text)
        , tags = None (List Text)
        , tags-ignore = None (List Text)
        }
    }
let PathTrigger =
    { Type = { paths : Optional (List Text), paths-ignore : Optional (List Text) }
    , default = { paths = None (List Text), paths-ignore = None (List Text) }
    }

let CheckRunTriggerTypes = < created | rerequested | completed | requested_action >
let OnCheckRun = TypedTrigger CheckRunTriggerTypes

let CheckSuiteTriggerTypes = < completed | requested | rerequested >
let OnCheckSuite = TypedTrigger CheckSuiteTriggerTypes

let IssueCommentTriggerTypes = < created | edited | deleted >
let OnIssueComment = TypedTrigger IssueCommentTriggerTypes

let IssuesTriggerTypes = <
    | opened
    | edited
    | deleted
    | transferred
    | pinned
    | unpinned
    | closed
    | reopened
    | assigned
    | unassigned
    | labeled
    | unlabeled
    | locked
    | unlocked
    | milestoned
    | demilestoned
    >
let OnIssues = TypedTrigger IssuesTriggerTypes

let LabelTriggerTypes = < created | edited | deleted >
let OnLabel = TypedTrigger LabelTriggerTypes

let MilestoneTriggerTypes = < created | closed | opened | edited | deleted >
let OnMilestone = TypedTrigger MilestoneTriggerTypes

let ProjectTriggerTypes = < created | updated | closed | reopened | edited | deleted >
let OnProject = TypedTrigger ProjectTriggerTypes

let ProjectCardTriggerTypes = < created | moved | converted | edited | deleted >
let OnProjectCard = TypedTrigger ProjectCardTriggerTypes

let ProjectColumnTriggerTypes = < created | updated | moved | deleted >
let OnProjectColumn = TypedTrigger ProjectColumnTriggerTypes

let PullRequestTriggerTypes = <
    | assigned
    | unassigned
    | labeled
    | unlabeled
    | opened
    | edited
    | closed
    | reopened
    | synchronize
    | ready_for_review
    | locked
    | unlocked
    | review_requested
    | review_request_removed
    >
let OnPullRequest =
    { Type = BranchTargetTriggerCommonParams.Type //\\ (TypedTrigger PullRequestTriggerTypes).Type
    , default = BranchTargetTriggerCommonParams.default /\ (TypedTrigger PullRequestTriggerTypes).default
    }

let PullRequestReviewTriggerTypes = < submitted | edited | dismissed >
let OnPullRequestReview = TypedTrigger PullRequestReviewTriggerTypes

let PullRequestReviewCommentTriggerTypes = < created | edited | deleted >
let OnPullRequestReviewComment = TypedTrigger PullRequestReviewCommentTriggerTypes

let OnPullRequestTarget = 
    { Type = BranchTargetTriggerCommonParams.Type //\\ (TypedTrigger PullRequestTriggerTypes).Type //\\ PathTrigger.Type
    , default = BranchTargetTriggerCommonParams.default /\ (TypedTrigger PullRequestTriggerTypes).default /\ PathTrigger.default
    }

let OnPush =
    { Type = BranchTargetTriggerCommonParams.Type //\\ PathTrigger.Type
    , default = BranchTargetTriggerCommonParams.default /\ PathTrigger.default
    }

let RegistryPackageTriggerTypes = < published | updated >
let OnRegistryPackage = TypedTrigger RegistryPackageTriggerTypes

let ReleaseTriggerTypes = < published | unpublished | created | edited | deleted | prereleased | released >
let OnRelease = TypedTrigger ReleaseTriggerTypes

let WatchTriggerTypes = < started >
let OnWatch = TypedTrigger WatchTriggerTypes

let WorkflowRunTriggerTypes = < completed | requested >
let OnWorkflowRun =
    { Type = (TypedTrigger WorkflowRunTriggerTypes).Type //\\
        { workflows : List Text, branches : Optional (List Text), branches-ignore : Optional (List Text) }
    , default = (TypedTrigger WorkflowRunTriggerTypes).default /\
        { branches = None (List Text), branches-ignore = None (List Text) }
    }

let Schedule =
    { Type = { cron : Text }
    , default = {=}
    }

let OnDetails =
    { Type =
        { check_run : Optional OnCheckRun.Type
        , check_suite : Optional OnCheckSuite.Type
        , issue_comment : Optional OnIssueComment.Type
        , issues : Optional OnIssues.Type
        , label : Optional OnLabel.Type
        , milestone : Optional OnMilestone.Type
        , project : Optional OnProject.Type
        , project_card : Optional OnProjectCard.Type
        , project_column : Optional OnProjectColumn.Type
        , pull_request : Optional OnPullRequest.Type
        , pull_request_review : Optional OnPullRequestReview.Type
        , pull_request_review_comment : Optional OnPullRequestReviewComment.Type
        , pull_request_target : Optional OnPullRequestTarget.Type
        , push: Optional OnPush.Type
        , registry_package : Optional OnRegistryPackage.Type
        , release : Optional OnRelease.Type
        , watch : Optional OnWatch.Type
        , workflow_run : Optional OnWorkflowRun.Type
        , schedule : Optional (List Schedule.Type)
        }
    , default =
        { check_run = None OnCheckRun.Type
        , check_suite = None OnCheckSuite.Type
        , issue_comment = None OnIssueComment.Type
        , issues = None OnIssues.Type
        , label = None OnLabel.Type
        , milestone = None OnMilestone.Type
        , project = None OnProject.Type
        , project_card = None OnProjectCard.Type
        , project_column = None OnProjectColumn.Type
        , pull_request = None OnPullRequest.Type
        , pull_request_review = None OnPullRequestReview.Type
        , pull_request_review_comment = None OnPullRequestReviewComment.Type
        , pull_request_target = None OnPullRequestTarget.Type
        , push = None OnPush.Type
        , registry_package = None OnRegistryPackage.Type
        , release = None OnRelease.Type
        , watch = None OnWatch.Type
        , workflow_run = None OnWorkflowRun.Type
        , schedule = None (List Schedule.Type)
        }
    }

let UnparameterizedTrigger = <
    | check_run
    | check_suite
    | create
    | delete
    | deployment
    | deployment_status
    | fork
    | gollum
    | issue_comment
    | issues
    | label
    | milestone
    | page_build
    | project
    | project_card
    | project_column
    | public
    | pull_request
    | pull_request_review
    | pull_request_review_comment
    | pull_request_target
    | push
    | registry_package
    | release
    | status
    | watch
    | workflow_dispatch
    | repository_dispatch
    >
let On = < Single : UnparameterizedTrigger | Multiple : List UnparameterizedTrigger | Detailed : OnDetails.Type >

in { On
   , UnparameterizedTrigger
   , OnDetails
   , OnCheckRun
   , CheckRunTriggerTypes
   , OnCheckSuite
   , CheckSuiteTriggerTypes
   , OnIssueComment
   , IssueCommentTriggerTypes
   , OnIssues
   , IssuesTriggerTypes
   , OnLabel
   , LabelTriggerTypes
   , OnMilestone
   , MilestoneTriggerTypes
   , OnProject
   , ProjectTriggerTypes
   , OnProjectCard
   , ProjectCardTriggerTypes
   , OnProjectColumn
   , ProjectColumnTriggerTypes
   , OnPullRequest
   , PullRequestTriggerTypes
   , OnPullRequestReview
   , PullRequestReviewTriggerTypes
   , OnPullRequestReviewComment
   , PullRequestReviewCommentTriggerTypes
   , OnPullRequestTarget
   , OnPush
   , OnRegistryPackage
   , RegistryPackageTriggerTypes
   , OnRelease
   , ReleaseTriggerTypes
   , OnWatch
   , WatchTriggerTypes
   , OnWorkflowRun
   , WorkflowRunTriggerTypes
   , Schedule
   }
