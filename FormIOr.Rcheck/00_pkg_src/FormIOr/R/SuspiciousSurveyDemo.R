#' Suspicious Survey Demo Dataset
#'
#' A synthetic survey-response dataset designed to test
#' [FlagSuspiciousSubmissions()].
#'
#' It includes:
#' - A pre-change (anonymous) phase and post-change (identity-verified) phase.
#' - Intentional distribution shifts between phases (numeric and categorical).
#' - Repeated submission IDs with close timestamps.
#' - Repeated answer fingerprints with different submission IDs.
#' - A low-completeness field (`mostly_missing`) for completeness-filter testing.
#'
#' @format A data frame with 262 rows and 10 variables:
#' \describe{
#'   \item{submissionId}{Character submission identifier. Some IDs are repeated intentionally.}
#'   \item{created}{POSIXct timestamp in UTC.}
#'   \item{age}{Numeric respondent age.}
#'   \item{satisfaction_score}{Numeric score (0-100).}
#'   \item{region}{Categorical region (`North`, `South`, `East`, `West`).}
#'   \item{program}{Categorical program (`A`, `B`, `C`).}
#'   \item{consent}{Categorical consent indicator (`Yes`, `No`).}
#'   \item{comment}{Short free-text category.}
#'   \item{phase_expected}{Expected phase label (`pre`, `post`).}
#'   \item{mostly_missing}{Mostly missing text field for completeness-threshold tests.}
#' }
#'
#' @source Synthetic data generated in `data-raw/make_suspicious_survey_demo.R`.
#' @usage data("SuspiciousSurveyDemo")
#' @examples
#' data("SuspiciousSurveyDemo")
#' str(SuspiciousSurveyDemo)
#'
#' out <- FlagSuspiciousSubmissions(
#'   SuspiciousSurveyDemo,
#'   id_col = "submissionId",
#'   time_col = "created",
#'   cutoff_time = "2026-01-06 00:00:00",
#'   group_action = "split_only",
#'   quiet = TRUE
#' )
#' out$comparability$non_comparable
#' @keywords datasets
"SuspiciousSurveyDemo"
