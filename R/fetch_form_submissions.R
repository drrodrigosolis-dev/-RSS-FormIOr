#' Retrieve submission metadata for a form
#'
#' Fetches submission-level metadata (not full response payloads) from a
#' 'FormIO' endpoint.
#'
#' @param base_url Character API base URL. Default:
#'   `"https://submit.digital.gov.bc.ca/app/api/v1"`.
#' @param form_id Optional form ID. If `NULL`, uses cached credentials or
#'   prompts interactively via [AskCredentials()].
#' @param api_key Optional API key. If `NULL`, uses cached credentials or
#'   prompts interactively via [AskCredentials()].
#' @param content.only Logical or character.
#'   - `TRUE` (default): return cleaned metadata data frame
#'   - `FALSE`: return full response list (status, headers, parsed data)
#'   - `"raw"`: return raw JSON text
#' @param reenter.credentials Logical. Force re-entry of credentials.
#' @param AdditionalCols Character vector of extra fields requested via the
#'   `fields` query parameter.
#'
#' @returns Submission metadata data frame, full response list, or raw JSON
#'   depending on `content.only`.
#' @export
fetch_form_submissions <- function(
    base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
    form_id = NULL,
    api_key = NULL,
    content.only = TRUE,
    reenter.credentials = FALSE,
    AdditionalCols = character()
) {
  creds <- resolve_credentials(
    form_id = form_id,
    api_key = api_key,
    reenter.credentials = reenter.credentials
  )

  export_url <- paste0(base_url, "/forms/", creds$form_id, "/submissions")
  query_params <- list()
  if (length(AdditionalCols) > 0) {
    query_params$fields <- AdditionalCols
  }

  resp <- GET(
    url = export_url,
    query = query_params,
    authenticate(user = creds$form_id, password = creds$api_key, type = "basic"),
    add_headers(Accept = "application/json"),
    timeout(60)
  )

  raw_json <- content(resp, as = "text")
  submission_data <- fromJSON(raw_json)

  full_submission_data <- list(
    status = status_code(resp),
    content_type = headers(resp)[["content-type"]],
    content_disposition = headers(resp)[["content-disposition"]],
    submission_data = submission_data
  )

  meta <- tryCatch(
    fetch_form_metadata(base_url = base_url, form_id = creds$form_id, api_key = creds$api_key),
    error = function(e) NULL
  )

  if (is.data.frame(submission_data) && is.list(meta) && is.data.frame(meta$versions)) {
    versions <- meta$versions
    if (all(c("id", "version") %in% names(versions)) && "formVersionId" %in% names(submission_data)) {
      submission_data$version <- versions$version[match(submission_data$formVersionId, versions$id)]
    }
  }

  wanted_cols <- c(
    "formId",
    "submissionId",
    "createdBy",
    "createdAt",
    "version",
    "formVersionId",
    "confirmationId",
    "formSubmissionStatusCode",
    "deleted",
    "formSubmissionAssignedToUserId",
    "formSubmissionAssignedToUsernameIdp",
    "formSubmissionAssignedToEmail",
    "lateEntry"
  )
  keep <- intersect(wanted_cols, names(submission_data))
  if (length(keep) > 0) {
    submission_data <- submission_data %>% select(all_of(keep))
  }

  if (is.character(content.only) && tolower(content.only[1]) == "raw") {
    return(raw_json)
  }
  if (isFALSE(content.only)) {
    return(full_submission_data)
  }
  submission_data
}
