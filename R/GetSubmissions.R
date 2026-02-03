#' Retrieve metadata for submissions of a specific form
#'
#' Fetches a list of submission metadata (not the full submission data) from a
#' Digital.gov.bc.ca form, including drafts, completed, and deleted submissions.
#'
#' @param base_url Character. API base URL. Default: "https://submit.digital.gov.bc.ca/app/api/v1"
#' @param form_id Character. Form identifier. If NULL, read from stored credentials.
#' @param api_key Character. API key / secret. If NULL, read from stored credentials.
#' @param content.only Logical or character.
#'   - `TRUE` (default): return cleaned data.frame of submission metadata
#'   - `FALSE`: return full response object (status, headers, content)
#'   - `"raw"`: return raw JSON response as text
#' @param reenter.credentials Logical. Force re-entry of credentials (default: FALSE)
#' @param AdditionalCols Character vector. Extra field names to request (passed to `fields` query param)
#'
#' @return
#' - Default (`content.only = TRUE`): data.frame containing submission metadata
#'   (formId, submissionId, createdAt, version, confirmationId, status, assigned user, etc.)
#' - `content.only = FALSE`: list with status, headers, and parsed content
#' - `content.only = "raw"`: raw JSON string
#'
#' @export
#'
#'
GetSubmissions <- function(
		base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
		form_id = NULL,
		api_key = NULL,
		content.only = TRUE,
		reenter.credentials = FALSE,
		AdditionalCols = c("")
) {
	# function body ...
}
GetSubmissions<-function(base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
												 form_id = NULL, api_key = NULL,
												 content.only = T, reenter.credentials = F,
												 AdditionalCols = c("")) {

	if (reenter.credentials == T) {
		Form_Info <- c()
		Form_Info <- AskCredentials()
	}

	if (exists(".Form_Info")) {
		Form_Info <- .Form_Info
	}
	if (!exists(".Form_Info")) {
		Form_Info <- c()
		Form_Info <- AskCredentials()
	}
	.Form_Info <<- Form_Info
	form_id <- ifelse(is.null(form_id), Form_Info[1], form_id)
	api_key <- ifelse(is.null(api_key), Form_Info[2], api_key)
	export_url <- paste0(base_url, "/forms/", form_id, "/submissions")

	query_params <- list(
		fields= AdditionalCols
	)

	resp <- GET(
		url = export_url, query = query_params, authenticate(
			user = form_id,
			password = api_key, type = "basic"
		), add_headers(Accept = "application/json"),
		timeout(60)
	)
	submission_data <- fromJSON(content(resp, as = "text"))
	full_submission_data <- list(
		status = status_code(resp),
		content_type = headers(resp)[["content-type"]], content_disposition = headers(resp)[["content-disposition"]],
		submission_data = submission_data
	)

	versions<-GetFormMetadata()$versions %>% select("formId", "version", "id", "published")
	submission_data$version<-versions$version[match(submission_data$formVersionId, versions$id)]

submission_data <- submission_data %>% select(
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
  "formSubmissionAssignedToUsernameIdp", "formSubmissionAssignedToEmail",
  "lateEntry"
)
	if (content.only == F) {
		return(full_submission_data)
	}
	if (content.only == T) {
		return(submission_data)
	}
	if (content.only == "raw") {
		return(content(resp, as = "text"))
	}
}
