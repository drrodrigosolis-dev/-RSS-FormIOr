#' Get responses submitted to a 'FormIO' form
#'
#' Downloads response export JSON from a form endpoint and returns parsed
#' submissions by default.
#'
#' @param base_url Character API base URL. Default:
#'   `"https://submit.digital.gov.bc.ca/app/api/v1"`.
#' @param form_id Optional form ID. If `NULL`, uses cached credentials or
#'   prompts interactively via [AskCredentials()].
#' @param api_key Optional API key. If `NULL`, uses cached credentials or
#'   prompts interactively via [AskCredentials()].
#' @param drafts Logical. Include drafts. Default `FALSE`.
#' @param deleted Logical. Include deleted submissions. Default `FALSE`.
#' @param content.only Logical or character.
#'   - `TRUE` (default): return parsed submissions
#'   - `FALSE`: return full response list (status, headers, parsed data)
#'   - `"raw"`: return raw JSON text
#' @param reenter.credentials Logical. Force re-entry of credentials.
#'
#' @returns Parsed submissions, full response list, or raw JSON depending on
#'   `content.only`.
#' @export
#'
#' @examples
#' \dontrun{
#' GetResponses(form_id = "your_form_id", api_key = "your_api_key")
#' }
GetResponses <- function(
    base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
    form_id = NULL,
    api_key = NULL,
    drafts = FALSE,
    deleted = FALSE,
    content.only = TRUE,
    reenter.credentials = FALSE
) {
  creds <- resolve_credentials(
    form_id = form_id,
    api_key = api_key,
    reenter.credentials = reenter.credentials
  )

  export_url <- paste0(base_url, "/forms/", creds$form_id, "/export")
  query_params <- list(
    format = "json",
    type = "submissions",
    drafts = if (isTRUE(drafts)) "true" else "false",
    deleted = if (isTRUE(deleted)) "true" else "false"
  )

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

  if (is.character(content.only) && tolower(content.only[1]) == "raw") {
    return(raw_json)
  }
  if (isFALSE(content.only)) {
    return(full_submission_data)
  }
  submission_data
}
