#' Get metadata for a 'FormIO' form
#'
#' Fetches form metadata from the API and retries the alternate base URL when
#' parsing fails (CHEF compatibility behavior).
#'
#' @param base_url Character API base URL. Default:
#'   `"https://submit.digital.gov.bc.ca/app/api/v1"`.
#' @param form_id Optional form ID. If `NULL`, uses cached credentials or
#'   prompts interactively via [AskCredentials()].
#' @param api_key Optional API key. If `NULL`, uses cached credentials or
#'   prompts interactively via [AskCredentials()].
#' @param reenter.credentials Logical. Force re-entry of credentials.
#'
#' @details
#' Returns a hierarchical named list representing the form configuration and
#' metadata, closely matching the JSON response structure.
#'
#' @returns A named list of form metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_form_metadata(form_id = "your_form_id", api_key = "your_api_key")
#' }
fetch_form_metadata <- function(
    base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
    form_id = NULL,
    api_key = NULL,
    reenter.credentials = FALSE
) {
  creds <- resolve_credentials(
    form_id = form_id,
    api_key = api_key,
    reenter.credentials = reenter.credentials
  )

  fetch_metadata <- function(url) {
    resp <- GET(
      url = url,
      query = list(formID = creds$form_id),
      authenticate(user = creds$form_id, password = creds$api_key, type = "basic"),
      add_headers(Accept = "application/json"),
      timeout(60)
    )

    raw <- content(resp, as = "text")
    parsed <- tryCatch(fromJSON(raw), error = function(e) e)
    list(parsed = parsed, raw = raw)
  }

  primary_url <- paste0(base_url, "/forms/", creds$form_id)
  first <- fetch_metadata(primary_url)
  if (!inherits(first$parsed, "error")) {
    return(invisible(first$parsed))
  }

  alt_base <- alternate_base_url(base_url)
  if (!is.null(alt_base) && !identical(alt_base, base_url)) {
    alt_url <- paste0(alt_base, "/forms/", creds$form_id)
    second <- fetch_metadata(alt_url)
    if (!inherits(second$parsed, "error")) {
      return(invisible(second$parsed))
    }
  }

  stop("Unable to parse form metadata from API response.")
}
