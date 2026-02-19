#' Ask for Form credentials
#'
#' Collects the Form ID and API key and caches them for this R session.
#'
#' If `form_id` and `api`/`api_key` are provided, prompts are skipped. If either
#' value is missing, the function prompts only for the missing value(s) in an
#' interactive session.
#'
#' @param form_id Optional form ID.
#' @param api Optional API key alias.
#' @param api_key Optional API key.
#'
#' @returns Named character vector with `ID` and `Key`.
#' @export
#'
#' @examples
#' \dontrun{
#' AskCredentials()
#' AskCredentials(form_id = "myformID", api = "myapiToken")
#' }
AskCredentials <- function(form_id = NULL, api = NULL, api_key = NULL) {
  normalize_value <- function(x) {
    if (is.null(x) || length(x) == 0) return(NULL)
    value <- trimws(as.character(x[[1]]))
    if (is.na(value) || !nzchar(value)) return(NULL)
    value
  }

  form_id <- normalize_value(form_id)
  api <- normalize_value(api)
  api_key <- normalize_value(api_key)

  if (!is.null(api) && !is.null(api_key) && !identical(api, api_key)) {
    stop("api and api_key were both provided but do not match.")
  }

  resolved_api <- api_key
  if (is.null(resolved_api)) {
    resolved_api <- api
  }

  if ((is.null(form_id) || is.null(resolved_api)) && !interactive()) {
    stop("AskCredentials() needs form_id and api/api_key in non-interactive sessions.")
  }

  if (is.null(form_id)) {
    message("You can find your Form ID on the form settings page URL (after '=').")
    form_id <- trimws(readline(prompt = "Form ID: "))
  }

  if (is.null(resolved_api)) {
    message("You can generate an API key from the form 'Manage Form' page.")
    resolved_api <- trimws(readline(prompt = "Form API key: "))
  }

  if (!nzchar(form_id) || !nzchar(resolved_api)) {
    stop("Both Form ID and API key are required.")
  }

  creds <- c(ID = form_id, Key = resolved_api)
  .formior_state$Form_Info <- creds
  creds
}
