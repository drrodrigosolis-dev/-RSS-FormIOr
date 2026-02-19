#' Ask for Form credentials
#'
#' Collects the Form ID and API key in an interactive session and caches them
#' for this R session.
#'
#' @returns Named character vector with `ID` and `Key`.
#' @export
#'
#' @examples
#' \dontrun{
#' AskCredentials()
#' }
AskCredentials <- function() {
  if (!interactive()) {
    stop("AskCredentials() is interactive. In non-interactive sessions, pass form_id and api_key explicitly.")
  }

  message("You can find your Form ID on the form settings page URL (after '=').")
  id <- trimws(readline(prompt = "Form ID: "))

  message("You can generate an API key from the form 'Manage Form' page.")
  api <- trimws(readline(prompt = "Form API key: "))

  if (!nzchar(id) || !nzchar(api)) {
    stop("Both Form ID and API key are required.")
  }

  creds <- c(ID = id, Key = api)
  .formior_state$Form_Info <- creds
  creds
}
