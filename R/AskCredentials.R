#' Ask Credentials
#'
#' Function to get the credentials that will be used to access the Form's API. Generally this function will be called from within GetResponses()
#' @returns Vector of 2
#' @export
#'
#' @examples
#' AskCredentials()
#'
AskCredentials <- function() {
  cat("\014")
  cat("-> You can find your Form's ID by going to your Form Settings page, and copying it from the URL (after the '=' sign in the URL)")
  cat('\n')
  cat(inverse("Please enter your Form's ID: "))
  ID <- readline()
  cat('-> You can generate an API for the specific form from the "Manage Form" page')
  cat('\n')
  cat(inverse("Please enter your Form's API: "))

  API <- readline()

  Form_Info <- c(ID = ID, Key = API)
  .Form_Info <<- Form_Info
  return(Form_Info)
}
