
#' Get Responses submitted to a FormIO Form
#'
#' @param base_url URL where the form is stored. for the Gov't of BC, the URL is "https://submit.digital.gov.bc.ca/app/api/v1"(default)
#' @param form_id if left  (Default), and credentials have not been entered, it will run AskCredentials() to obtain the information. You can find your Form's ID by going to your Form Settings page, and copying it from the URL (after the "=" sign in the URL)
#' @param api_key if left  (Default), and credentials have not been entered, it will run AskCredentials() to obtain the information. You can generate an API for the specific form from the "Manage Form" page
#' @param drafts Default FALSE, set to TRUE to download forms in draft
#' @param deleted Default FALSE, set to TRUE to download deleted forms
#' @param content.only Default TRUE. if F, returns a list with the submission data, content type, content disposition, and API status. If FALSE, it will only return the submission data as a data frame.
#' @param reenter.credentials Default as FALSE. if TRUE, it will ask you to reenter the Form ID and API KEY, regardless if they already exist in the system
#'
#' @returns if content.only= FALSE, returns a list with the submission data, content type, content disposition, and API status. IF False, it will only return the submission data as a data frame.
#' @export
#'
#' @examples
#'#' \dontrun{ GetResponses(form_id = form_id, api_key = api_key, content.only = F)}
#'

GetResponses <- function(base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
                         form_id= NULL,
                         api_key= NULL,
                         drafts = F,
                         deleted = F,
                         content.only = T,
                         reenter.credentials= F) {

  if(reenter.credentials== T){
    Form_Info <- c()
    Form_Info<- AskCredentials()}
  if(exists(".Form_Info")) {Form_Info <-  .Form_Info}
  if(!exists(".Form_Info")){
   Form_Info <- c()
    Form_Info<- AskCredentials()}

  .Form_Info<<-Form_Info


  form_id<-   ifelse(is.null(form_id), Form_Info[1], form_id)
  api_key<-   ifelse(is.null(api_key), Form_Info[2], api_key)

  export_url <- paste0(base_url, "/forms/", form_id, "/export")

  query_params <- list(
    format = "json",
    type = "submissions",
    drafts = ifelse(drafts == F, "false", "true"),
    deleted = ifelse(deleted == F, "false", "true")
  )

  # ---- Try request with x-token header (preferred) ----
  resp <- GET(
    url = export_url,
    query = query_params,
    authenticate(user = form_id, password = api_key, type = "basic"),
    add_headers("Accept" = "application/json"),
    timeout(60)
  )

  submission_data <- fromJSON(content(resp, as = "text"))

  full_submission_data <- list(
    status = status_code(resp),
    content_type = headers(resp)[["content-type"]],
    content_disposition = headers(resp)[["content-disposition"]],
    submission_data = submission_data  )

  # ---- Return object to R environment ----
  # (submissions_data will be either the parsed JSON list/data.frame or NULL if a file was saved)
  if (content.only == F) {return(full_submission_data)}
  if (content.only == T){return(submission_data)}
  if (content.only == 'raw'){return(content(resp, as = "text"))}
}
