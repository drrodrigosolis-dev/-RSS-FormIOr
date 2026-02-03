

#' Title
#' @param base_url URL where the form is stored. for the Gov't of BC, the URL is "https://submit.digital.gov.bc.ca/app/api/v1"(default)
#' @param form_id if left  (Default), and credentials have not been entered, it will run AskCredentials() to obtain the information. You can find your Form's ID by going to your Form Settings page, and copying it from the URL (after the "=" sign in the URL)
#' @param api_key if left  (Default), and credentials have not been entered, it will run AskCredentials() to obtain the information. You can generate an API for the specific form from the "Manage Form" page
#' @param reenter.credentials Default as FALSE. if TRUE, it will ask you to reenter the Form ID and API KEY, regardless if they already exist in the system
#'
#' @details
#' #' @details
#' The function returns a hierarchical named list representing the full survey
#' form configuration. Top-level elements include scalar metadata fields such as
#' the form name, description, identifiers, and feature flags. Nested sublists
#' encode structured configuration blocks (e.g., scheduling rules and event
#' stream configuration), and selected components (e.g., versions and identity
#' providers) are returned as data frames. The structure closely mirrors the
#' original JSON response from the API while using native R data types.

#' @returns
#' A named, hierarchical list representing the configuration and metadata of a
#' survey form. The list contains a mix of scalar elements (character, logical,
#' and numeric), nested sublists, and embedded data frames. Top-level elements
#' include form metadata (e.g., name, description, identifiers, deployment
#' settings), feature flags, and ownership information. Nested components
#' describe structured configuration domains such as scheduling rules
#' (`schedule`), external metadata (`formMetadata`), and event streaming
#' configuration (`eventStreamConfig`). Version history and identity provider
#' information are returned as data frames within the list. The structure
#' mirrors the hierarchical JSON schema returned by the underlying API while
#' using native R data types for programmatic access.

#' @export
#'
#' @examples
#' #'#' \dontrun{ GetFormMetadata(form_id = form_id, api_key = api_key)}
#'
#'
GetFormMetadata<-function(base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
												 form_id = NULL, api_key = NULL,
												  reenter.credentials = F) {

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

	versionRelation<-fromJSON(content(as='text',GET(
		url = paste0(base_url, "/forms/", form_id),
		query = list(formID=form_id), authenticate(user = form_id, password = api_key, type = "basic"), add_headers(Accept = "application/json"),timeout(60)  )))

	return(invisible(versionRelation))

	}
