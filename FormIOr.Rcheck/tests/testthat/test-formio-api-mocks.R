
test_that("GetResponses errors when credentials are unavailable in non-interactive mode", {
	ns <- asNamespace("FormIOr")
	state <- get(".formior_state", envir = ns)
	prev <- state$Form_Info
	on.exit({
		state$Form_Info <- prev
	}, add = TRUE)

	state$Form_Info <- NULL

	expect_error(GetResponses(form_id = NULL, api_key = NULL), "not available")
})


test_that("GetResponses returns parsed submissions and supports content.only modes", {
	ns <- asNamespace("FormIOr")
	state <- get(".formior_state", envir = ns)
	prev <- state$Form_Info
	on.exit({
		state$Form_Info <- prev
	}, add = TRUE)

	captured <- new.env(parent = emptyenv())

	mock_GET <- function(url, query = NULL, ...) {
		captured$url <- url
		captured$query <- query
		structure(list(url = url, query = query), class = "mock_resp")
	}
	mock_content <- function(resp, as = "text", encoding = NULL, ...) {
		# Simple JSON array -> data.frame via jsonlite::fromJSON().
		'[{"submissionId":"s1","value":1},{"submissionId":"s2","value":2}]'
	}
	mock_status_code <- function(resp) 200L
	mock_headers <- function(resp) list("content-type" = "application/json", "content-disposition" = "inline")

	testthat::local_mocked_bindings(
		GET = mock_GET,
		content = mock_content,
		status_code = mock_status_code,
		headers = mock_headers,
		authenticate = function(...) NULL,
		add_headers = function(...) NULL,
		timeout = function(...) NULL,
		.package = "FormIOr"
	)

	base_url <- "https://example.com/app/api/v1"

	parsed <- GetResponses(
		base_url = base_url,
		form_id = "FORM",
		api_key = "KEY",
		drafts = TRUE,
		deleted = FALSE,
		content.only = TRUE
	)

	expect_true(is.data.frame(parsed))
	expect_equal(nrow(parsed), 2)
	expect_true(grepl("/forms/FORM/export$", captured$url))
	expect_equal(captured$query$drafts, "true")
	expect_equal(captured$query$deleted, "false")

	full <- GetResponses(
		base_url = base_url,
		form_id = "FORM",
		api_key = "KEY",
		content.only = FALSE
	)
	expect_true(is.list(full))
	expect_equal(full$status, 200L)
	expect_true(inherits(full$submission_data, "data.frame"))

	raw <- GetResponses(
		base_url = base_url,
		form_id = "FORM",
		api_key = "KEY",
		content.only = "raw"
	)
	expect_true(is.character(raw))
	expect_true(grepl("^\\[\\{", raw))
})


test_that("GetSubmissions returns cleaned metadata with version mapping (mocked HTTP)", {
	ns <- asNamespace("FormIOr")
	state <- get(".formior_state", envir = ns)
	prev <- state$Form_Info
	on.exit({
		state$Form_Info <- prev
	}, add = TRUE)

	captured <- new.env(parent = emptyenv())

	mock_GET <- function(url, query = NULL, ...) {
		captured$url <- url
		captured$query <- query
		structure(list(url = url, query = query), class = "mock_resp")
	}
	mock_content <- function(resp, as = "text", encoding = NULL, ...) {
		# Minimal fields needed for the later select() call.
		'[\n  {\n    \"formId\": \"FORM\",\n    \"submissionId\": \"s1\",\n    \"createdBy\": \"u1\",\n    \"createdAt\": \"2024-01-01\",\n    \"formVersionId\": \"v1\",\n    \"confirmationId\": \"c1\",\n    \"formSubmissionStatusCode\": \"SUBMITTED\",\n    \"deleted\": false,\n    \"formSubmissionAssignedToUserId\": null,\n    \"formSubmissionAssignedToUsernameIdp\": null,\n    \"formSubmissionAssignedToEmail\": null,\n    \"lateEntry\": false\n  }\n]'
	}
	mock_status_code <- function(resp) 200L
	mock_headers <- function(resp) list("content-type" = "application/json", "content-disposition" = "inline")

	mock_meta <- function(...) {
		list(
			versions = data.frame(
				formId = "FORM",
				version = 3,
				id = "v1",
				published = TRUE,
				stringsAsFactors = FALSE
			)
		)
	}

	testthat::local_mocked_bindings(
		GET = mock_GET,
		content = mock_content,
		status_code = mock_status_code,
		headers = mock_headers,
		authenticate = function(...) NULL,
		add_headers = function(...) NULL,
		timeout = function(...) NULL,
		GetFormMetadata = mock_meta,
		.package = "FormIOr"
	)

	out <- GetSubmissions(
		base_url = "https://example.com/app/api/v1",
		form_id = "FORM",
		api_key = "KEY",
		content.only = TRUE,
		AdditionalCols = c("submissionId")
	)

	expect_true(is.data.frame(out))
	expect_true(grepl("/forms/FORM/submissions$", captured$url))
	expect_true(is.list(captured$query))
	expect_true("fields" %in% names(captured$query))
	expect_equal(out$submissionId, "s1")
	expect_equal(out$version, 3)
})


test_that("GetFormMetadata retries alternate base_url when JSON parsing fails (mocked HTTP)", {
	ns <- asNamespace("FormIOr")
	state <- get(".formior_state", envir = ns)
	prev <- state$Form_Info
	on.exit({
		state$Form_Info <- prev
	}, add = TRUE)

	calls <- list()

	mock_GET <- function(url, query = NULL, ...) {
		calls[[length(calls) + 1]] <<- url
		structure(list(url = url), class = "mock_resp")
	}
	mock_content <- function(resp, as = "text", encoding = NULL, ...) {
		if (grepl("/app/api/v1/", resp$url)) {
			return("not-json")
		}
		# A minimal metadata response with a versions table.
		'{\"title\":\"Example\",\"versions\":[{\"formId\":\"FORM\",\"version\":1,\"id\":\"v1\",\"published\":true}]}'
	}

	testthat::local_mocked_bindings(
		GET = mock_GET,
		content = mock_content,
		authenticate = function(...) NULL,
		add_headers = function(...) NULL,
		timeout = function(...) NULL,
		.package = "FormIOr"
	)

	meta <- GetFormMetadata(
		base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
		form_id = "FORM",
		api_key = "KEY"
	)

	expect_true(is.list(meta))
	expect_equal(meta$title, "Example")
	expect_true(length(calls) >= 2)
	expect_true(any(grepl("/app/api/v1/forms/FORM$", calls)))
	expect_true(any(grepl("/api/v1/forms/FORM$", calls)))
})


test_that("GetFormMetadata errors when both base URLs return invalid JSON (mocked HTTP)", {
	calls <- list()

	mock_GET <- function(url, query = NULL, ...) {
		calls[[length(calls) + 1]] <<- url
		structure(list(url = url), class = "mock_resp")
	}
	mock_content <- function(resp, as = "text", encoding = NULL, ...) {
		"not-json"
	}

	testthat::local_mocked_bindings(
		GET = mock_GET,
		content = mock_content,
		authenticate = function(...) NULL,
		add_headers = function(...) NULL,
		timeout = function(...) NULL,
		.package = "FormIOr"
	)

	expect_error(
		GetFormMetadata(
			base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
			form_id = "FORM",
			api_key = "KEY"
		),
		"Unable to parse form metadata"
	)

	expect_true(length(calls) >= 2)
	expect_true(any(grepl("/app/api/v1/forms/FORM$", calls)))
	expect_true(any(grepl("/api/v1/forms/FORM$", calls)))
})
