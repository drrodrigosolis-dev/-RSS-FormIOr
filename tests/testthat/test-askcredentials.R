test_that("AskCredentials uses provided values and caches credentials", {
	ns <- asNamespace("FormIOr")
	state <- get(".formior_state", envir = ns)
	prev <- state$Form_Info
	on.exit({
		state$Form_Info <- prev
	}, add = TRUE)

	creds <- AskCredentials(form_id = "FORM_123", api = "API_456")

	expect_equal(creds, c(ID = "FORM_123", Key = "API_456"))
	expect_equal(state$Form_Info, c(ID = "FORM_123", Key = "API_456"))
})


test_that("AskCredentials rejects conflicting api aliases", {
	expect_error(
		AskCredentials(form_id = "FORM_123", api = "API_456", api_key = "DIFFERENT"),
		"do not match"
	)
})


test_that("AskCredentials errors in non-interactive sessions when required values are missing", {
	testthat::skip_if(interactive())
	expect_error(AskCredentials(form_id = "FORM_123"), "non-interactive")
	expect_error(AskCredentials(api_key = "API_456"), "non-interactive")
})


test_that("AskCredentials prompts for missing values in interactive sessions", {
	testthat::skip_if_not(interactive())

	ns <- asNamespace("FormIOr")
	state <- get(".formior_state", envir = ns)
	prev <- state$Form_Info
	on.exit({
		state$Form_Info <- prev
	}, add = TRUE)

	answers <- c("FORM_FROM_PROMPT", "API_FROM_PROMPT")
	i <- 0

	testthat::local_mocked_bindings(
		readline = function(prompt = "") {
			i <<- i + 1
			answers[[i]]
		},
		.package = "base"
	)

	creds <- AskCredentials()
	expect_equal(creds, c(ID = "FORM_FROM_PROMPT", Key = "API_FROM_PROMPT"))
})
