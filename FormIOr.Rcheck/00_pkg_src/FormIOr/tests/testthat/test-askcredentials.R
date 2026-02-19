
test_that("AskCredentials returns and caches credentials using mocked input", {
	if (!interactive()) {
		expect_error(AskCredentials(), "interactive")
		return()
	}

	ns <- asNamespace("FormIOr")
	state <- get(".formior_state", envir = ns)
	prev <- state$Form_Info
	on.exit({
		state$Form_Info <- prev
	}, add = TRUE)

	answers <- c("FORM_123", "API_456")
	i <- 0

	testthat::local_mocked_bindings(
		readline = function(prompt = "") {
			i <<- i + 1
			answers[[i]]
		},
		.package = "base"
	)

	creds <- NULL
	utils::capture.output({
		creds <- AskCredentials()
	})

	expect_equal(creds, c(ID = "FORM_123", Key = "API_456"))
	expect_equal(state$Form_Info, c(ID = "FORM_123", Key = "API_456"))
})
