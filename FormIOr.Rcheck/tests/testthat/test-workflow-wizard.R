
test_that("FormIOrWorkflow runs in plan mode and exports files", {
	plan <- list(
		output_dir = tempdir(),
		audit = TRUE,
		download = FALSE,
		normalize_names = TRUE,
		codebook = TRUE,
		include_schema = FALSE,
		export = TRUE,
		export_path = tempfile(fileext = ".csv")
	)

	df <- data.frame(
		submissionId = c("s1", "s2"),
		age = c(10, 12),
		color = c("red", "blue"),
		stringsAsFactors = FALSE
	)

	out <- FormIOrWorkflow(data = df, plan = plan, quiet = TRUE)

	expect_true(is.list(out))
	expect_true(all(file.exists(out$files$export)))
	expect_true(file.exists(out$files$audit_log))
})
