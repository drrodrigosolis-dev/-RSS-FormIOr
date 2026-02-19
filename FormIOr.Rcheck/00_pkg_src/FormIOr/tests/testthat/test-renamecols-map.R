
test_that("RenameCols can apply a rename map without prompting", {
	df <- data.frame(
		submissionId = c("s1", "s2"),
		age = c(10, 12),
		stringsAsFactors = FALSE
	)

	flat <- list(
		FlatResponses = df,
		ColumnNames = data.frame(Number = seq_along(names(df)), Name = names(df), stringsAsFactors = FALSE)
	)

	map_df <- data.frame(
		OldNames = c("submissionId", "age"),
		NewNames = c("submission_id", "age_years"),
		stringsAsFactors = FALSE
	)

	out <- RenameCols(flat, rename_map = map_df, quiet = TRUE)

	expect_true(is.list(out))
	expect_true(all(c("renamedDF", "flat") %in% names(out)))
	expect_true(all(c("submission_id", "age_years") %in% names(out$flat$FlatResponses)))
})

