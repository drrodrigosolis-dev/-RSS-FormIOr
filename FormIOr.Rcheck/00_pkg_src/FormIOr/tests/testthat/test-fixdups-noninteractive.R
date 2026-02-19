
test_that("FixDups can run non-interactively with default strategy", {
	df <- data.frame(
		submissionId = c("s1", "s1", "s2"),
		q1 = c("A", "B", "C"),
		other = c(1, 1, 2),
		stringsAsFactors = FALSE
	)

	flat <- list(
		FlatResponses = df,
		ColumnNames = data.frame(Number = seq_along(names(df)), Name = names(df), stringsAsFactors = FALSE)
	)

	out <- FixDups(flat, id_col = "submissionId", prompt = FALSE, default_strategy = "concat_comma", quiet = TRUE)

	expect_true(is.list(out))
	expect_equal(nrow(out$cleaned), 2)
	expect_true(any(out$decisions$column == "q1"))
})


test_that("FixDups can apply an explicit strategy map without prompting", {
	df <- data.frame(
		submissionId = c("s1", "s1", "s2"),
		q1 = c("A", "B", "C"),
		other = c(1, 1, 2),
		stringsAsFactors = FALSE
	)

	flat <- list(
		FlatResponses = df,
		ColumnNames = data.frame(Number = seq_along(names(df)), Name = names(df), stringsAsFactors = FALSE)
	)

	strategies <- data.frame(column = "q1", strategy = "first", stringsAsFactors = FALSE)
	out <- FixDups(flat, id_col = "submissionId", prompt = FALSE, strategies = strategies, quiet = TRUE)

	expect_true(is.list(out))
	expect_equal(nrow(out$cleaned), 2)
	expect_equal(out$cleaned$q1[out$cleaned$submissionId == "s1"], "A")
})

