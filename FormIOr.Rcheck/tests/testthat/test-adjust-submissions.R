
test_that("AdjustSubmissions deletes rows by ID and applies updates", {
	df <- data.frame(
		submissionId = c("a", "b", "c"),
		age = c(10, 11, 12),
		status = c("ok", "test", "ok"),
		stringsAsFactors = FALSE
	)

	out <- AdjustSubmissions(
		df,
		id_col = "submissionId",
		delete_ids = "b",
		updates = data.frame(id = "c", column = "status", value = "review", stringsAsFactors = FALSE),
		quiet = TRUE
	)

	expect_equal(nrow(out$data), 2)
	expect_false("b" %in% out$data$submissionId)
	expect_equal(out$data$status[out$data$submissionId == "c"], "review")
	expect_true(all(c("summary", "changes") %in% names(out)))
})


test_that("AdjustSubmissions coerces values to numeric columns when possible", {
	df <- data.frame(
		submissionId = c("a", "b"),
		score = c(1, 2),
		stringsAsFactors = FALSE
	)

	out <- AdjustSubmissions(
		df,
		id_col = "submissionId",
		updates = data.frame(id = "a", column = "score", value = "42", stringsAsFactors = FALSE),
		quiet = TRUE
	)

	expect_equal(out$data$score[out$data$submissionId == "a"], 42)
})


test_that("AdjustSubmissions can update a FlattenSubmissions-style object", {
	flat <- list(
		FlatResponses = data.frame(submissionId = c("a", "b"), status = c("ok", "ok"), stringsAsFactors = FALSE),
		ColumnNames = data.frame(Number = 1:2, Name = c("submissionId", "status"), stringsAsFactors = FALSE)
	)

	out <- AdjustSubmissions(
		flat,
		id_col = "submissionId",
		updates = data.frame(id = "b", column = "status", value = "review", stringsAsFactors = FALSE),
		return_flat = TRUE,
		quiet = TRUE
	)

	expect_true(is.list(out$flat))
	expect_equal(out$flat$FlatResponses$status[out$flat$FlatResponses$submissionId == "b"], "review")
})

