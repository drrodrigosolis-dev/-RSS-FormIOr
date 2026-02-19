test_that("FlattenSubmissions unnests nested data.frame columns with names_sep", {
	df <- data.frame(id = c("a", "b"), stringsAsFactors = FALSE)
	df$nested <- I(list(
		data.frame(x = 1, y = "u", stringsAsFactors = FALSE),
		data.frame(x = 2, y = "v", stringsAsFactors = FALSE)
	))

	out <- FlattenSubmissions(df)

	expect_true(is.list(out))
	expect_true(all(c("FlatResponses", "ColumnNames") %in% names(out)))
	expect_true(is.data.frame(out$FlatResponses))
	expect_equal(nrow(out$FlatResponses), 2)
	expect_true(all(c("id", "nested-x", "nested-y") %in% names(out$FlatResponses)))

	expect_true(is.data.frame(out$ColumnNames))
	expect_equal(out$ColumnNames$Name, names(out$FlatResponses))
})


test_that("FlattenSubmissions flattens the bundled FoodTypes dataset", {
	raw <- foodtypes_raw()
	out <- FlattenSubmissions(raw)

	expect_true(is.list(out))
	expect_true(all(c("FlatResponses", "ColumnNames") %in% names(out)))

	expect_true(is.data.frame(out$FlatResponses))
	expect_true(nrow(out$FlatResponses) >= nrow(raw))
	expect_true(ncol(out$FlatResponses) >= ncol(raw))

	expect_true(all(c("form-submission_id", "form-created_at", "editGrid-ingredient") %in% names(out$FlatResponses)))
	expect_equal(length(unique(out$FlatResponses[["form-submission_id"]])), nrow(raw))

	expect_true(is.data.frame(out$ColumnNames))
	expect_equal(nrow(out$ColumnNames), ncol(out$FlatResponses))
	expect_equal(out$ColumnNames$Name, names(out$FlatResponses))
})
