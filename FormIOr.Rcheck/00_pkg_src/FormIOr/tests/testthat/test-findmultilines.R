test_that("findMultilines counts distinct values per submission", {
	df <- data.frame(
		submissionId = c("a", "a", "b"),
		fruit = c("Apple", "Banana", "Apple"),
		qty = c(1, 1, 2),
		stringsAsFactors = FALSE
	)

	flat <- list(FlatResponses = df)

	out <- findMultilines(flat, id_col = 1)

	expect_true(is.data.frame(out))
	expect_equal(nrow(out), nrow(df))
	expect_equal(names(out), names(df))

	expect_equal(out$submissionId, df$submissionId)
	expect_equal(out$fruit[out$submissionId == "a"], c(2, 2))
	expect_equal(out$fruit[out$submissionId == "b"], 1)
	expect_equal(unique(out$qty), 1)
})


test_that("findMultilines flags repeated editGrid fields in FoodTypes", {
	flat <- foodtypes_flat()

	out <- findMultilines(flat, id_col = 1)

	expect_true(is.data.frame(out))
	expect_equal(dim(out), dim(flat$FlatResponses))
	expect_equal(names(out), names(flat$FlatResponses))

	# ID column should be preserved verbatim.
	expect_equal(out[[1]], flat$FlatResponses[[1]])

	# editGrid-ingredient has up to 3 distinct values within a submission in FoodTypes.
	expect_equal(max(out[["editGrid-ingredient"]], na.rm = TRUE), 3)

	# region is constant per submission, so its per-submission distinct count is 1 everywhere.
	expect_equal(unique(na.omit(out[["region"]])), 1)
})
