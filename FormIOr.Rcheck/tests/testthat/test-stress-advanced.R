test_that("MakeCodebook handles list columns by converting to text", {
	flat <- edge_flat()
	cb <- MakeCodebook(flat$FlatResponses, quiet = TRUE)

	expect_true(is.data.frame(cb))
	expect_true("repeatGrid-tags" %in% cb$name)
	expect_equal(cb$class[cb$name == "repeatGrid-tags"], "character")
})


test_that("RenameCols applies a partial rename_map and keeps other names", {
	flat <- edge_flat()
	map <- data.frame(
		OldNames = c("region", "status"),
		NewNames = c("RegionName", "StatusName"),
		stringsAsFactors = FALSE
	)

	out <- RenameCols(flat, rename_map = map, quiet = TRUE)

	expect_true(is.list(out$flat))
	expect_true(all(c("RegionName", "StatusName") %in% names(out$flat$FlatResponses)))
	expect_true("region" %in% out$renamedDF$OldNames)
	expect_true("status" %in% out$renamedDF$OldNames)
})


test_that("ExportToExcel sanitizes long/illegal sheet names for delimited output", {
	df1 <- data.frame(a = 1)
	df2 <- data.frame(a = 2)
	sheets <- structure(
		list(df1, df2),
		names = c("Very/Long*Name:With?Illegal[Chars]", "Very/Long*Name:With?Illegal[Chars]")
	)

	out_path <- tempfile(fileext = ".csv")
	out <- ExportToExcel(sheets, path = out_path, overwrite = TRUE, quiet = TRUE)

	expect_equal(out$format, "csv")
	expect_equal(length(out$sheets), 2)
	expect_true(length(unique(out$sheets)) == 2)
	expect_true(all(nchar(out$sheets) <= 31))
	expect_true(all(!grepl("[\\[\\]\\*\\?/\\\\:]", out$sheets)))
	expect_true(all(file.exists(out$path)))
})


test_that("ExportToExcel errors when output exists and overwrite is FALSE", {
	df <- data.frame(a = 1)
	out_path <- tempfile(fileext = ".csv")
	ExportToExcel(df, path = out_path, overwrite = TRUE, quiet = TRUE)

	expect_error(ExportToExcel(df, path = out_path, overwrite = FALSE, quiet = TRUE), "already exist")
})


test_that("AdjustSubmissions updates list columns and preserves list type", {
	df <- data.frame(submissionId = c("s1", "s2"), stringsAsFactors = FALSE)
	df$notes <- I(list(list(a = 1), list(a = 2)))

	out <- AdjustSubmissions(
		df,
		id_col = "submissionId",
		updates = list(list(id = "s1", column = "notes", value = list(a = 99))),
		quiet = TRUE
	)

	expect_true(is.list(out$data$notes))
	expect_equal(out$data$notes[[1]], 99)
	expect_equal(out$data$notes[[2]]$a, 2)
})


test_that("ReviewDuplicateSubmissions supports dropping an entire group", {
	df <- data.frame(
		submissionId = c("a1", "a2", "b1", "b2"),
		email = c("a@example.com", "a@example.com", "b@example.com", "b@example.com"),
		status = c("x", "y", "x", "y"),
		stringsAsFactors = FALSE
	)

	out <- ReviewDuplicateSubmissions(
		df,
		id_col = "submissionId",
		key_cols = "email",
		compare_cols = c("submissionId", "email"),
		keep_map = list("1" = "none"),
		default_keep = "all",
		prompt = FALSE,
		quiet = TRUE
	)

	expect_false(any(out$data$email == "a@example.com"))
	expect_true(all(out$data$email == "b@example.com"))
	expect_true(any(out$decisions$decision == "dropped_all"))
})
