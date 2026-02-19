test_that("standardize_column_names handles duplicates, blanks, and transliteration", {
	df <- data.frame(matrix(1:8, nrow = 2))
	names(df) <- c("Café", "Cafe", "Cafe", " ")

	res <- standardize_column_names(df, make_unique = TRUE, transliterate = TRUE, quiet = TRUE)
	new_names <- names(res$data)

	expect_equal(length(new_names), 4)
	expect_true(length(unique(new_names)) == 4)
	expect_true(all(grepl("^[A-Za-z0-9_]+$", new_names)))
	expect_true(any(new_names == "col"))
})


test_that("apply_submission_updates records invalid updates without applying them", {
	df <- data.frame(
		submissionId = c("s1", "s2"),
		score = c(10, 20),
		stringsAsFactors = FALSE
	)

	updates <- data.frame(
		id = c("missing", "s1"),
		column = c("score", "bad_col"),
		value = c("99", "123"),
		stringsAsFactors = FALSE
	)

	out <- apply_submission_updates(df, id_col = "submissionId", updates = updates, quiet = TRUE)

	expect_equal(out$summary$value[out$summary$metric == "updates_applied"], 0)
	expect_true(any(out$changes$status == "id_not_found"))
	expect_true(any(out$changes$status == "column_not_found"))
	expect_equal(out$data$score, df$score)
})


test_that("collapse_repeated_values supports explicit strategies", {
	df <- data.frame(
		submission_id = c("a", "a", "b", "b"),
		num = c(1, 2, 10, 20),
		text = c("x", "y", "z", "z"),
		flag = c("Yes", "No", "Yes", "Yes"),
		stringsAsFactors = FALSE
	)

	res_sum <- collapse_repeated_values(df, id_col = "submission_id", strategy = "sum", quiet = TRUE)
	expect_equal(res_sum$data$num[res_sum$data$submission_id == "a"], 3)
	expect_equal(res_sum$data$num[res_sum$data$submission_id == "b"], 30)

	res_first <- collapse_repeated_values(df, id_col = "submission_id", strategy = "first", quiet = TRUE)
	expect_equal(res_first$data$text[res_first$data$submission_id == "a"], "x")

	res_last <- collapse_repeated_values(df, id_col = "submission_id", strategy = "last", quiet = TRUE)
	expect_equal(res_last$data$text[res_last$data$submission_id == "a"], "y")

	res_concat <- collapse_repeated_values(df, id_col = "submission_id", strategy = "concat", quiet = TRUE)
	expect_true(grepl("x", res_concat$data$text[res_concat$data$submission_id == "a"]))
	expect_true(grepl("y", res_concat$data$text[res_concat$data$submission_id == "a"]))

	res_count <- collapse_repeated_values(df, id_col = "submission_id", strategy = "count", quiet = TRUE)
	expect_equal(res_count$data$text[res_count$data$submission_id == "a"], 2)

	res_count_yes <- collapse_repeated_values(df, id_col = "submission_id", strategy = "count_yes", quiet = TRUE)
	expect_equal(res_count_yes$data$flag[res_count_yes$data$submission_id == "a"], 1)
	expect_equal(res_count_yes$data$flag[res_count_yes$data$submission_id == "b"], 2)
})


test_that("deduplicate_submission_rows keeps first row when time_col is invalid", {
	df <- data.frame(
		submission_id = c("a", "a", "b"),
		created = c("2026-01-01", "2026-01-02", "2026-01-03"),
		value = c(1, 2, 3),
		stringsAsFactors = FALSE
	)

	out <- deduplicate_submission_rows(df, id_col = "submission_id", time_col = "missing", keep = "first", quiet = TRUE)
	expect_equal(out$data$value[out$data$submission_id == "a"], 1)
})


test_that("collapse_checkbox_selections keeps empty selections when requested and can retain originals", {
	df <- data.frame(
		`group-A` = c(FALSE, TRUE),
		`group-B` = c(FALSE, FALSE),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	out <- collapse_checkbox_selections(df, sep = "-", keep_empty = TRUE, drop = FALSE, quiet = TRUE)

	expect_true("group-selected" %in% names(out$data))
	expect_equal(out$data[["group-selected"]][1], "")
	expect_equal(out$data[["group-selected"]][2], "A")
	expect_true(all(c("group-A", "group-B") %in% names(out$data)))
})


test_that("summarize_response_timeline errors on missing date column and on empty filtered range", {
	df <- stress_flat()

	expect_error(summarize_response_timeline(df, date_col = "nope", quiet = TRUE), "not found")

	expect_error(
		summarize_response_timeline(
			df,
			date_col = "created_at",
			start = "2030-01-01",
			end = "2030-01-02",
			quiet = TRUE
		),
		"No dates remain"
	)
})


test_that("plot_numeric_distribution handles numeric-like factors", {
	df <- data.frame(score = factor(c("1", "2", "3")), stringsAsFactors = FALSE)
	out <- testthat::expect_invisible(plot_numeric_distribution(df, "score", plot = FALSE))

	expect_true(is.list(out))
	expect_equal(out$field, "score")
	expect_true(is.list(out$hist))
})


test_that("plot_categorical_summary can include missing values", {
	df <- data.frame(region = c("North", NA, "South"), stringsAsFactors = FALSE)
	out <- testthat::expect_invisible(plot_categorical_summary(df, "region", include_na = TRUE, plot = FALSE))

	expect_true("(Missing)" %in% out$data$value)
})


test_that("tabulate_field_by_group percent modes are consistent", {
	df <- data.frame(
		row = c("A", "A", "B", "B"),
		col = c("X", "Y", "X", "Y"),
		stringsAsFactors = FALSE
	)

	out_row <- tabulate_field_by_group(df, "row", "col", percent = "row", quiet = TRUE)
	row_pct <- out_row$long$percent
	row_groups <- out_row$long$row
	expect_equal(as.numeric(tapply(row_pct, row_groups, sum)), c(100, 100))

	out_col <- tabulate_field_by_group(df, "row", "col", percent = "col", quiet = TRUE)
	col_pct <- out_col$long$percent
	col_groups <- out_col$long$col
	expect_equal(as.numeric(tapply(col_pct, col_groups, sum)), c(100, 100))
})


test_that("summarize_field_distribution detects date fields and reports min/max", {
	df <- data.frame(
		created_at = as.POSIXct(
			c("2026-02-01 10:00:00", "2026-02-01 12:00:00"),
			tz = "UTC"
		)
	)
	res <- summarize_field_distribution(df, "created_at", quiet = TRUE)

	expect_equal(res$type, "date")
	expect_true(all(c("min", "max") %in% res$summary$metric))
})
