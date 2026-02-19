test_that("plot_numeric_distribution returns histogram data without plotting", {
	df <- data.frame(age = c(10, 12, 14, NA), stringsAsFactors = FALSE)

	out <- testthat::expect_invisible(plot_numeric_distribution(df, "age", plot = FALSE))

	expect_true(is.list(out))
	expect_equal(out$field, "age")
	expect_true(is.list(out$hist))
	expect_true(is.numeric(out$hist$counts))
})


test_that("plot_numeric_distribution errors on non-numeric fields", {
	df <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)
	expect_error(plot_numeric_distribution(df, "x", plot = FALSE), "numeric")
})


test_that("plot_categorical_summary returns bar data without plotting", {
	df <- data.frame(color = c("red", "red", "blue"), stringsAsFactors = FALSE)

	out <- testthat::expect_invisible(plot_categorical_summary(df, "color", plot = FALSE))

	expect_true(is.list(out))
	expect_equal(out$field, "color")
	expect_true(is.data.frame(out$data))
	expect_true(all(c("value", "count") %in% names(out$data)))
	expect_equal(out$data$count[out$data$value == "red"], 2)
})


test_that("plot_categorical_summary errors on numeric fields", {
	df <- data.frame(score = c(1, 2, 3), stringsAsFactors = FALSE)
	expect_error(plot_categorical_summary(df, "score", plot = FALSE), "not categorical")
})


test_that("Plot helpers run on FoodTypes resolved data (plot = FALSE)", {
	raw <- foodtypes_raw()
	resolved <- foodtypes_resolved()

	hist_out <- testthat::expect_invisible(plot_numeric_distribution(resolved, "completion_pct", plot = FALSE))
	expect_true(is.list(hist_out))
	expect_equal(hist_out$field, "completion_pct")
	expect_true(is.list(hist_out$hist))

	bar_out <- testthat::expect_invisible(plot_categorical_summary(resolved, "region", plot = FALSE))
	expect_true(is.list(bar_out))
	expect_equal(bar_out$field, "region")
	expect_true(is.data.frame(bar_out$data))

	tl_out <- testthat::expect_invisible(plot_response_timeline(
		resolved,
		date_col = "form-created_at",
		interval = "hour",
		include_empty = TRUE,
		plot = FALSE
	))
	expect_true(is.list(tl_out))
	expect_true(is.data.frame(tl_out$data))
	expect_equal(sum(tl_out$data$count), nrow(raw))
})


test_that("plot_response_timeline supports start_date/end_date aliases", {
	df <- data.frame(
		created = c("2024-01-01", "2024-01-03", "2024-01-05"),
		stringsAsFactors = FALSE
	)

	out <- testthat::expect_invisible(plot_response_timeline(
		df,
		date_col = "created",
		interval = "day",
		start_date = "2024-01-02",
		end_date = "2024-01-04",
		include_empty = TRUE,
		plot = FALSE
	))

	expect_true(is.list(out))
	expect_true(is.data.frame(out$data))
	expect_true(all(as.Date(c("2024-01-02", "2024-01-03", "2024-01-04")) %in% out$data$period))
	expect_equal(out$data$count[out$data$period == as.Date("2024-01-03")], 1)
})


test_that("plot_response_timeline can draw a plot (headless)", {
	df <- data.frame(
		created = c("2024-01-01", "2024-01-03", "2024-01-05"),
		stringsAsFactors = FALSE
	)

	out_png <- tempfile(fileext = ".png")
	grDevices::png(out_png)
	on.exit(grDevices::dev.off(), add = TRUE)

	out <- testthat::expect_invisible(plot_response_timeline(
		df,
		date_col = "created",
		interval = "day",
		plot = TRUE
	))

	expect_true(is.list(out))
	expect_true(is.data.frame(out$data))
})


test_that("plot_word_cloud errors cleanly when 'wordcloud' is not installed", {
	if (requireNamespace("wordcloud", quietly = TRUE)) {
		skip("wordcloud is installed; skipping error-path test.")
	}

	df <- data.frame(feedback = c("hello world", "hello"), stringsAsFactors = FALSE)
	expect_error(plot_word_cloud(df, "feedback"), "wordcloud")
})


test_that("plot_word_cloud returns word frequencies when 'wordcloud' is installed", {
	skip_if_not_installed("wordcloud")

	df <- data.frame(feedback = c("apple banana", "apple"), stringsAsFactors = FALSE)

	# Ensure we have a graphics device in headless test runs.
	out_png <- tempfile(fileext = ".png")
	grDevices::png(out_png)
	on.exit(grDevices::dev.off(), add = TRUE)

	out <- testthat::expect_invisible(plot_word_cloud(
		df,
		field = "feedback",
		remove_stopwords = FALSE,
		seed = 1,
		max_words = 10
	))

	expect_true(is.list(out))
	expect_true(all(c("fields", "freq") %in% names(out)))
	expect_true("apple" %in% names(out$freq))
	expect_true(as.integer(out$freq[["apple"]]) >= 2)
})
