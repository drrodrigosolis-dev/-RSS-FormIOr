

test_that("SummaryByField summarizes numeric fields", {
	df <- data.frame(score = c(1, 2, 3, NA))

	res <- SummaryByField(df, "score", quiet = TRUE)
	metrics <- res$summary$metric

	expect_equal(res$type, "numeric")
	expect_equal(res$missing, 1)
	expect_true("mean" %in% metrics)
})


test_that("SummaryByField summarizes categorical fields", {
	df <- data.frame(fruit = c("Apple", "Apple", "Banana", NA))

	res <- SummaryByField(df, "fruit", include_na = TRUE, quiet = TRUE)

	expect_equal(res$type, "categorical")
	expect_equal(res$missing, 1)
	expect_equal(res$summary$count[res$summary$value == "Apple"], 2)
})


test_that("CrossTab builds counts and percents", {
	df <- data.frame(
		region = c("North", "North", "South"),
		program = c("A", "B", "A"),
		stringsAsFactors = FALSE
	)

	res <- CrossTab(df, "region", "program", percent = "overall", quiet = TRUE)

	expect_equal(res$row, "region")
	expect_equal(res$col, "program")
	expect_true("count" %in% names(res$long))
	expect_true("percent" %in% names(res$long))
	expect_equal(res$table[res$table$region == "North", "A"], 1)
})


test_that("ResponseTimeline counts by day and fills empty dates", {
	df <- data.frame(
		created = c("2024-01-01", "2024-01-01", "2024-01-03"),
		stringsAsFactors = FALSE
	)

	res <- ResponseTimeline(df, date_col = "created", interval = "day", include_empty = TRUE, quiet = TRUE)

	# Tight range: do not extend beyond first/last submission period by default.
	expect_equal(min(res$data$period), as.Date("2024-01-01"))
	expect_equal(max(res$data$period), as.Date("2024-01-03"))

	expect_true(as.Date("2024-01-02") %in% res$data$period)
	expect_equal(res$data$count[res$data$period == as.Date("2024-01-02")], 0)
})


test_that("ResponseTimeline respects explicit start/end bounds and keeps edge zeroes", {
	df <- data.frame(
		created = c("2024-01-01", "2024-01-03"),
		stringsAsFactors = FALSE
	)

	res <- ResponseTimeline(
		df,
		date_col = "created",
		interval = "day",
		start = "2023-12-30",
		end = "2024-01-05",
		include_empty = TRUE,
		quiet = TRUE
	)

	expect_equal(min(res$data$period), as.Date("2023-12-30"))
	expect_equal(max(res$data$period), as.Date("2024-01-05"))

	expect_equal(res$data$count[res$data$period == as.Date("2023-12-30")], 0)
	expect_equal(res$data$count[res$data$period == as.Date("2024-01-01")], 1)
	expect_equal(res$data$count[res$data$period == as.Date("2024-01-05")], 0)
})


test_that("ResponseTimeline buckets by hour and fills empty hours", {
	df <- data.frame(
		created = c("2024-01-01 10:15:00", "2024-01-01 10:45:00", "2024-01-01 12:00:00"),
		stringsAsFactors = FALSE
	)

	res <- ResponseTimeline(df, date_col = "created", interval = "hour", tz = "UTC", include_empty = TRUE, quiet = TRUE)

	expect_true(inherits(res$data$period, "POSIXt"))

	t10 <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC")
	t11 <- as.POSIXct("2024-01-01 11:00:00", tz = "UTC")
	t12 <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")

	expect_true(all(c(t10, t11, t12) %in% res$data$period))
	expect_equal(res$data$count[res$data$period == t10], 2)
	expect_equal(res$data$count[res$data$period == t11], 0)
	expect_equal(res$data$count[res$data$period == t12], 1)
})


test_that("ResponseTimeline works on FoodTypes (after ResolveRepeats) and counts submissions", {
	raw <- foodtypes_raw()
	resolved <- foodtypes_resolved()

	tl <- ResponseTimeline(
		resolved,
		date_col = "form-created_at",
		interval = "hour",
		tz = "UTC",
		include_empty = TRUE,
		quiet = TRUE
	)

	expect_true(is.list(tl))
	expect_true(is.data.frame(tl$data))
	expect_true(inherits(tl$data$period, "POSIXt"))
	expect_equal(sum(tl$data$count), nrow(raw))
})
