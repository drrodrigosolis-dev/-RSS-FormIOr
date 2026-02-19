

test_that("NormalizeColumnNames standardizes names", {
	df <- data.frame(
		`First Name` = c("Ana", "Ben"),
		`Age (Years)` = c(30, 25),
		check.names = FALSE
	)

	res <- NormalizeColumnNames(df)
	expect_equal(names(res$data), c("first_name", "age_years"))
	expect_equal(nrow(res$name_map), 2)
})


test_that("ResolveRepeats collapses rows by submission id", {
	df <- data.frame(
		submissionId = c("a", "a", "b"),
		fruit = c("Apple", "Banana", "Apple"),
		qty = c(1, 2, 1),
		check1 = c(TRUE, FALSE, TRUE),
		stringsAsFactors = FALSE
	)

res <- ResolveRepeats(df, id_col = "submissionId")
expect_equal(nrow(res$data), 2)
expect_true(grepl("Apple", res$data$fruit[res$data$submissionId == "a"]))
expect_equal(res$data$qty[res$data$submissionId == "a"], 3)
expect_equal(res$data$check1[res$data$submissionId == "a"], 1)
})


test_that("ResolveRepeats handles groups that are all NA", {
	df <- data.frame(
		submissionId = c("a", "a", "b"),
		comment = c(NA, NA, "ok"),
		stringsAsFactors = FALSE
	)

	res <- ResolveRepeats(df, id_col = "submissionId")
	expect_equal(nrow(res$data), 2)
	expect_true(is.na(res$data$comment[res$data$submissionId == "a"]))
	expect_equal(res$data$comment[res$data$submissionId == "b"], "ok")
})


test_that("ResolveRepeats handles list columns by converting to text", {
	df <- data.frame(
		submissionId = c("a", "a", "b"),
		stringsAsFactors = FALSE
	)
	df$payload <- I(list(list(x = 1), list(x = 1), list(x = 2)))

	res <- ResolveRepeats(df, id_col = "submissionId")
	expect_equal(nrow(res$data), 2)
	expect_true(is.character(res$data$payload))
})

test_that("ResolveRepeats handles data.frame columns by converting to text", {
	df <- data.frame(
		submissionId = c("a", "a", "b"),
		stringsAsFactors = FALSE
	)
	df$nested <- tibble::tibble(x = c(1, 1, 2), y = c("u", "u", "v"))

	expect_warning(res <- ResolveRepeats(df, id_col = "submissionId"), NA)
	expect_equal(nrow(res$data), 2)
	expect_true(is.character(res$data$nested))
})


test_that("DeduplicateSubmissions keeps last by time column", {
	df <- data.frame(
		submissionId = c("a", "a", "b"),
		created = c("2020-01-01", "2020-01-02", "2020-01-03"),
		value = c(1, 2, 3),
		stringsAsFactors = FALSE
	)

	res <- DeduplicateSubmissions(df, id_col = "submissionId", keep = "last")
	expect_equal(nrow(res$data), 2)
	expect_equal(res$data$value[res$data$submissionId == "a"], 2)
})


test_that("CompactSelections merges checkbox columns", {
	df <- data.frame(
		`food-apple` = c(TRUE, FALSE),
		`food-banana` = c(FALSE, TRUE),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	res <- CompactSelections(df, sep = "-")
	expect_true("food-selected" %in% names(res$data))
	expect_equal(res$data[["food-selected"]], c("apple", "banana"))
})

test_that("CompactSelections tolerates blank/NA checkbox columns", {
	df <- data.frame(
		`Funding_Phase-NotSure` = c("", NA, "No"),
		`Funding_Phase-phase_I` = c("Yes", "", "No"),
		`Funding_Phase-phase_II` = c("", "", "Yes"),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	res <- CompactSelections(df, sep = "-")
	expect_true("Funding_Phase-selected" %in% names(res$data))
	expect_equal(res$data[["Funding_Phase-selected"]], c("phase_I", NA, "phase_II"))
})

test_that("CompactSelections works with logical columns", {
	df <- data.frame(
		`Funding_Phase-NotSure` = c(TRUE, FALSE, NA),
		`Funding_Phase-phase_I` = c(FALSE, TRUE, NA),
		`Funding_Phase-phase_II` = c(FALSE, TRUE, NA),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	res <- CompactSelections(df, sep = "-")
	expect_true("Funding_Phase-selected" %in% names(res$data))
	expect_equal(res$data[["Funding_Phase-selected"]], c("NotSure", "phase_I, phase_II", NA))
})


test_that("Cleaning helpers return updated $flat when return_flat = TRUE", {
	flat_norm <- list(
		FlatResponses = data.frame(
			`Submission ID` = c("a", "a", "b"),
			`First Name` = c("Ana", "Ana", "Ben"),
			stringsAsFactors = FALSE,
			check.names = FALSE
		),
		ColumnNames = data.frame(
			Number = 1:2,
			Name = c("Submission ID", "First Name"),
			stringsAsFactors = FALSE
		)
	)

	norm <- NormalizeColumnNames(flat_norm, return_flat = TRUE, quiet = TRUE)
	expect_true(is.data.frame(norm$data))
	expect_true(is.list(norm$flat))
	expect_equal(names(norm$data), c("submission_id", "first_name"))
	expect_equal(norm$data, norm$flat$FlatResponses)
	expect_equal(norm$flat$ColumnNames$Name, names(norm$data))

	flat_resolve <- list(
		FlatResponses = data.frame(
			submission_id = c("a", "a", "b"),
			fruit = c("Apple", "Banana", "Apple"),
			qty = c(1, 2, 1),
			stringsAsFactors = FALSE
		),
		ColumnNames = data.frame(
			Number = 1:3,
			Name = c("submission_id", "fruit", "qty"),
			stringsAsFactors = FALSE
		)
	)

	resolved <- ResolveRepeats(flat_resolve, id_col = "submission_id", return_flat = TRUE, quiet = TRUE)
	expect_true(is.data.frame(resolved$data))
	expect_true(is.list(resolved$flat))
	expect_equal(nrow(resolved$data), 2)
	expect_equal(resolved$data, resolved$flat$FlatResponses)
	expect_equal(resolved$flat$ColumnNames$Name, names(resolved$data))

	flat_dedup <- list(
		FlatResponses = data.frame(
			submission_id = c("a", "a", "b"),
			created = c("2020-01-01", "2020-01-02", "2020-01-03"),
			value = c(1, 2, 3),
			stringsAsFactors = FALSE
		),
		ColumnNames = data.frame(
			Number = 1:3,
			Name = c("submission_id", "created", "value"),
			stringsAsFactors = FALSE
		)
	)

	dedup <- DeduplicateSubmissions(flat_dedup, id_col = "submission_id", keep = "last", return_flat = TRUE, quiet = TRUE)
	expect_true(is.data.frame(dedup$data))
	expect_true(is.list(dedup$flat))
	expect_equal(nrow(dedup$data), 2)
	expect_equal(dedup$data, dedup$flat$FlatResponses)
	expect_equal(dedup$flat$ColumnNames$Name, names(dedup$data))

	flat_compact <- list(
		FlatResponses = data.frame(
			`food-apple` = c(TRUE, FALSE),
			`food-banana` = c(FALSE, TRUE),
			stringsAsFactors = FALSE,
			check.names = FALSE
		),
		ColumnNames = data.frame(
			Number = 1:2,
			Name = c("food-apple", "food-banana"),
			stringsAsFactors = FALSE
		)
	)

	compacted <- CompactSelections(flat_compact, sep = "-", return_flat = TRUE, quiet = TRUE)
	expect_true(is.data.frame(compacted$data))
	expect_true(is.list(compacted$flat))
	expect_true("food-selected" %in% names(compacted$data))
	expect_equal(compacted$data, compacted$flat$FlatResponses)
	expect_equal(compacted$flat$ColumnNames$Name, names(compacted$data))
})


test_that("ResolveRepeats collapses FoodTypes back to one row per submission and preserves totals", {
	raw <- foodtypes_raw()
	flat <- foodtypes_flat()

	resolved <- ResolveRepeats(flat, id_col = "form-submission_id", return_flat = TRUE, quiet = TRUE)

	expect_true(is.data.frame(resolved$data))
	expect_equal(nrow(resolved$data), nrow(raw))
	expect_equal(length(unique(resolved$data[["form-submission_id"]])), nrow(raw))

	# In the bundled FoodTypes example, the editGrid cost totals match the stored total_cost.
	has_total <- !is.na(resolved$data[["total_cost"]])
	expect_equal(resolved$data[["editGrid-cost"]][has_total], resolved$data[["total_cost"]][has_total])

	# Spot-check a couple of known totals to catch regressions in repeat handling.
	cost_map <- setNames(resolved$data[["editGrid-cost"]], resolved$data[["form-submission_id"]])
	expect_equal(cost_map[["food-2"]], 15)
	expect_equal(cost_map[["food-3"]], 6)

	expect_true(is.list(resolved$flat))
	expect_equal(resolved$data, resolved$flat$FlatResponses)
})
