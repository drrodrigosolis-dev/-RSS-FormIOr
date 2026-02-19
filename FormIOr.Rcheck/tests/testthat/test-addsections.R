test_that("AddSections assigns section levels using mocked console input", {
	testthat::skip_if_not(interactive())

	df <- data.frame(
		col_a = 1:3,
		col_b = 4:6,
		stringsAsFactors = FALSE
	)

	# Expected readline() calls (in order):
	# 1) section depth
	# 2) level-1 section names
	# 3) level-2 section names
	# 4) row numbers for level-1 section
	# 5) row numbers for level-2 section
	answers <- c("2", "LevelOne", "LevelTwo", "1", "2")
	i <- 0

	testthat::local_mocked_bindings(
		readline = function(prompt = "") {
			i <<- i + 1
			answers[[i]]
		},
		.package = "base"
	)

	res <- NULL
	utils::capture.output({
		res <- AddSections(df)
	})

	expect_true(is.list(res))
	expect_true(all(c("FlatResponses", "Sections") %in% names(res)))
	expect_true(is.data.frame(res$FlatResponses))
	expect_equal(res$FlatResponses, df)

	expect_true(is.data.frame(res$Sections))
	expect_true(all(c("No", "Names", "Level-1", "Level-2") %in% names(res$Sections)))

	# Assigned selections.
	expect_equal(res$Sections$`Level-1`[1], "LevelOne")
	expect_equal(res$Sections$`Level-2`[2], "LevelTwo")

	# Unassigned cells should be filled with "General".
	expect_equal(res$Sections$`Level-1`[2], "General")
	expect_equal(res$Sections$`Level-2`[1], "General")
})


test_that("AddSections errors on unsupported input types", {
	if (!interactive()) {
		expect_error(AddSections(1), "interactive")
	} else {
		expect_error(AddSections(1), "Input must be a data frame")
	}
})


test_that("AddSections flattens FoodTypes when nested columns are present", {
	testthat::skip_if_not(interactive())

	raw <- foodtypes_raw()
	expected_flat <- FlattenSubmissions(raw)$FlatResponses

	# Minimal interactive mapping:
	# 1) section depth
	# 2) level-1 section name(s)
	# 3) row numbers for the section
	answers <- c("1", "LevelOne", "1")
	i <- 0

	testthat::local_mocked_bindings(
		readline = function(prompt = "") {
			i <<- i + 1
			answers[[i]]
		},
		.package = "base"
	)

	res <- NULL
	utils::capture.output({
		res <- AddSections(raw)
	})

	expect_true(is.list(res))
	expect_true(all(c("FlatResponses", "Sections") %in% names(res)))
	expect_true(is.data.frame(res$FlatResponses))
	expect_equal(res$FlatResponses, expected_flat)

	expect_true(is.data.frame(res$Sections))
	expect_equal(nrow(res$Sections), ncol(res$FlatResponses))
	expect_equal(res$Sections$Names, names(res$FlatResponses))
	expect_equal(res$Sections$`Level-1`[1], "LevelOne")
	expect_equal(res$Sections$`Level-1`[2], "General")
})
