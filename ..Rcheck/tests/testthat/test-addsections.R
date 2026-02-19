test_that("assign_section_hierarchy assigns section levels from supplied non-interactive arguments", {

	df <- data.frame(
		col_a = 1:3,
		col_b = 4:6,
		stringsAsFactors = FALSE
	)

	res <- assign_section_hierarchy(
		df,
		depth = 2,
		section_names = list(
			c("LevelOne"),
			c("LevelTwo")
		),
		section_rows = list(
			list(LevelOne = "1"),
			list(LevelTwo = "2")
		)
	)

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


test_that("assign_section_hierarchy errors on unsupported input types", {
	expect_error(
		assign_section_hierarchy(
			1,
			depth = 1,
			section_names = list(c("LevelOne")),
			section_rows = list(list(LevelOne = "1"))
		),
		"Input must be a data frame"
	)
})


test_that("assign_section_hierarchy flattens FoodTypes when nested columns are present in non-interactive mode", {

	raw <- foodtypes_raw()
	expected_flat <- flatten_submission_records(raw)$FlatResponses

	res <- assign_section_hierarchy(
		raw,
		depth = 1,
		section_names = list(c("LevelOne")),
		section_rows = list(list(LevelOne = "1"))
	)

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


test_that("assign_section_hierarchy requires full prompt arguments in non-interactive sessions", {
	testthat::skip_if(interactive())
	df <- data.frame(col_a = 1:3, stringsAsFactors = FALSE)

	expect_error(assign_section_hierarchy(df, depth = 1), "section_names")
	expect_error(
		assign_section_hierarchy(df, depth = 1, section_names = list(c("OnlySection"))),
		"section_rows"
	)
})
