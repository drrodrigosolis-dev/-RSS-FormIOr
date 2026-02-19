
test_that("export_results_to_excel writes output", {
	df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
	out <- export_results_to_excel(df, path = tempfile(fileext = ".xlsx"), overwrite = TRUE, quiet = TRUE)

	expect_true(file.exists(out$path))
	expect_true(out$format %in% c("xlsx", "csv"))
	expect_equal(unname(out$rows[1]), 2)
	expect_equal(unname(out$cols[1]), 2)
})


test_that("export_results_to_excel writes multiple CSVs when given sheets", {
	sheets <- list(
		First = data.frame(a = 1),
		Second = data.frame(b = 2)
	)

	out <- export_results_to_excel(sheets, path = tempfile(fileext = ".csv"), overwrite = TRUE, quiet = TRUE)

	expect_equal(length(out$path), 2)
	expect_true(all(file.exists(out$path)))
	expect_equal(out$format, "csv")
})


test_that("build_data_codebook summarizes columns", {
	df <- data.frame(
		age = c(10, 12, NA),
		color = c("red", "blue", "red"),
		stringsAsFactors = FALSE
	)

	cb <- build_data_codebook(df, quiet = TRUE)

	expect_true(all(c("name", "missing", "unique") %in% names(cb)))
	expect_equal(cb$missing[cb$name == "age"], 1)
	expect_equal(cb$unique[cb$name == "color"], 2)
})

test_that("build_data_codebook enriches from schema when column names include prefixes", {
	form <- list(
		title = "Sample",
		components = list(
			list(
				type = "textfield",
				key = "firstName",
				label = "First name",
				input = TRUE,
				description = "Given name",
				validate = list(required = TRUE)
			),
			list(
				type = "select",
				key = "color",
				label = "Color",
				input = TRUE,
				data = list(values = list(
					list(label = "Red", value = "red"),
					list(label = "Blue", value = "blue")
				))
			)
		)
	)

	df <- data.frame(
		`data-firstName` = c("Ana", "Ben"),
		`data-color` = c("red", "blue"),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	cb <- build_data_codebook(df, form = form, quiet = TRUE)

	row_first <- cb[cb$name == "data-firstName", , drop = FALSE]
	expect_equal(row_first$label, "First name")
	expect_equal(row_first$type, "textfield")
	expect_true(isTRUE(row_first$required))
	expect_equal(row_first$description, "Given name")

	row_color <- cb[cb$name == "data-color", , drop = FALSE]
	expect_equal(row_color$type, "select")
	expect_true(grepl("Red", row_color$options))
})


test_that("append_audit_log_entry writes a CSV entry", {
	df <- data.frame(a = 1:3)
	log_path <- tempfile(fileext = ".csv")

	entry <- append_audit_log_entry("export", details = "test", data = df, file = log_path, quiet = TRUE)

	expect_true(file.exists(log_path))
	expect_equal(nrow(entry), 1)
	expect_equal(entry$data_rows, 3)
})
