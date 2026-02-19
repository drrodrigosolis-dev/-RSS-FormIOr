
sample_form <- list(
	title = "Sample Form",
	name = "sample_form",
	version = 2,
	components = list(
		list(
			type = "textfield",
			key = "first_name",
			label = "First name",
			input = TRUE,
			validate = list(required = TRUE)
		),
		list(
			type = "select",
			key = "color",
			label = "Favorite color",
			input = TRUE,
			data = list(values = list(
				list(label = "Red", value = "red"),
				list(label = "Blue", value = "blue")
			))
		),
		list(
			type = "panel",
			title = "Details",
			components = list(
				list(type = "number", key = "age", label = "Age", input = TRUE)
			)
		)
	)
)


test_that("FieldDictionary returns input fields with expected columns", {
	fields <- FieldDictionary(sample_form, quiet = TRUE)

	expect_true(all(c("key", "label", "type", "required", "options", "section") %in% names(fields)))
	expect_true(all(c("first_name", "color", "age") %in% fields$key))
	expect_equal(fields$required[fields$key == "first_name"], TRUE)
	expect_match(fields$options[fields$key == "color"], "Red")
})


test_that("DescribeForm summarizes counts", {
	res <- DescribeForm(sample_form, quiet = TRUE)

	expect_equal(res$counts$fields, 3)
	expect_equal(res$counts$sections, 1)
})


test_that("FieldDictionary can unwrap schema containers", {
	wrapped <- list(schema = sample_form)
	fields <- FieldDictionary(wrapped, quiet = TRUE)

	expect_equal(nrow(fields), 3)
})


test_that("Schema root detection prefers top-level form objects", {
	wrapped <- list(
		schema = list(
			type = "form",
			title = "Top",
			components = list(
				list(type = "panel", title = "Panel", components = list(
					list(type = "textfield", key = "name", label = "Name", input = TRUE)
				)),
				list(type = "number", key = "age", label = "Age", input = TRUE)
			)
		)
	)

	fields <- FieldDictionary(wrapped, include = "all", quiet = TRUE)

	expect_true(all(c("name", "age") %in% fields$key))
})


test_that("FieldDictionary handles missing labels without errors", {
	form <- list(
		type = "form",
		components = list(
			list(type = "panel", key = "", title = NA, components = list(
				list(type = "textfield", key = "field1", label = NA, input = TRUE)
			))
		)
	)

	fields <- FieldDictionary(form, include = "all", quiet = TRUE)
	expect_true(nrow(fields) >= 1)
})


test_that("resolve_version_id picks requested version", {
	meta <- list(
		versions = data.frame(
			id = c("v1-id", "v2-id"),
			formId = c("form-1", "form-1"),
			version = c(1, 2),
			published = c(FALSE, TRUE),
			updatedAt = c("2024-01-01", "2024-02-01"),
			stringsAsFactors = FALSE
		)
	)

	expect_equal(FormIOr:::resolve_version_id(meta, version = 1), "v1-id")
	expect_equal(FormIOr:::resolve_version_id(meta, version = "v2-id"), "v2-id")
	expect_equal(FormIOr:::resolve_version_id(meta, version = "latest"), "v2-id")
})


test_that("CompareFormVersions reports added, removed, and changed fields", {
	old <- sample_form
	new <- list(
		title = "Sample Form",
		components = list(
			list(
				type = "select",
				key = "color",
				label = "Favourite color",
				input = TRUE,
				data = list(values = list(
					list(label = "Red", value = "red"),
					list(label = "Blue", value = "blue")
				))
			),
			list(
				type = "panel",
				title = "Details",
				components = list(
					list(type = "number", key = "age", label = "Age", input = TRUE),
					list(type = "textarea", key = "comments", label = "Comments", input = TRUE)
				)
			)
		)
	)

	res <- CompareFormVersions(old, new, quiet = TRUE)

	expect_equal(res$summary$count[res$summary$metric == "added"], 1)
	expect_equal(res$summary$count[res$summary$metric == "removed"], 1)
	expect_equal(res$summary$count[res$summary$metric == "changed"], 1)
	expect_match(res$changed$changes[1], "label")
})
