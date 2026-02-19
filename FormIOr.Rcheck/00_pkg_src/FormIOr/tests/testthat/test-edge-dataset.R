test_that("Edge dataset flattens with expected expansion and columns", {
	raw <- edge_raw()
	flat <- FlattenSubmissions(raw)

	expect_true(is.list(flat))
	expect_true(is.data.frame(flat$FlatResponses))

	expect_equal(nrow(flat$FlatResponses), 7)
	expect_equal(length(unique(flat$FlatResponses[["form-submission_id"]])), nrow(raw))

	expect_true(all(c(
		"form-submission_id",
		"form-created_at",
		"repeatGrid-item",
		"repeatGrid-qty",
		"repeatGrid-cost",
		"repeatGrid-tags",
		"address-street",
		"uploads-name"
	) %in% names(flat$FlatResponses)))

	expect_true(is.list(flat$FlatResponses[["repeatGrid-tags"]]))
})


test_that("Edge dataset findMultilines highlights repeatGrid fields", {
	flat <- edge_flat()
	id_idx <- match("form-submission_id", names(flat$FlatResponses))
	out <- findMultilines(flat, id_col = id_idx)

	expect_equal(dim(out), dim(flat$FlatResponses))
	expect_equal(names(out), names(flat$FlatResponses))
	expect_equal(max(out[["repeatGrid-item"]], na.rm = TRUE), 3)
	expect_equal(unique(na.omit(out[["region"]])), 1)
})


test_that("Edge dataset ResolveRepeats collapses and sums repeatGrid totals", {
	resolved <- ResolveRepeats(edge_flat(), id_col = "form-submission_id", quiet = TRUE)

	expect_equal(nrow(resolved$data), 4)
	expect_equal(length(unique(resolved$data[["form-submission_id"]])), 4)

	cost_map <- setNames(resolved$data[["repeatGrid-cost"]], resolved$data[["form-submission_id"]])
	qty_map <- setNames(resolved$data[["repeatGrid-qty"]], resolved$data[["form-submission_id"]])

	expect_equal(cost_map[["edge-1"]], 0)
	expect_equal(cost_map[["edge-2"]], 15)
	expect_equal(cost_map[["edge-3"]], 6)
	expect_equal(cost_map[["edge-4"]], 9)

	expect_equal(qty_map[["edge-1"]], 0)
	expect_equal(qty_map[["edge-2"]], 3)
	expect_equal(qty_map[["edge-3"]], 3)
	expect_equal(qty_map[["edge-4"]], 3)

	expect_true(is.character(resolved$data[["repeatGrid-tags"]]))
	expect_true(grepl("frozen", resolved$data[["repeatGrid-tags"]][resolved$data[["form-submission_id"]] == "edge-2"]))
})


test_that("Edge dataset CompactSelections builds program-selected values", {
	resolved <- edge_resolved()

	compacted <- CompactSelections(resolved, sep = "-", drop = FALSE, quiet = TRUE)
	out <- compacted$data

	expect_true("program-selected" %in% names(out))

	selected <- setNames(out[["program-selected"]], out[["form-submission_id"]])
	expect_equal(selected[["edge-1"]], "A")
	expect_equal(selected[["edge-2"]], "B, C")
	expect_true(is.na(selected[["edge-3"]]))
	expect_equal(selected[["edge-4"]], "A")
})


test_that("Edge dataset ResponseTimeline counts all submissions", {
	resolved <- edge_resolved()

	tl <- ResponseTimeline(
		resolved,
		date_col = "form-created_at",
		interval = "hour",
		tz = "UTC",
		include_empty = TRUE,
		quiet = TRUE
	)

	expect_true(is.data.frame(tl$data))
	expect_equal(sum(tl$data$count), 4)
})
