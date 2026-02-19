edge_raw <- function() {
	base <- data.frame(
		region = c("North", "South", "East", "West"),
		status = c("draft", "final", "final", "draft"),
		completion_pct = c(5, 50, 100, 25),
		score_text = factor(c("1", "2", "3", "4")),
		`program-A` = c("Yes", "", "No", TRUE),
		`program-B` = c("", "Yes", "", FALSE),
		`program-C` = c(0, 1, "", "No"),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	base$form <- I(list(
		data.frame(
			submission_id = "edge-1",
			created_at = "2026-02-10 08:00:00",
			user = "u1",
			stringsAsFactors = FALSE
		),
		data.frame(
			submission_id = "edge-2",
			created_at = "2026-02-10 09:30:00",
			user = "u2",
			stringsAsFactors = FALSE
		),
		data.frame(
			submission_id = "edge-3",
			created_at = "2026-02-11 10:00:00",
			user = "u3",
			stringsAsFactors = FALSE
		),
		data.frame(
			submission_id = "edge-4",
			created_at = "2026-02-12 11:45:00",
			user = "u4",
			stringsAsFactors = FALSE
		)
	))

	base$address <- I(list(
		data.frame(street = "1 Main", city = "Alpha", postal = "A1A1A1", stringsAsFactors = FALSE),
		data.frame(street = "2 Main", city = "Beta", postal = "B2B2B2", stringsAsFactors = FALSE),
		NULL,
		data.frame(street = "4 Main", city = "Delta", postal = "D4D4D4", stringsAsFactors = FALSE)
	))

	base$uploads <- I(list(
		data.frame(name = "a.pdf", size = 100L, stringsAsFactors = FALSE),
		NULL,
		data.frame(name = "b.pdf", size = 200L, stringsAsFactors = FALSE),
		data.frame(name = "d.pdf", size = 400L, stringsAsFactors = FALSE)
	))

	base$repeatGrid <- I(list(
		NULL,
		data.frame(
			item = c("A", "B"),
			qty = c(1, 2),
			cost = c(5, 10),
			tags = I(list("fresh", c("frozen", "bulk"))),
			stringsAsFactors = FALSE
		),
		data.frame(
			item = c("C"),
			qty = c(3),
			cost = c(6),
			tags = I(list("local")),
			stringsAsFactors = FALSE
		),
		data.frame(
			item = c("D", "E", "F"),
			qty = c(1, 1, 1),
			cost = c(2, 3, 4),
			tags = I(list(c("t1", "t2"), "t3", character(0))),
			stringsAsFactors = FALSE
		)
	))

	base
}

edge_flat <- function() {
	flatten_submission_records(edge_raw())
}

edge_resolved <- function() {
	collapse_repeated_values(
		edge_flat(),
		id_col = "form-submission_id",
		return_flat = TRUE,
		quiet = TRUE
	)$flat
}
