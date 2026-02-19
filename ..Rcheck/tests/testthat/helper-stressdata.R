stress_flat <- function() {
	data.frame(
		submission_id = c("s1", "s1", "s2", "s3", "s3", "s3"),
		created_at = c(
			"2026-02-01 10:00:00",
			"2026-02-01 10:05:00",
			"2026-02-02 09:00:00",
			"2026-02-03 12:00:00",
			"2026-02-03 12:30:00",
			"2026-02-03 13:00:00"
		),
		score = c(1, 2, 5, NA, 3, 4),
		score_text = factor(c("1", "2", "5", "", "3", "4")),
		region = c("North", "North", "South", "East", "East", "East"),
		`program-A` = c("Yes", "", "", "No", "Yes", "No"),
		`program-B` = c("", "Yes", "", "No", "No", ""),
		`program-C` = c("", "", "Yes", "", "", "Yes"),
		notes = c("alpha", "beta", "gamma", NA, "delta", "epsilon"),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)
}

