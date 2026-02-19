
test_that("StartAuditLog creates log and auto logging appends", {
	log_path <- tempfile(fileext = ".csv")
	StartAuditLog(log_path, overwrite = TRUE, quiet = TRUE)

	df <- data.frame(a = 1:2)
	MakeCodebook(df, quiet = TRUE)

	StopAuditLog(quiet = TRUE)

	expect_true(file.exists(log_path))
	log_df <- read.csv(log_path, stringsAsFactors = FALSE)
	expect_true(nrow(log_df) >= 2)
	expect_true(any(log_df$action == "MakeCodebook"))
})
