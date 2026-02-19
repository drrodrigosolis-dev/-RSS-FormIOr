
test_that("start_audit_log creates log and auto logging appends", {
	log_path <- tempfile(fileext = ".csv")
	start_audit_log(log_path, overwrite = TRUE, quiet = TRUE)

	df <- data.frame(a = 1:2)
	build_data_codebook(df, quiet = TRUE)

	stop_audit_log(quiet = TRUE)

	expect_true(file.exists(log_path))
	log_df <- read.csv(log_path, stringsAsFactors = FALSE)
	expect_true(nrow(log_df) >= 2)
	expect_true(any(log_df$action == "build_data_codebook"))
})
