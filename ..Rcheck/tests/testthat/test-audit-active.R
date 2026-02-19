test_that("is_audit_log_active reflects Start/stop_audit_log", {
	prev_state <- FormIOr:::get_audit_state()
	on.exit(FormIOr:::set_audit_state(prev_state), add = TRUE)

	log_path <- tempfile(fileext = ".csv")

	start_audit_log(log_path, overwrite = TRUE, quiet = TRUE)
	expect_true(is_audit_log_active())

	stop_audit_log(quiet = TRUE)
	expect_false(is_audit_log_active())
})

