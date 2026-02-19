test_that("IsAuditLogActive reflects Start/StopAuditLog", {
	prev_state <- FormIOr:::get_audit_state()
	on.exit(FormIOr:::set_audit_state(prev_state), add = TRUE)

	log_path <- tempfile(fileext = ".csv")

	StartAuditLog(log_path, overwrite = TRUE, quiet = TRUE)
	expect_true(IsAuditLogActive())

	StopAuditLog(quiet = TRUE)
	expect_false(IsAuditLogActive())
})

