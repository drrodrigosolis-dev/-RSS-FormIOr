
test_that("Wizard plan files are written and include recorded answers", {
	out_dir <- tempfile("formior_test_")
	dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

	ctx <- list(
		wizard = FormIOr:::init_workflow_wizard_state(),
		quiet = TRUE,
		base_url = "https://example.invalid",
		form_id = "form-123",
		api_key = NULL,
		form_meta = list(title = "Test Form"),
		output_dir = out_dir,
		audit_file = file.path(out_dir, "audit_log.csv"),
		id_col = "submissionId",
		files = list(output_dir = out_dir)
	)

	ctx <- FormIOr:::wizard_set_plan_files(ctx, out_dir)
	ctx <- FormIOr:::wiz_record(ctx, "download.now", TRUE)
	ctx <- FormIOr:::wiz_record(ctx, "plots.use_defaults", FALSE)

	expect_true(file.exists(ctx$files$workflow_plan_rds))
	expect_true(file.exists(ctx$files$workflow_plan_json))

	plan <- readRDS(ctx$files$workflow_plan_rds)
	expect_true(is.list(plan))
	expect_true(isTRUE(plan$answers[["download.now"]]))
	expect_false(isTRUE(plan$answers[["plots.use_defaults"]]))
})

