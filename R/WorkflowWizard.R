#' Guided end-to-end workflow (download -> clean -> report -> export)
#'
#' This interactive helper walks you through a complete FormIOr workflow:
#' downloading responses, flattening and cleaning the data, generating simple
#' diagnostics (like a codebook and basic plots), and creating output files.
#'
#' It is designed for non-technical users: you can accept the defaults by
#' pressing Enter at each prompt.
#'
#' Audit logging:
#' - If you start an audit log (see [start_audit_log()]), each step is recorded.
#' - If you do not start a log, FormIOr will ask once per session whether you
#'   want to begin logging.
#'
#' Output folder naming:
#' - When downloading from FormIO and you don't supply `output_dir`, the workflow
#'   will try to fetch basic form metadata first so it can suggest a folder name
#'   based on the form name. This may prompt for credentials a bit earlier than
#'   the download step. The default base directory is `tempdir()` unless you
#'   choose a different location.
#'
#' Output files:
#' - The export workbook includes a `FlattenedRaw` sheet (the flattened data
#'   before any cleaning) and, when available, an `AuditLog` sheet.
#' - Plots are saved as `.png` files under `output_dir/plots` and are listed in
#'   a `Plots` sheet.
#' - The workflow also saves your wizard choices to `workflow_plan.rds` and
#'   `workflow_plan.json` inside the output folder, so you can repeat the same
#'   steps later.
#'
#' Repeatable sessions:
#' - When you run [run_form_processing_workflow()] again, it looks for a previous
#'   `workflow_plan.rds/json` and asks whether you want to reuse it.
#' - You can reuse a previous session in two ways:
#'   1. Apply automatically (no prompts; uses saved answers where available)
#'   2. Use as defaults (prompts still appear, but you can press Enter to accept)
#'
#' @section CHEF credentials and base URL:
#' FormIOr is designed for the BC Public Service CHEF FormIO service.
#' The default base URL is `https://submit.digital.gov.bc.ca/app/api/v1`.
#' If you use a different FormIO service, you must override `base_url` and
#' compatibility is not guaranteed.
#' Note (CHEF): Response export endpoints use `/app/api/v1`, while some
#' metadata/schema endpoints live at `/api/v1`. FormIOr automatically retries
#' the alternate base for metadata/schema requests when needed.
#'
#' For CHEF users, generate your API key from the form's **Manage** page.
#' The Form ID is the final alphanumeric code after the `=` sign in the
#' Manage page URL.
#'
#' @param data Optional. A data.frame of responses to start from. If `NULL`
#'   (default), the workflow can download responses using [fetch_form_responses()].
#' @param base_url Character. API base URL. Default:
#'   `"https://submit.digital.gov.bc.ca/app/api/v1"`.
#' @param form_id Character. Form identifier. If `NULL`, uses stored credentials
#'   or prompts via [AskCredentials()].
#' @param api_key Character. API key / secret. If `NULL`, uses stored credentials
#'   or prompts via [AskCredentials()].
#' @param output_dir Folder to write output files into. If `NULL`, a new folder
#'   is created under `tempdir()` (or you will be prompted to choose). When
#'   downloading from FormIO, the default folder name uses the form name (when
#'   available) plus a timestamp.
#' @param overwrite Logical. If `TRUE`, allow overwriting output files.
#' @param plan Optional list. For non-interactive/scripted runs you can pass a
#'   simple plan list (created manually). If provided, the workflow runs without
#'   prompting and the plan must include `output_dir` (to avoid implicit file
#'   writes). (This is separate from the `workflow_plan.rds` file saved by the
#'   interactive wizard.)
#' @param quiet Logical. If `FALSE`, prints progress messages.
#'
#' @return A list with:
#' \describe{
#'   \item{data_raw}{The downloaded/raw responses (if downloaded).}
#'   \item{flat_raw}{The fully flattened data *before* any cleaning/wrangling steps.}
#'   \item{flat}{A [flatten_submission_records()]-style list with `FlatResponses`.}
#'   \item{reports}{Named list of diagnostic tables (codebook, summaries).}
#'   \item{files}{Named list of output file paths.}
#'   \item{plan}{The plan that was executed (useful for reproducibility).}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Fully guided interactive run
#' out <- run_form_processing_workflow()
#'
#' # Start from an existing data.frame (skip download)
#' out <- run_form_processing_workflow(data = FoodTypes)
#' }
run_form_processing_workflow <- function(
		data = NULL,
		base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
		form_id = NULL,
		api_key = NULL,
		output_dir = NULL,
		overwrite = FALSE,
		plan = NULL,
		quiet = FALSE
) {
	if (!is.null(plan)) {
		return(run_formior_workflow_plan(
			plan = plan,
			data = data,
			base_url = base_url,
			form_id = form_id,
			api_key = api_key,
			quiet = quiet
		))
	}

	if (!interactive()) {
		stop("run_form_processing_workflow() is interactive. Run it in an interactive R session, or pass a 'plan' list for scripted use.")
	}

	# Make schema fetching consistent for build_field_dictionary()/build_data_codebook()
	options(FormIOr.base_url = base_url)

	timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
	ctx <- list(
		wizard = init_workflow_wizard_state(timestamp = timestamp),
		flat = NULL,
		flat_raw = NULL,
		id_col = NULL,
		reports = list(),
		files = list(),
		output_dir = NULL,
		audit_file = NULL,
		overwrite = overwrite,
		quiet = quiet,
		base_url = base_url,
		form_id = form_id,
		api_key = api_key,
		form_meta = NULL
	)

	ctx <- maybe_load_previous_workflow_session(ctx, search_dir = getwd())

	# Best-effort form metadata for (a) output folder naming and (b) codebook enrichment.
	# We suppress the "start audit log?" prompt here so this wizard controls the flow.
	form_meta <- NULL
	form_label <- ctx$wizard$prev_plan$form_label %||% NULL
	if ((is.null(output_dir) || !nzchar(output_dir)) && is.null(data)) {
		# Some existing functions ignore passed credentials unless credentials are cached.
		# If provided, seed it to avoid unexpected credential prompts.
		if (!is.null(form_id) && !is.null(api_key) && nzchar(form_id) && nzchar(api_key)) {
			.formior_state$Form_Info <- c(ID = form_id, Key = api_key)
		}

		audit_state_before <- get_audit_state()
		audit_state_suppress <- audit_state_before
		audit_state_suppress$prompted <- TRUE
		set_audit_state(audit_state_suppress)

		form_meta <- tryCatch(
			fetch_form_metadata(base_url = base_url, form_id = form_id, api_key = api_key),
			error = function(e) NULL
		)

		# Restore audit state (the wizard will handle prompting + file location below).
		set_audit_state(audit_state_before)

		if (is.list(form_meta)) {
			form_label <- form_meta$title %||% form_meta$name %||% form_meta$path %||% NULL
		}
	}
	ctx$form_meta <- form_meta
	ctx$wizard$meta$form_label <- form_label %||% ctx$wizard$meta$form_label %||% NULL

	# ---- Output folder ----------------------------------------------------------
	safe_label <- sanitize_path_fragment(form_label %||% "output")
	output_parent <- ctx$wizard$prev_plan$output_parent %||% tempdir()
	default_out <- file.path(output_parent, paste0("formior_", safe_label, "_", timestamp))
	if (is.null(output_dir) || !nzchar(output_dir)) {
		if (identical(ctx$wizard$mode, "auto")) {
			output_dir <- default_out
			if (!quiet) message("Output folder (auto): ", output_dir)
		} else {
			output_dir <- ask_text("Output folder", default = default_out)
		}
	}
	ctx <- wiz_record(ctx, "output_dir", output_dir)
	output_dir <- path.expand(output_dir)
	if (!dir.exists(output_dir)) {
		dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
	}
	if (!quiet) message("Outputs will be saved in: ", output_dir)
	ctx$output_dir <- output_dir
	ctx$files$output_dir <- output_dir

	ctx <- wizard_set_plan_files(ctx, output_dir)

	# Pre-set the default audit log file so prompts (if any) use this location.
	audit_file <- file.path(output_dir, "audit_log.csv")
	state <- get_audit_state()
	state$file <- audit_file
	set_audit_state(state)
	ctx$audit_file <- audit_file

	# ---- Audit log --------------------------------------------------------------
	start_log <- wiz_yes_no(ctx, "audit.create", "Create an audit log (recommended)?", default = TRUE)
	if (isTRUE(start_log)) {
		ow <- FALSE
		if (file.exists(audit_file)) {
			ow <- wiz_yes_no(ctx, "audit.overwrite", paste0("Audit log exists at ", audit_file, ". Overwrite?"), default = FALSE)
		}
		append_mode <- file.exists(audit_file) && !isTRUE(ow)
		start_audit_log(file = audit_file, overwrite = ow, append = append_mode, quiet = TRUE)
		if (!quiet) message("Audit log: ", audit_file)
	} else {
		# Do not keep prompting during this workflow.
		state <- get_audit_state()
		state$prompted <- TRUE
		set_audit_state(state)
	}

	# ---- Download ---------------------------------------------------------------
	data_raw <- NULL
	if (is.null(data)) {
		download_now <- wiz_yes_no(ctx, "download.now", "Download responses from FormIO now?", default = TRUE)
		if (isTRUE(download_now)) {
			drafts <- wiz_yes_no(ctx, "download.drafts", "Include drafts?", default = FALSE)
			deleted <- wiz_yes_no(ctx, "download.deleted", "Include deleted submissions?", default = FALSE)

			data_raw <- fetch_form_responses(
				base_url = base_url,
				form_id = form_id,
				api_key = api_key,
				drafts = drafts,
				deleted = deleted,
				content.only = TRUE
			)
			data <- data_raw
		} else {
			stop("No data provided and download was skipped.")
		}
	} else {
		ctx <- wiz_record(ctx, "download.now", FALSE)
		data <- as.data.frame(data, stringsAsFactors = FALSE)
	}

	# ---- Flatten ---------------------------------------------------------------
	flat <- NULL
	if (any(vapply(data, is.list, logical(1)))) {
		if (!quiet) message("Step: Flattening nested columns...")
		flat <- flatten_submission_records(data)
	} else {
		flat <- list(
			FlatResponses = data,
			ColumnNames = data.frame(Number = seq_along(names(data)), Name = names(data), stringsAsFactors = FALSE)
		)
	}

	# Keep a copy of the fully flattened data *before* any modifications.
	# This is exported as its own sheet for transparency and reproducibility.
	flat_raw <- as.data.frame(flat$FlatResponses, stringsAsFactors = FALSE)
	df <- flat$FlatResponses
	ctx$flat <- flat
	ctx$flat_raw <- flat_raw

	# ---- Choose ID column -------------------------------------------------------
	id_guess <- guess_submission_id(df)
	id_default <- wiz_prev_value(ctx, "id_col_selected") %||% id_guess
	if (!quiet) message("Suggested submission ID column: ", id_default)
	id_col <- wiz_text(ctx, "id_col_selected", "Submission ID column", default = id_default)
	if (!id_col %in% names(df)) {
		stop("Column '", id_col, "' not found in data.")
	}
	ctx$id_col <- id_col

	# ---- Wrangling / reporting / export -----------------------------------------
	ctx <- run_formior_workflow_steps_interactive(ctx)

	flat <- ctx$flat
	flat_raw <- ctx$flat_raw
	reports <- ctx$reports
	files <- ctx$files

	ctx <- wizard_finalize(ctx)
	plan <- workflow_build_plan(ctx)

	list(
		data_raw = data_raw,
		flat_raw = flat_raw,
		flat = flat,
		reports = reports,
		files = files,
		plan = plan
	)
}


# ---- Internal helpers ---------------------------------------------------------

run_formior_workflow_steps_interactive <- function(ctx) {
	# To add a new wizard step, add a new entry to this list.
	steps <- list(
		list(
			id = "normalize_names",
			prompt = "Normalize column names (recommended)?",
			default = TRUE,
			run = step_normalize_names
		),
		list(
			id = "deduplicate",
			prompt = "Deduplicate submissions (1 row per submission)?",
			default = TRUE,
			run = step_deduplicate
		),
		list(
			id = "resolve_repeats",
			prompt = "Resolve repeated answers within each submission?",
			default = TRUE,
			run = step_resolve_repeats
		),
		list(
			id = "compact_selections",
			prompt = "Compact checkbox / multi-select columns?",
			default = TRUE,
			run = step_compact_selections
		),
		list(
			id = "multivalue_diagnostic",
			prompt = "Check for multi-value columns (diagnostic)?",
			default = TRUE,
			run = step_multivalue_diagnostic
		),
			list(
				id = "rename_cols",
				prompt = "Rename columns interactively (optional)?",
				default = FALSE,
				run = step_rename_cols
			),
			list(
				id = "adjustments",
				prompt = "Apply targeted adjustments (delete submissions / edit values)?",
				default = FALSE,
				run = step_adjustments
			),
			list(
				id = "codebook",
				prompt = "Create a codebook (recommended)?",
				default = TRUE,
			run = step_codebook
		),
		list(
			id = "plots",
			prompt = "Create diagnostic plots (recommended)?",
			default = TRUE,
			run = step_plots
		),
		list(
			id = "export",
			prompt = "Create output file(s) now?",
			default = TRUE,
			run = step_export
		)
	)

	for (st in steps) {
		do_step <- wiz_yes_no(ctx, paste0("step.", st$id), st$prompt, default = st$default)
		if (!isTRUE(do_step)) next
		ctx <- st$run(ctx)
	}

	# Record (best-effort) audit log file path
	if (is_audit_log_active() && file.exists(ctx$audit_file)) {
		ctx$files$audit_log <- ctx$audit_file
	}

	ctx
}


step_normalize_names <- function(ctx) {
	norm <- standardize_column_names(ctx$flat, return_flat = TRUE, quiet = TRUE)
	ctx$reports$name_map <- norm$name_map
	ctx$flat <- norm$flat

	# If the ID column was renamed, keep using the updated name.
	if (!is.null(norm$name_map) && all(c("OldName", "NewName") %in% names(norm$name_map))) {
		idx <- match(ctx$id_col, norm$name_map$OldName)
		if (!is.na(idx)) {
			ctx$id_col <- norm$name_map$NewName[idx]
			if (!ctx$quiet) message("Updated submission ID column to: ", ctx$id_col)
		}
	}

	ctx
}


step_deduplicate <- function(ctx) {
	dedup <- deduplicate_submission_rows(ctx$flat, id_col = ctx$id_col, return_flat = TRUE, quiet = TRUE)
	ctx$reports$dedup_summary <- as.data.frame(dedup$summary, stringsAsFactors = FALSE)
	ctx$flat <- dedup$flat
	ctx
}


step_resolve_repeats <- function(ctx) {
	resolved <- collapse_repeated_values(ctx$flat, id_col = ctx$id_col, strategy = "auto", return_flat = TRUE, quiet = TRUE)
	ctx$reports$repeat_summary <- resolved$summary
	ctx$flat <- resolved$flat
	ctx
}


step_compact_selections <- function(ctx) {
	compacted <- collapse_checkbox_selections(ctx$flat, return_flat = TRUE, quiet = TRUE)
	ctx$reports$compact_summary <- compacted$summary
	ctx$flat <- compacted$flat
	ctx
}


step_multivalue_diagnostic <- function(ctx) {
	df_now <- ctx$flat$FlatResponses
	id_idx <- match(ctx$id_col, names(df_now))
	if (is.na(id_idx)) id_idx <- 1

	multi_counts <- count_multivalue_fields(ctx$flat, id_col = id_idx)
	ctx$reports$multi_value_max <- summarize_multivalue(multi_counts, id_col_name = names(df_now)[id_idx])

	needs_fix <- any(ctx$reports$multi_value_max$max_distinct > 1.05, na.rm = TRUE)
	if (needs_fix) {
		run_fix <- wiz_yes_no(
			ctx,
			"fixdups.run",
			"Multi-value columns found. Run resolve_duplicate_values cleaner now?",
			default = TRUE
		)

		if (isTRUE(run_fix)) {
			prev_strategies <- wiz_prev_value(ctx, "fixdups.strategies")
			use_prev <- FALSE
			if (!is.null(prev_strategies) && length(prev_strategies) > 0) {
				if (identical(ctx$wizard$mode, "auto")) {
					use_prev <- TRUE
				} else {
					use_prev <- wiz_yes_no(
						ctx,
						"fixdups.use_previous",
						"Reuse saved resolve_duplicate_values decisions from the previous session?",
						default = TRUE
					)
				}
			}

			fixed <- resolve_duplicate_values(
				ctx$flat,
				multi_counts = multi_counts,
				id_col = ctx$id_col,
				quiet = ctx$quiet,
				dry_run = FALSE,
				strategies = if (isTRUE(use_prev)) prev_strategies else NULL,
				prompt = !identical(ctx$wizard$mode, "auto")
			)

			ctx$reports$fixdups_decisions <- fixed$decisions
			ctx <- wiz_record(ctx, "fixdups.strategies", as.data.frame(fixed$decisions, stringsAsFactors = FALSE))

			ctx$flat <- list(
				FlatResponses = fixed$cleaned,
				ColumnNames = data.frame(
					Number = seq_along(names(fixed$cleaned)),
					Name = names(fixed$cleaned),
					stringsAsFactors = FALSE
				)
			)
		}
	}

	ctx
}


step_rename_cols <- function(ctx) {
	prev_map <- wiz_prev_value(ctx, "rename.map")
	use_prev <- FALSE
	if (!is.null(prev_map) && length(prev_map) > 0) {
		if (identical(ctx$wizard$mode, "auto")) {
			use_prev <- TRUE
		} else {
			use_prev <- wiz_yes_no(ctx, "rename.use_previous", "Reuse the saved column renaming map from the previous session?", default = TRUE)
		}
	}

	if (identical(ctx$wizard$mode, "auto") && !isTRUE(use_prev)) {
		if (!ctx$quiet) message("Rename step skipped (auto mode): no saved renaming map was found.")
		return(ctx)
	}

	ren <- rename_columns_from_dictionary(
		ctx$flat,
		NamesDF = TRUE,
		renameDF = TRUE,
		rename_map = if (isTRUE(use_prev)) prev_map else NULL,
		quiet = TRUE
	)
	if (!is.null(ren$renamedDF)) ctx$reports$rename_map <- ren$renamedDF
	if (!is.null(ren$flat)) ctx$flat <- ren$flat
	if (!is.null(ren$renamedDF)) {
		ctx <- wiz_record(ctx, "rename.map", as.data.frame(ren$renamedDF, stringsAsFactors = FALSE))
	}

	# Keep tracking the submission ID column if it was renamed.
	if (!is.null(ren$renamedDF) && all(c("OldNames", "NewNames") %in% names(ren$renamedDF))) {
		idx <- match(ctx$id_col, ren$renamedDF$OldNames)
		if (!is.na(idx)) {
			ctx$id_col <- ren$renamedDF$NewNames[idx]
			if (!ctx$quiet) message("Updated submission ID column to: ", ctx$id_col)
		}
	}

	ctx
}


step_adjustments <- function(ctx) {
	df <- ctx$flat$FlatResponses
	df <- as.data.frame(df, stringsAsFactors = FALSE)

	if (!ctx$quiet) {
		message("Adjustment step: delete specific submissions, or change values for specific submission IDs.")
		message("Tip: if you only want to remove test rows, deleting submission IDs is usually enough.")
	}

	prev_delete_ids <- wiz_prev_value(ctx, "adjust.delete_ids")
	prev_delete_ids <- as.character(prev_delete_ids %||% character(0))
	prev_delete_ids <- prev_delete_ids[prev_delete_ids != ""]

	delete_ids <- character(0)
	if (wiz_yes_no(ctx, "adjust.delete", "Delete specific submission IDs?", default = length(prev_delete_ids) > 0)) {
		default_raw <- if (length(prev_delete_ids) > 0) paste(prev_delete_ids, collapse = ", ") else ""
		raw <- wiz_text(ctx, "adjust.delete_ids_raw", "Enter submission IDs to delete (comma-separated)", default = default_raw)
		delete_ids <- unique(trimws(unlist(strsplit(raw, "[,;]+"))))
		delete_ids <- delete_ids[delete_ids != ""]
		ctx <- wiz_record(ctx, "adjust.delete_ids", delete_ids)
	}

	updates <- NULL
	prev_updates <- wiz_prev_value(ctx, "adjust.updates")
	if (!is.null(prev_updates) && inherits(prev_updates, "data.frame")) {
		prev_updates <- as.data.frame(prev_updates, stringsAsFactors = FALSE)
		rownames(prev_updates) <- NULL
	}

	edit_values <- wiz_yes_no(
		ctx,
		"adjust.edit_values",
		"Edit specific values (by submission ID)?",
		default = !is.null(prev_updates) && nrow(prev_updates) > 0
	)

	if (isTRUE(edit_values)) {
		use_prev <- FALSE
		if (!is.null(prev_updates) && nrow(prev_updates) > 0) {
			if (identical(ctx$wizard$mode, "auto")) {
				use_prev <- TRUE
			} else {
				use_prev <- wiz_yes_no(
					ctx,
					"adjust.use_previous_updates",
					"Reuse the saved value edits from the previous session?",
					default = TRUE
				)
			}
		}

		if (isTRUE(use_prev) && !is.null(prev_updates)) {
			updates <- prev_updates
		} else {
			if (identical(ctx$wizard$mode, "auto")) {
				if (!ctx$quiet) message("Skipping value edits (auto mode): no saved edits were found.")
				updates <- NULL
			} else {
			if (!ctx$quiet) {
				message("Enter edits one-by-one. Leave the submission ID blank to finish.")
				message("Use NA to set a value to missing.")
			}

			rows <- list()
			repeat {
				id_val <- ask_text("Submission ID", default = "")
				if (!nzchar(id_val)) break

				col_nm <- ask_text("Column name to edit", default = "")
				if (!nzchar(col_nm)) {
					if (!ctx$quiet) message("No column provided; skipping.")
					next
				}

				if (!col_nm %in% names(df)) {
					if (!ctx$quiet) message("Column not found: ", col_nm)
					next
				}

				id_rows <- which(as.character(df[[ctx$id_col]]) == id_val)
				if (length(id_rows) == 0) {
					if (!ctx$quiet) message("Submission ID not found: ", id_val)
					next
				}

				old_val <- list_element_to_text(df[[col_nm]][id_rows[1]])
				if (!ctx$quiet) message("Current value (first match): ", old_val)

				new_val <- ask_text(paste0("New value for '", col_nm, "'"), default = "")
				rows[[length(rows) + 1]] <- data.frame(
					id = id_val,
					column = col_nm,
					value = new_val,
					stringsAsFactors = FALSE
				)
			}

			if (length(rows) > 0) {
				updates <- do.call(rbind, rows)
				rownames(updates) <- NULL
			}
			}
		}

		if (!is.null(updates) && nrow(updates) > 0) {
			ctx <- wiz_record(ctx, "adjust.updates", as.data.frame(updates, stringsAsFactors = FALSE))
		}
	}

	if (length(delete_ids) == 0 && (is.null(updates) || nrow(updates) == 0)) {
		if (!ctx$quiet) message("No adjustments requested.")
		return(ctx)
	}

	adj <- apply_submission_updates(
		ctx$flat,
		id_col = ctx$id_col,
		delete_ids = if (length(delete_ids) > 0) delete_ids else NULL,
		updates = updates,
		return_flat = TRUE,
		quiet = TRUE
	)

	ctx$flat <- adj$flat
	ctx$reports$adjust_summary <- adj$summary
	ctx$reports$adjust_changes <- adj$changes

	if (!ctx$quiet) {
		rows_deleted <- adj$summary$value[adj$summary$metric == "rows_deleted"] %||% 0
		updates_applied <- adj$summary$value[adj$summary$metric == "updates_applied"] %||% 0
		message("Adjustments complete: deleted ", rows_deleted, " row(s); applied ", updates_applied, " update(s).")
	}

	ctx
}


step_codebook <- function(ctx) {
	include_schema <- wiz_yes_no(ctx, "codebook.include_schema", "Include labels/sections from the form schema?", default = TRUE)

	form_obj <- NULL
	if (isTRUE(include_schema)) {
		form_obj <- ctx$form_meta %||% tryCatch(
			fetch_form_metadata(base_url = ctx$base_url, form_id = ctx$form_id, api_key = ctx$api_key),
			error = function(e) NULL
		)
		ctx$form_meta <- form_obj
	}

	ctx$reports$codebook <- build_data_codebook(ctx$flat, form = form_obj, quiet = TRUE)
	ctx
}


step_plots <- function(ctx) {
	plots_dir <- file.path(ctx$output_dir, "plots")
	if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

	suggested <- suggest_workflow_plots(ctx$flat, id_col = ctx$id_col)

	if (!ctx$quiet) {
		message("Plotting step: you can accept the recommended plots, or choose your own columns.")
		if (!is.null(suggested$date_col)) {
			message("Suggested timeline column: ", suggested$date_col)
		}
		if (length(suggested$numeric) > 0) {
			message("Suggested numeric columns: ", paste(suggested$numeric, collapse = ", "))
		}
		if (length(suggested$categorical) > 0) {
			message("Suggested categorical columns: ", paste(suggested$categorical, collapse = ", "))
		}
		if (!is.null(suggested$text)) {
			message("Suggested text column (wordcloud): ", suggested$text)
		}
	}

	use_defaults <- wiz_yes_no(ctx, "plots.use_defaults", "Use recommended plots and columns?", default = TRUE)

	config <- NULL
	if (!isTRUE(use_defaults)) {
		df_names <- suggested$df_names

		config <- list(
			timeline = list(enabled = FALSE, date_col = NULL, interval = "month"),
			histogram = list(enabled = FALSE, fields = character(0)),
			bar = list(enabled = FALSE, fields = character(0), top_n = 10, horiz = TRUE),
			wordcloud = list(enabled = FALSE, field = NULL)
		)

		# Timeline
		timeline_enabled <- wiz_yes_no(
			ctx,
			"plots.timeline.enabled",
			"Create a response timeline plot?",
			default = !is.null(suggested$date_col)
		)
		if (isTRUE(timeline_enabled)) {
			date_default <- wiz_prev_value(ctx, "plots.timeline.date_col") %||% suggested$date_col %||% ""
			date_col <- wiz_text(ctx, "plots.timeline.date_col", "Date/time column for timeline", default = date_default)

			if (nzchar(date_col) && date_col %in% df_names) {
				interval_default <- wiz_prev_value(ctx, "plots.timeline.interval") %||% "month"
				interval <- wiz_text(ctx, "plots.timeline.interval", "Timeline interval (day/week/month/hour)", default = interval_default)
				interval <- tolower(trimws(interval))
				if (!interval %in% c("day", "week", "month", "hour")) interval <- "month"

				config$timeline <- list(enabled = TRUE, date_col = date_col, interval = interval)
			} else if (!ctx$quiet) {
				message("Timeline column not found; skipping timeline plot: ", date_col)
			}
		}

		# Histograms
		hist_enabled <- wiz_yes_no(
			ctx,
			"plots.histogram.enabled",
			"Create histogram plot(s) for numeric columns?",
			default = length(suggested$numeric) > 0
		)
		if (isTRUE(hist_enabled)) {
			raw_default <- wiz_prev_value(ctx, "plots.histogram.fields_raw") %||% paste(suggested$numeric, collapse = ", ")
			input <- wiz_text(ctx, "plots.histogram.fields_raw", "Numeric columns for histograms (comma-separated)", default = raw_default)
			fields <- parse_column_list(input, df_names = df_names)
			fields <- intersect(fields, df_names)
			config$histogram <- list(enabled = length(fields) > 0, fields = fields)
		}

		# Bar plots
		bar_enabled <- wiz_yes_no(
			ctx,
			"plots.bar.enabled",
			"Create bar chart(s) for categorical columns?",
			default = length(suggested$categorical) > 0
		)
		if (isTRUE(bar_enabled)) {
			raw_default <- wiz_prev_value(ctx, "plots.bar.fields_raw") %||% paste(suggested$categorical, collapse = ", ")
			input <- wiz_text(ctx, "plots.bar.fields_raw", "Categorical columns for bar charts (comma-separated)", default = raw_default)
			fields <- parse_column_list(input, df_names = df_names)
			fields <- intersect(fields, df_names)

			top_default <- as.character(wiz_prev_value(ctx, "plots.bar.top_n") %||% "10")
			top_n <- suppressWarnings(as.integer(wiz_text(ctx, "plots.bar.top_n", "How many categories to show per bar chart?", default = top_default)))
			if (is.na(top_n) || top_n <= 0) top_n <- 10

			config$bar <- list(enabled = length(fields) > 0, fields = fields, top_n = top_n, horiz = TRUE)
		}

		# Wordcloud
		if (wiz_yes_no(ctx, "plots.wordcloud.enabled", "Create a wordcloud for a text column (optional)?", default = !is.null(suggested$text))) {
			if (!requireNamespace("wordcloud", quietly = TRUE)) {
				if (!ctx$quiet) {
					message("Skipping wordcloud: package 'wordcloud' is not installed.")
				}
			} else {
				field_default <- wiz_prev_value(ctx, "plots.wordcloud.field") %||% suggested$text %||% ""
				field <- wiz_text(ctx, "plots.wordcloud.field", "Text column for wordcloud", default = field_default)
				if (nzchar(field) && field %in% df_names) {
					config$wordcloud <- list(enabled = TRUE, field = field)
				} else if (!ctx$quiet) {
					message("Text column not found; skipping wordcloud: ", field)
				}
			}
		}
	}

	out <- create_workflow_plots(
		flat = ctx$flat,
		id_col = ctx$id_col,
		output_dir = plots_dir,
		config = config,
		quiet = ctx$quiet
	)

	if (!is.null(out$manifest) && nrow(out$manifest) > 0) {
		ctx$reports$plots <- out$manifest
		ctx$files$plots <- out$files
		if (!ctx$quiet) message("Saved ", length(out$files), " plot file(s) in: ", plots_dir)
	} else if (!ctx$quiet) {
		message("No plots were created (no suitable fields found).")
	}

	ctx
}


step_export <- function(ctx) {
	can_xlsx <- excel_writer_available()
	default_ext <- if (can_xlsx) "xlsx" else "csv"
	default_name <- paste0("FormIOr_output.", default_ext)
	default_export <- file.path(ctx$output_dir, default_name)

	if (!ctx$quiet) {
		if (can_xlsx) {
			message("Export step: an Excel file (.xlsx) will be created with multiple tabs (Responses, Codebook, etc.).")
		} else {
			message("Export step: Excel writer package not found, so CSV file(s) will be created instead (one file per tab).")
			message("Tip: to create a single Excel file, install.packages('writexl') or install.packages('openxlsx').")
		}
		message("Choose where to save the export (press Enter to accept the default).")
	}

	prev_input <- wiz_prev_value(ctx, "export.path_input")
	if (!is.null(prev_input) && nzchar(as.character(prev_input)[1])) {
		prev_input <- as.character(prev_input)[1]
		if (is_absolute_path(prev_input)) {
			# Keep exports inside the current output folder by default.
			prev_input <- file.path(ctx$output_dir, basename(prev_input))
		}
	} else {
		prev_input <- NULL
	}

	if (!ctx$quiet) message("Your export will be saved in: ", ctx$output_dir)

	export_input <- wiz_text(
		ctx,
		"export.path_input",
		"Export file name (or full path)",
		default = prev_input %||% default_name
	)
	export_path <- coerce_export_path(
		export_input,
		output_dir = ctx$output_dir,
		default_ext = default_ext,
		default_path = default_export
	)
	ctx <- wiz_record(ctx, "export.path", export_path)

	if (!ctx$quiet) message("Saving export to: ", export_path)

	# Make sure the export action is captured *inside* the exported AuditLog sheet.
	# (export_results_to_excel() also logs automatically, but that would happen after the
	# workbook is written, so it wouldn't appear in the copied sheet.)
	if (is_audit_log_active() && !is.null(ctx$audit_file)) {
		append_audit_log_entry(
			action = "export_results_to_excel",
			details = paste0("path=", export_path),
			file = ctx$audit_file,
			data = ctx$flat,
			append = TRUE,
			quiet = TRUE
		)
	}

	sheets <- build_export_sheets(
		flat = ctx$flat,
		reports = ctx$reports,
		flat_raw = ctx$flat_raw,
		audit_file = ctx$audit_file
	)

	# Avoid duplicating the export entry in the audit log.
	state_before <- get_audit_state()
	state_suppress <- state_before
	state_suppress$active <- FALSE
	set_audit_state(state_suppress)
	on.exit(set_audit_state(state_before), add = TRUE)

	export_out <- export_results_to_excel(sheets, path = export_path, overwrite = ctx$overwrite, quiet = TRUE)
	ctx$files$export <- export_out$path

	if (!ctx$quiet) {
		message("Export saved (", export_out$format, "):")
		message(paste(ctx$files$export, collapse = "\n"))
		message("Folder: ", ctx$output_dir)
	}

	ctx
}


run_formior_workflow_plan <- function(plan, data = NULL, base_url, form_id, api_key, quiet = FALSE) {
	# Minimal non-interactive runner used for tests and scripted runs.
	# The plan structure is intentionally simple and only supports a subset
	# of the interactive workflow options.
	options(FormIOr.base_url = base_url)

	output_dir <- plan$output_dir
	if (is.null(output_dir) || (length(output_dir) > 0 && is.na(output_dir[1])) || !nzchar(as.character(output_dir)[1])) {
		stop("Plan must include output_dir for non-interactive runs. Provide a writable folder path.")
	}
	output_dir <- path.expand(as.character(output_dir)[1])
	if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

	audit_file <- file.path(output_dir, "audit_log.csv")
	state <- get_audit_state()
	state$file <- audit_file
	set_audit_state(state)

	if (isTRUE(plan$audit)) {
		start_audit_log(audit_file, overwrite = TRUE, quiet = TRUE)
	} else {
		state <- get_audit_state()
		state$prompted <- TRUE
		set_audit_state(state)
	}

	data_raw <- NULL
	if (is.null(data)) {
		if (!isTRUE(plan$download)) stop("Plan requested no download and no data was supplied.")
		data_raw <- fetch_form_responses(
			base_url = base_url,
			form_id = form_id,
			api_key = api_key,
			drafts = isTRUE(plan$drafts),
			deleted = isTRUE(plan$deleted),
			content.only = TRUE
		)
		data <- data_raw
	}

	flat <- if (any(vapply(data, is.list, logical(1)))) {
		flatten_submission_records(data)
	} else {
		list(
			FlatResponses = as.data.frame(data, stringsAsFactors = FALSE),
			ColumnNames = data.frame(Number = seq_along(names(data)), Name = names(data), stringsAsFactors = FALSE)
		)
	}

	flat_raw <- as.data.frame(flat$FlatResponses, stringsAsFactors = FALSE)

	df <- flat$FlatResponses
	id_col <- plan$id_col %||% guess_submission_id(df)

	reports <- list()

	if (isTRUE(plan$normalize_names)) {
		norm <- standardize_column_names(flat, return_flat = TRUE, quiet = TRUE)
		reports$name_map <- norm$name_map
		flat <- norm$flat
	}

	if (isTRUE(plan$codebook)) {
		form_obj <- NULL
		if (isTRUE(plan$include_schema)) {
			form_obj <- fetch_form_metadata(base_url = base_url, form_id = form_id, api_key = api_key)
		}
		reports$codebook <- build_data_codebook(flat, form = form_obj, quiet = TRUE)
	}

	files <- list(output_dir = output_dir)
	if (isTRUE(plan$export)) {
		export_path <- plan$export_path %||% file.path(output_dir, "FormIOr_output.csv")
		sheets <- build_export_sheets(flat = flat, reports = reports, flat_raw = flat_raw, audit_file = audit_file)
		files$export <- export_results_to_excel(sheets, path = export_path, overwrite = TRUE, quiet = TRUE)$path
	}

	if (is_audit_log_active() && file.exists(audit_file)) files$audit_log <- audit_file

	list(
		data_raw = data_raw,
		flat_raw = flat_raw,
		flat = flat,
		reports = reports,
		files = files,
		plan = plan
	)
}


ask_yes_no <- function(prompt, default = TRUE) {
	default <- isTRUE(default)
	suffix <- if (default) "[Y/n]" else "[y/N]"

	while (TRUE) {
		answer <- readline(paste0(prompt, " ", suffix, ": "))
		answer <- trimws(tolower(answer))

		if (!nzchar(answer)) return(default)
		if (answer %in% c("y", "yes")) return(TRUE)
		if (answer %in% c("n", "no")) return(FALSE)

		message("Please answer y or n.")
	}
}


ask_text <- function(prompt, default = "") {
	full <- if (nzchar(default)) paste0(prompt, " [", default, "]: ") else paste0(prompt, ": ")
	answer <- readline(full)
	answer <- trimws(answer)
	if (!nzchar(answer)) default else answer
}


guess_submission_id <- function(df) {
	candidates <- c(
		"form_submissionid",
		"form-submissionId",
		"submissionId",
		"submissionid",
		"submission_id",
		"_id",
		"id",
		"confirmationId",
		"formSubmissionId"
	)

	for (nm in candidates) {
		if (nm %in% names(df)) return(nm)
	}

	names(df)[1]
}


summarize_multivalue <- function(multi_counts, id_col_name) {
	if (!inherits(multi_counts, "data.frame") || nrow(multi_counts) == 0) {
		return(data.frame(column = character(), max_distinct = integer(), stringsAsFactors = FALSE))
	}

	cols <- setdiff(names(multi_counts), id_col_name)
	if (length(cols) == 0) {
		return(data.frame(column = character(), max_distinct = integer(), stringsAsFactors = FALSE))
	}

	max_distinct <- vapply(cols, function(nm) {
		val <- multi_counts[[nm]]
		suppressWarnings(max(val, na.rm = TRUE))
	}, numeric(1))

	data.frame(
		column = cols,
		max_distinct = as.integer(max_distinct),
		stringsAsFactors = FALSE
	)[order(-max_distinct), , drop = FALSE]
}


build_export_sheets <- function(flat, reports, flat_raw = NULL, audit_file = NULL) {
	sheets <- list()

	if (!is.null(flat_raw) && inherits(flat_raw, "data.frame")) {
		sheets[["FlattenedRaw"]] <- as.data.frame(flat_raw, stringsAsFactors = FALSE)
	}

	sheets[["Responses"]] <- as.data.frame(flat$FlatResponses, stringsAsFactors = FALSE)

	add_sheet <- function(name, x) {
		if (is.null(x)) return(invisible(NULL))
		if (!inherits(x, "data.frame")) return(invisible(NULL))
		if (nrow(x) == 0 && ncol(x) == 0) return(invisible(NULL))
		sheets[[name]] <<- as.data.frame(x, stringsAsFactors = FALSE)
		invisible(NULL)
	}

	add_sheet("Codebook", reports$codebook)
	add_sheet("NameMap", reports$name_map)
	add_sheet("DedupSummary", reports$dedup_summary)
	add_sheet("RepeatSummary", reports$repeat_summary)
	add_sheet("CompactSummary", reports$compact_summary)
	add_sheet("MultiValueMax", reports$multi_value_max)
	add_sheet("FixDupsDecisions", reports$fixdups_decisions)
	add_sheet("RenameMap", reports$rename_map)
	add_sheet("AdjustSummary", reports$adjust_summary)
	add_sheet("AdjustChanges", reports$adjust_changes)
	add_sheet("Plots", reports$plots)

	audit_df <- read_audit_log_file(audit_file)
	if (is.null(audit_df) && !is.null(audit_file) && nzchar(audit_file)) {
		audit_df <- data.frame(
			message = paste0("No audit log found at: ", audit_file),
			stringsAsFactors = FALSE
		)
	}
	add_sheet("AuditLog", audit_df)

	# Ensure each sheet is export-friendly (no list/data.frame columns).
	sheets <- lapply(sheets, make_exportable_df)
	sheets
}


excel_writer_available <- function() {
	requireNamespace("writexl", quietly = TRUE) || requireNamespace("openxlsx", quietly = TRUE)
}


coerce_export_path <- function(path, output_dir, default_ext, default_path) {
	path <- trimws(path)
	if (!nzchar(path)) path <- default_path

	# Help users who accidentally type a yes/no response at a file prompt.
	if (tolower(path) %in% c("y", "yes", "n", "no")) {
		path <- default_path
	}

	path <- path.expand(path)
	if (!is_absolute_path(path)) {
		path <- file.path(output_dir, path)
	}

	ext <- tolower(tools::file_ext(path))
	if (!nzchar(ext)) {
		path <- paste0(path, ".", default_ext)
	}

	path
}


is_absolute_path <- function(path) {
	if (!nzchar(path)) return(FALSE)
	if (grepl("^/", path)) return(TRUE)
	if (grepl("^[A-Za-z]:[/\\\\]", path)) return(TRUE)
	FALSE
}


sanitize_path_fragment <- function(x, max_chars = 60) {
	x <- as.character(x)[1]
	x <- trimws(x)
	if (!nzchar(x) || is.na(x)) return("formior_output")

	# Keep it filesystem-friendly and predictable.
	x <- gsub("[^A-Za-z0-9]+", "_", x)
	x <- gsub("^_+|_+$", "", x)
	x <- gsub("_+", "_", x)
	x <- substr(x, 1, max_chars)
	if (!nzchar(x)) "formior_output" else x
}


read_audit_log_file <- function(file) {
	if (is.null(file) || !nzchar(file) || !file.exists(file)) return(NULL)

	ext <- tolower(tools::file_ext(file))
	sep <- if (ext == "tsv") "\t" else ","

	tryCatch(
		utils::read.table(
			file,
			sep = sep,
			header = TRUE,
			stringsAsFactors = FALSE,
			quote = "\"",
			comment.char = "",
			fill = TRUE
		),
		error = function(e) NULL
	)
}


make_exportable_df <- function(df) {
	if (!inherits(df, "data.frame")) return(df)

	out <- as.data.frame(df, stringsAsFactors = FALSE)
	if (nrow(out) == 0) return(out)

	for (nm in names(out)) {
		col <- out[[nm]]

		# Keep dates/times as-is.
		if (inherits(col, "Date") || inherits(col, "POSIXt")) next

		# Factors are confusing in exports; convert to text.
		if (is.factor(col)) {
			out[[nm]] <- as.character(col)
			next
		}

		# Nested data.frame columns (e.g., nested tibbles).
		if (inherits(col, "data.frame")) {
			out[[nm]] <- vapply(seq_len(nrow(col)), function(i) {
				list_element_to_text(col[i, , drop = FALSE])
			}, character(1))
			next
		}

		# List-columns (uploads, objects, etc.)
		if (is.list(col)) {
			out[[nm]] <- vapply(col, list_element_to_text, character(1))
			next
		}

		# Matrix columns (rare) -> one string per row
		if (is.matrix(col)) {
			out[[nm]] <- vapply(seq_len(nrow(col)), function(i) {
				paste(as.character(col[i, ]), collapse = ", ")
			}, character(1))
		}
	}

	out
}


create_workflow_plots <- function(flat, id_col, output_dir, config = NULL, quiet = FALSE) {
	df <- if (is.list(flat) && !is.null(flat$FlatResponses) && inherits(flat$FlatResponses, "data.frame")) {
		flat$FlatResponses
	} else if (inherits(flat, "data.frame")) {
		flat
	} else {
		return(list(manifest = data.frame(), files = character(0)))
	}

	df <- as.data.frame(df, stringsAsFactors = FALSE)
	if (nrow(df) == 0 || ncol(df) == 0) {
		return(list(manifest = data.frame(), files = character(0)))
	}

	if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

	exclude <- unique(stats::na.omit(c(as.character(id_col), guess_time_col(names(df)))))
	suggested <- suggest_workflow_plots_from_df(df, exclude = exclude)

	rows <- list()
	files <- character(0)

	add_row <- function(plot_type, field, file = NA_character_, notes = NA_character_) {
		rows[[length(rows) + 1]] <<- data.frame(
			plot_type = plot_type,
			field = field,
			file = file,
			notes = notes,
			stringsAsFactors = FALSE
		)
		if (!is.na(file) && nzchar(file)) files <<- c(files, file)

		if (is_audit_log_active() && !is.na(file) && nzchar(file)) {
			maybe_write_audit("PlotSaved", details = paste0("type=", plot_type, "; field=", field, "; file=", file))
		}
	}

	make_unique_file <- function(path) {
		if (!file.exists(path)) return(path)
		base <- tools::file_path_sans_ext(path)
		ext <- tools::file_ext(path)
		i <- 1L
		repeat {
			candidate <- paste0(base, "_", i, ".", ext)
			if (!file.exists(candidate)) return(candidate)
			i <- i + 1L
		}
	}

	save_plot_png <- function(file, plot_fun) {
		file <- path.expand(file)
		file <- make_unique_file(file)

		ok <- FALSE
		err <- NULL
		grDevices::png(filename = file, width = 1200, height = 700, res = 150)
		on.exit({
			try(grDevices::dev.off(), silent = TRUE)
			if (!ok && file.exists(file)) file.remove(file)
		}, add = TRUE)

		tryCatch(
			{
				plot_fun()
				ok <- TRUE
			},
			error = function(e) {
				err <- conditionMessage(e)
			}
		)

		list(ok = ok, file = file, error = err)
	}

	# ---- Timeline ---------------------------------------------------------------
	timeline_cfg <- config$timeline
	if (is.null(timeline_cfg)) {
		timeline_cfg <- list(enabled = !is.null(suggested$date_col), date_col = suggested$date_col, interval = "month")
	}
	if (isTRUE(timeline_cfg$enabled)) {
		date_col <- timeline_cfg$date_col %||% suggested$date_col
		interval <- timeline_cfg$interval %||% "month"

		if (!is.null(date_col) && nzchar(date_col)) {
			file <- file.path(output_dir, paste0("timeline_", sanitize_path_fragment(date_col), ".png"))
			res <- save_plot_png(file, function() {
				plot_response_timeline(
					x = flat,
					date_col = date_col,
					interval = interval,
					main = "Responses Over Time"
				)
			})

			if (isTRUE(res$ok)) {
				add_row("timeline", date_col, res$file)
			} else {
				add_row("timeline", date_col, notes = res$error)
				if (!quiet) message("Timeline plot failed for '", date_col, "': ", res$error)
			}
		}
	}

	# ---- Histograms -------------------------------------------------------------
	hist_cfg <- config$histogram
	if (is.null(hist_cfg)) {
		hist_cfg <- list(enabled = length(suggested$numeric) > 0, fields = suggested$numeric)
	}
	if (isTRUE(hist_cfg$enabled)) {
		fields <- hist_cfg$fields %||% suggested$numeric
		fields <- intersect(as.character(fields), names(df))
		for (nm in fields) {
			file <- file.path(output_dir, paste0("hist_", sanitize_path_fragment(nm), ".png"))
			res <- save_plot_png(file, function() plot_numeric_distribution(flat, nm))
			if (isTRUE(res$ok)) {
				add_row("histogram", nm, res$file)
			} else {
				add_row("histogram", nm, notes = res$error)
				if (!quiet) message("Histogram failed for '", nm, "': ", res$error)
			}
		}
	}

	# ---- Bar charts -------------------------------------------------------------
	bar_cfg <- config$bar
	if (is.null(bar_cfg)) {
		bar_cfg <- list(enabled = length(suggested$categorical) > 0, fields = suggested$categorical, top_n = 10, horiz = TRUE)
	}
	if (isTRUE(bar_cfg$enabled)) {
		fields <- bar_cfg$fields %||% suggested$categorical
		fields <- intersect(as.character(fields), names(df))
		top_n <- bar_cfg$top_n %||% 10
		horiz <- isTRUE(bar_cfg$horiz)

		for (nm in fields) {
			file <- file.path(output_dir, paste0("bar_", sanitize_path_fragment(nm), ".png"))
			res <- save_plot_png(file, function() plot_categorical_summary(flat, nm, top_n = top_n, horiz = horiz))
			if (isTRUE(res$ok)) {
				add_row("bar", nm, res$file)
			} else {
				add_row("bar", nm, notes = res$error)
				if (!quiet) message("Bar plot failed for '", nm, "': ", res$error)
			}
		}
	}

	# ---- Wordcloud (optional) ---------------------------------------------------
	wc_cfg <- config$wordcloud
	if (is.null(wc_cfg)) {
		wc_cfg <- list(enabled = !is.null(suggested$text), field = suggested$text)
	}
	if (isTRUE(wc_cfg$enabled)) {
		if (!requireNamespace("wordcloud", quietly = TRUE)) {
			add_row("wordcloud", wc_cfg$field %||% NA_character_, notes = "Package 'wordcloud' not installed.")
		} else {
			field <- wc_cfg$field %||% suggested$text
			if (!is.null(field) && nzchar(field) && field %in% names(df)) {
				file <- file.path(output_dir, paste0("wordcloud_", sanitize_path_fragment(field), ".png"))
				res <- save_plot_png(file, function() plot_word_cloud(flat, field))
				if (isTRUE(res$ok)) {
					add_row("wordcloud", field, res$file)
				} else {
					add_row("wordcloud", field, notes = res$error)
					if (!quiet) message("Wordcloud failed for '", field, "': ", res$error)
				}
			}
		}
	}

	manifest <- if (length(rows) == 0) {
		data.frame(plot_type = character(), field = character(), file = character(), notes = character(), stringsAsFactors = FALSE)
	} else {
		do.call(rbind, rows)
	}

	list(manifest = manifest, files = files)
}


suggest_workflow_plots <- function(flat, id_col) {
	df <- if (is.list(flat) && !is.null(flat$FlatResponses) && inherits(flat$FlatResponses, "data.frame")) {
		flat$FlatResponses
	} else if (inherits(flat, "data.frame")) {
		flat
	} else {
		return(list(date_col = NULL, numeric = character(0), categorical = character(0), text = NULL, df_names = character(0)))
	}

	df <- as.data.frame(df, stringsAsFactors = FALSE)
	exclude <- unique(stats::na.omit(c(as.character(id_col), guess_time_col(names(df)))))
	out <- suggest_workflow_plots_from_df(df, exclude = exclude)
	out$df_names <- names(df)
	out
}


suggest_workflow_plots_from_df <- function(df, exclude = character(0)) {
	df <- as.data.frame(df, stringsAsFactors = FALSE)
	if (nrow(df) == 0 || ncol(df) == 0) {
		return(list(date_col = NULL, numeric = character(0), categorical = character(0), text = NULL))
	}

	date_col <- guess_time_col(names(df))

	numeric_candidates <- setdiff(names(df), exclude)
	numeric_candidates <- numeric_candidates[vapply(numeric_candidates, function(nm) {
		x <- df[[nm]]
		if (inherits(x, "Date") || inherits(x, "POSIXt") || is.logical(x)) return(FALSE)
		is.numeric(x) || is_numeric_like(x)
	}, logical(1))]
	numeric_candidates <- head(numeric_candidates, 3)

	cat_candidates <- setdiff(names(df), exclude)
	cat_candidates <- cat_candidates[vapply(cat_candidates, function(nm) {
		x <- df[[nm]]
		if (inherits(x, "Date") || inherits(x, "POSIXt")) return(FALSE)
		is.character(x) || is.factor(x) || is.logical(x)
	}, logical(1))]
	cat_candidates <- cat_candidates[!vapply(cat_candidates, function(nm) is_numeric_like(df[[nm]]), logical(1))]

	distinct <- vapply(cat_candidates, function(nm) {
		vals <- df[[nm]]
		vals <- vals[!is.na(vals)]
		length(unique(vals))
	}, integer(1))
	cat_candidates <- cat_candidates[distinct >= 2 & distinct <= 20]
	cat_candidates <- cat_candidates[order(distinct[cat_candidates])]
	cat_candidates <- head(cat_candidates, 3)

	text_candidates <- setdiff(names(df), exclude)
	text_candidates <- text_candidates[vapply(text_candidates, function(nm) is.character(df[[nm]]), logical(1))]

	best_text <- NULL
	if (length(text_candidates) > 0) {
		text_score <- vapply(text_candidates, function(nm) {
			vals <- df[[nm]]
			vals <- as.character(vals[!is.na(vals)])
			if (length(vals) == 0) return(0)
			stats::median(nchar(vals), na.rm = TRUE)
		}, numeric(1))
		best <- names(sort(text_score, decreasing = TRUE))[1]
		if (!is.na(best) && text_score[[best]] >= 20) best_text <- best
	}

	list(
		date_col = if (!is.null(date_col) && date_col %in% names(df)) date_col else NULL,
		numeric = numeric_candidates,
		categorical = cat_candidates,
		text = best_text
	)
}


parse_column_list <- function(x, df_names) {
	x <- trimws(x)
	if (!nzchar(x)) return(character(0))

	parts <- unlist(strsplit(x, "[,;]+"))
	parts <- trimws(parts)
	parts <- parts[parts != ""]
	if (length(parts) == 0) return(character(0))

	if (all(grepl("^[0-9]+$", parts))) {
		idx <- suppressWarnings(as.integer(parts))
		idx <- idx[!is.na(idx) & idx >= 1 & idx <= length(df_names)]
		return(df_names[idx])
	}

	parts
}


# ---- Workflow Plan / Repeatable Sessions ---------------------------------------

init_workflow_wizard_state <- function(timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")) {
	wiz <- new.env(parent = emptyenv())
	wiz$mode <- "none" # "none" | "defaults" | "auto"
	wiz$prev_plan <- list()
	wiz$prev_answers <- list()
	wiz$answers <- list()
	wiz$meta <- list(
		version = 1L,
		created_at = as.character(Sys.time()),
		timestamp = timestamp
	)
	wiz$plan_files <- NULL
	wiz
}


maybe_load_previous_workflow_session <- function(ctx, search_dir = getwd()) {
	plans <- find_previous_workflow_plans(search_dir)
	if (length(plans) == 0) return(ctx)

	most_recent <- plans[[1]]
	label <- most_recent$form_label %||% basename(most_recent$dir)
	created <- most_recent$created_at %||% format(most_recent$mtime, "%Y-%m-%d %H:%M:%S")

	if (!ctx$quiet) {
		message("Found a previous workflow session:")
		message(" - Form: ", label)
		message(" - Run date: ", created)
		message(" - Folder: ", most_recent$dir)
	}

	load_prev <- ask_yes_no("Load actions from the previous session?", default = TRUE)
	if (!isTRUE(load_prev)) return(ctx)

	chosen <- most_recent
	if (length(plans) > 1) {
		if (!ctx$quiet) {
			message("\nAvailable sessions (most recent first):")
			for (i in seq_along(plans)) {
				p <- plans[[i]]
				lbl <- p$form_label %||% basename(p$dir)
				when <- p$created_at %||% format(p$mtime, "%Y-%m-%d %H:%M:%S")
				message(" ", i, ") ", lbl, " - ", when, " - ", p$dir)
			}
		}
		idx <- suppressWarnings(as.integer(ask_text("Which session number", default = "1")))
		if (!is.na(idx) && idx >= 1 && idx <= length(plans)) chosen <- plans[[idx]]
	}

	mode_raw <- ask_text("Use previous actions as: 1 = apply automatically, 2 = defaults at prompts", default = "2")
	mode <- if (trimws(mode_raw) == "1") "auto" else "defaults"

	ctx$wizard$mode <- mode
	ctx$wizard$prev_plan <- chosen$plan %||% list()
	ctx$wizard$prev_answers <- ctx$wizard$prev_plan$answers %||% list()
	ctx$wizard$meta$source_plan_file <- chosen$file

	# Optional: reuse form_id/base_url if caller didn't supply.
	if (is.null(ctx$form_id) && !is.null(ctx$wizard$prev_plan$form_id) && nzchar(ctx$wizard$prev_plan$form_id)) {
		ctx$form_id <- ctx$wizard$prev_plan$form_id
	}
	if (is.null(ctx$base_url) && !is.null(ctx$wizard$prev_plan$base_url) && nzchar(ctx$wizard$prev_plan$base_url)) {
		ctx$base_url <- ctx$wizard$prev_plan$base_url
	}

	if (!ctx$quiet) {
		message("Replay mode: ", if (mode == "auto") "apply automatically" else "use as defaults")
	}

	ctx
}


find_previous_workflow_plans <- function(search_dir = getwd()) {
	search_dir <- path.expand(search_dir)
	dirs <- c(search_dir, list.dirs(search_dir, recursive = FALSE, full.names = TRUE))
	dirs <- dirs[dir.exists(dirs)]
	dirs <- unique(dirs)

	candidates <- lapply(dirs, function(d) {
		rds <- file.path(d, "workflow_plan.rds")
		json <- file.path(d, "workflow_plan.json")
		if (file.exists(rds)) return(rds)
		if (file.exists(json)) return(json)
		NULL
	})
	candidates <- Filter(Negate(is.null), candidates)
	if (length(candidates) == 0) return(list())

	entries <- list()
	for (f in candidates) {
		info <- file.info(f)
		plan <- read_workflow_plan_file(f)
		if (is.null(plan) || !is.list(plan)) next

		entries[[length(entries) + 1]] <- list(
			file = f,
			dir = dirname(f),
			mtime = info$mtime,
			created_at = plan$created_at %||% NULL,
			form_label = plan$form_label %||% NULL,
			plan = plan
		)
	}

	if (length(entries) == 0) return(list())
	entries[order(vapply(entries, function(x) as.numeric(x$mtime), numeric(1)), decreasing = TRUE)]
}


read_workflow_plan_file <- function(file) {
	if (is.null(file) || !nzchar(file) || !file.exists(file)) return(NULL)

	ext <- tolower(tools::file_ext(file))
	if (ext == "rds") {
		return(tryCatch(readRDS(file), error = function(e) NULL))
	}

	if (ext == "json") {
		return(tryCatch(jsonlite::fromJSON(file, simplifyVector = FALSE), error = function(e) NULL))
	}

	NULL
}


wizard_set_plan_files <- function(ctx, output_dir) {
	if (is.null(output_dir) || !nzchar(output_dir)) return(ctx)

	paths <- list(
		rds = file.path(output_dir, "workflow_plan.rds"),
		json = file.path(output_dir, "workflow_plan.json")
	)
	ctx$wizard$plan_files <- paths
	ctx$files$workflow_plan_rds <- paths$rds
	ctx$files$workflow_plan_json <- paths$json

	ctx$wizard$meta$output_parent <- dirname(output_dir)
	ctx <- wizard_persist_plan(ctx)

	if (!ctx$quiet) {
		message("Workflow plan will be saved to:")
		message(" - ", paths$rds)
		message(" - ", paths$json)
	}

	ctx
}


wizard_finalize <- function(ctx) {
	ctx$wizard$meta$completed_at <- as.character(Sys.time())
	ctx$wizard$meta$id_col_final <- ctx$id_col %||% NULL
	ctx$wizard$meta$output_dir <- ctx$output_dir %||% NULL

	if (!is.null(ctx$files$export)) ctx$wizard$meta$export <- ctx$files$export
	if (is_audit_log_active() && !is.null(ctx$audit_file) && file.exists(ctx$audit_file)) {
		ctx$files$audit_log <- ctx$audit_file
	}

	wizard_persist_plan(ctx)
}


workflow_build_plan <- function(ctx) {
	plan <- list(
		version = ctx$wizard$meta$version %||% 1L,
		created_at = ctx$wizard$meta$created_at %||% NULL,
		completed_at = ctx$wizard$meta$completed_at %||% NULL,
		form_label = ctx$wizard$meta$form_label %||% ctx$form_meta$title %||% ctx$form_meta$name %||% NULL,
		base_url = ctx$base_url %||% NULL,
		form_id = ctx$form_id %||% NULL,
		output_dir = ctx$output_dir %||% NULL,
		output_parent = ctx$wizard$meta$output_parent %||% (if (!is.null(ctx$output_dir)) dirname(ctx$output_dir) else NULL),
		audit_file = ctx$audit_file %||% NULL,
		id_col = ctx$id_col %||% NULL,
		files = ctx$files %||% list(),
		answers = ctx$wizard$answers %||% list()
	)

	# Include minimal replay info (helps troubleshooting).
	if (!is.null(ctx$wizard$meta$source_plan_file)) {
		plan$source_plan_file <- ctx$wizard$meta$source_plan_file
		plan$replay_mode <- ctx$wizard$mode
	}

	plan
}


wizard_persist_plan <- function(ctx) {
	paths <- ctx$wizard$plan_files
	if (is.null(paths) || !is.list(paths)) return(ctx)

	plan <- workflow_build_plan(ctx)

	if (!is.null(paths$rds) && nzchar(paths$rds)) {
		tryCatch(saveRDS(plan, paths$rds), error = function(e) NULL)
	}
	if (!is.null(paths$json) && nzchar(paths$json)) {
		tryCatch(
			jsonlite::write_json(plan, paths$json, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
			error = function(e) NULL
		)
	}

	ctx
}


wiz_prev_value <- function(ctx, key) {
	if (is.null(ctx$wizard) || is.null(key) || !nzchar(key)) return(NULL)
	ctx$wizard$prev_answers[[key]] %||% NULL
}


wiz_record <- function(ctx, key, value) {
	if (is.null(ctx$wizard) || is.null(key) || !nzchar(key)) return(ctx)
	ctx$wizard$answers[[key]] <- value
	wizard_persist_plan(ctx)
}


wiz_yes_no <- function(ctx, key, prompt, default = TRUE) {
	prev <- wiz_prev_value(ctx, key)
	mode <- ctx$wizard$mode %||% "none"

	if (identical(mode, "auto")) {
		val <- if (!is.null(prev)) isTRUE(prev) else isTRUE(default)
		ctx <- wiz_record(ctx, key, val)
		return(val)
	}

	def <- isTRUE(default)
	if (identical(mode, "defaults") && !is.null(prev)) def <- isTRUE(prev)
	val <- ask_yes_no(prompt, default = def)
	ctx <- wiz_record(ctx, key, val)
	val
}


wiz_text <- function(ctx, key, prompt, default = "") {
	prev <- wiz_prev_value(ctx, key)
	mode <- ctx$wizard$mode %||% "none"

	prev_chr <- NULL
	if (!is.null(prev)) prev_chr <- as.character(prev)[1]

	if (identical(mode, "auto")) {
		val <- if (!is.null(prev_chr) && nzchar(prev_chr)) prev_chr else default
		ctx <- wiz_record(ctx, key, val)
		return(val)
	}

	def <- default
	if (identical(mode, "defaults") && !is.null(prev_chr) && nzchar(prev_chr)) def <- prev_chr
	val <- ask_text(prompt, default = def)
	ctx <- wiz_record(ctx, key, val)
	val
}
