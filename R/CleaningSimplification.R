#' Normalize column names into a clean, readable format
#'
#' Designed for non-technical users who want consistent column names after
#' downloading FormIO submissions. Works with either a raw data frame or the
#' list returned by [flatten_submission_records()].
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param style One of `"snake"` (default), `"lower"`, `"upper"`, or `"title"`.
#'   Controls casing and separator behavior.
#' @param make_unique Logical. If `TRUE`, make duplicated names unique.
#' @param transliterate Logical. If `TRUE`, convert accents/special characters
#'   to ASCII when possible.
#' @param return_flat Logical. If `TRUE` and `x` came from [flatten_submission_records()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Data frame with normalized column names (the cleaned data)}
#'   \item{name_map}{Data frame with old and new column names}
#'   \item{flat}{If `return_flat = TRUE`, the updated flatten_submission_records-style list (for workflows that expect `FlatResponses`)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc"))
#' norm <- standardize_column_names(flat)
#' names(norm$data)
#' norm$name_map
#' }
standardize_column_names <- function(
		x,
		style = c("snake", "lower", "upper", "title"),
		make_unique = TRUE,
		transliterate = TRUE,
		return_flat = FALSE,
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	style <- match.arg(style) # Validate the style option.

	df <- extract_flat_df(x) # Normalize input to a flat data frame.
	old_names <- names(df) # Capture original column names.

	new_names <- normalize_names( # Apply normalization rules.
		old_names,
		style = style,
		make_unique = make_unique,
		transliterate = transliterate
	)

	names(df) <- new_names # Update column names in the data.

	name_map <- data.frame( # Build a mapping of old to new names.
		OldName = old_names,
		NewName = new_names,
		stringsAsFactors = FALSE
	)

	out <- list( # Package outputs.
		data = df,
		name_map = name_map
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- df # Update the flattened data.
		x$ColumnNames <- update_column_names(x$ColumnNames, new_names) # Keep metadata in sync.
		out$flat <- x # Attach the updated flat list.
	}

	if (!quiet) {
		changed <- sum(old_names != new_names) # Count renamed columns.
		message("Normalized ", changed, " column name(s).")
	}

	if (audit_depth == 1) {
		maybe_write_audit("standardize_column_names", data = out$data) # Record normalization action.
	}

	out # Return the results.
}


#' Resolve repeated answers into one row per submission
#'
#' Automatically collapses repeated values within each submission ID using a
#' consistent strategy (or simple heuristics when `strategy = "auto"`).
#' This is a non-interactive alternative to [resolve_duplicate_values()].
#'
#' Note: some FormIO components (for example uploads or address blocks) can
#' produce list-columns or nested data-frame columns even after flattening.
#' These values are converted to readable JSON/text before collapsing so the
#' output remains stable and export-friendly.
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param id_col Integer or character. Submission ID column (default 1).
#' @param strategy One of `"auto"`, `"concat"`, `"first"`, `"last"`, `"sum"`,
#'   `"mean"`, `"count"`, or `"count_yes"`.
#' @param sep Separator used for concatenation (default `", "`).
#' @param unique Logical. If `TRUE`, remove duplicate values before concatenating.
#' @param return_flat Logical. If `TRUE` and `x` came from [flatten_submission_records()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Collapsed data frame with one row per submission (the cleaned data)}
#'   \item{summary}{Data frame describing how each column was handled}
#'   \item{flat}{If `return_flat = TRUE`, the updated flatten_submission_records-style list (for workflows that expect `FlatResponses`)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc"))
#' resolved <- collapse_repeated_values(flat, id_col = "submissionId")
#' head(resolved$data)
#' }
collapse_repeated_values <- function(
		x,
		id_col = 1,
		strategy = c("auto", "concat", "first", "last", "sum", "mean", "count", "count_yes"),
		sep = ", ",
		unique = TRUE,
		return_flat = FALSE,
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	strategy <- match.arg(strategy) # Validate the strategy option.

	df <- extract_flat_df(x) # Normalize input to a flat data frame.
	id_col_name <- resolve_id_col(df, id_col) # Resolve the submission ID column.

	ids <- df[[id_col_name]] # Extract submission IDs.
	unique_ids <- ids[!duplicated(ids)] # Preserve original order of IDs.

	out_df <- data.frame( # Initialize output with unique IDs.
		tmp_id = unique_ids,
		stringsAsFactors = FALSE
	)

	summary <- data.frame( # Track strategy decisions per column.
		column = character(),
		max_distinct = integer(),
		applied_strategy = character(),
		stringsAsFactors = FALSE
	)

	for (col_name in names(df)) {
		values <- df[[col_name]] # Extract column values.
		values <- coerce_repeat_values(values) # Normalize list/data-frame columns.
		split_vals <- split(values, ids, drop = TRUE) # Group by submission ID.

		max_distinct <- max(vapply(split_vals, function(v) length(unique(v[!is.na(v)])), 1L)) # Distinct count per ID.

		if (col_name == id_col_name) {
			out_df[[col_name]] <- unique_ids # Preserve IDs as-is.
			next
		}

		if (max_distinct <= 1) {
			out_df[[col_name]] <- vapply( # Use the first non-missing value.
				split_vals,
				pick_first_non_na,
				na_value_for(values)
			)
			applied <- "first" # Strategy for single-value fields.
		} else {
			applied <- if (strategy == "auto") auto_strategy(values) else strategy # Choose strategy.
			out_df[[col_name]] <- vapply( # Apply the selected strategy.
				split_vals,
				apply_repeat_strategy,
				strategy_fun_value(applied, values),
				strategy = applied,
				sep = sep,
				unique = unique
			)
		}

		summary <- rbind( # Record column-level decisions.
			summary,
			data.frame(
				column = col_name,
				max_distinct = as.integer(max_distinct),
				applied_strategy = applied,
				stringsAsFactors = FALSE
			)
		)
	}

	out_df <- out_df[, names(df), drop = FALSE] # Restore original column order.

	out <- list( # Package outputs.
		data = out_df,
		summary = summary
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- out_df # Update the flattened data.
		x$ColumnNames <- update_column_names(x$ColumnNames, names(out_df)) # Keep metadata in sync.
		out$flat <- x # Attach the updated flat list.
	}

	if (!quiet) {
		message("Resolved repeats across ", nrow(summary), " column(s).")
	}

	if (audit_depth == 1) {
		maybe_write_audit("collapse_repeated_values", data = out$data) # Record repeat resolution.
	}

	out # Return results.
}


#' Deduplicate submissions by submission ID
#'
#' Keeps one row per submission ID, using a timestamp column when available
#' (for example `created` or `modified`), otherwise keeps first/last row.
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param id_col Integer or character. Submission ID column (default 1).
#' @param time_col Optional column name to use for ordering. If `NULL`, the
#'   function tries common timestamp names automatically.
#' @param keep One of `"last"` (default) or `"first"`.
#' @param return_flat Logical. If `TRUE` and `x` came from [flatten_submission_records()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Deduplicated data frame (the cleaned data)}
#'   \item{summary}{List with counts and the time column used}
#'   \item{flat}{If `return_flat = TRUE`, the updated flatten_submission_records-style list (for workflows that expect `FlatResponses`)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc"))
#' dedup <- deduplicate_submission_rows(flat, id_col = "submissionId")
#' nrow(dedup$data)
#' }
deduplicate_submission_rows <- function(
		x,
		id_col = 1,
		time_col = NULL,
		keep = c("last", "first"),
		return_flat = FALSE,
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	keep <- match.arg(keep) # Validate keep option.

	df <- extract_flat_df(x) # Normalize input to a flat data frame.
	id_col_name <- resolve_id_col(df, id_col) # Resolve the submission ID column.

	if (is.null(time_col)) {
		time_col <- guess_time_col(names(df)) # Attempt to guess a timestamp column.
	}
	if (!is.null(time_col) && !time_col %in% names(df)) {
		time_col <- NULL # Ignore invalid time column names.
	}

	selected_idx <- pick_dedup_rows( # Compute which rows to keep.
		df = df,
		id_col = id_col_name,
		time_col = time_col,
		keep = keep
	)

	out_df <- df[selected_idx, , drop = FALSE] # Subset to the selected rows.

	out <- list( # Package outputs.
		data = out_df,
		summary = list(
			original_rows = nrow(df),
			kept_rows = nrow(out_df),
			removed_rows = nrow(df) - nrow(out_df),
			time_col = time_col %||% NA
		)
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- out_df # Update the flattened data.
		x$ColumnNames <- update_column_names(x$ColumnNames, names(out_df)) # Keep metadata in sync.
		out$flat <- x # Attach the updated flat list.
	}

	if (!quiet) {
		message(
			"Kept ", out$summary$kept_rows,
			" of ", out$summary$original_rows, " rows."
		)
	}

	if (audit_depth == 1) {
		maybe_write_audit("deduplicate_submission_rows", data = out$data) # Record deduplication.
	}

	out # Return results.
}


#' Interactively review and resolve duplicate submissions
#'
#' This function walks you through duplicate groups defined by key columns
#' (for example `email`, `username`, `project_id`). For each duplicate group,
#' it shows selected columns so you can compare submissions and choose which
#' ones to keep. You can keep multiple submissions or drop all submissions
#' for a given group if needed.
#'
#' This is designed for non-technical users who want full control over which
#' duplicates are kept. If you want an automatic (non-interactive) approach,
#' use [deduplicate_submission_rows()].
#'
#' If audit logging is active (see [start_audit_log()]), each decision is logged.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param id_col Integer or character. Submission ID column (default 1).
#' @param key_cols Optional. Columns used to define "duplicate submissions"
#'   (for example `username`, `email`, `project_id`). If `NULL`, you will be
#'   prompted to choose columns interactively (unless `prompt = FALSE`).
#'   The default suggestions are the first three columns that do not start
#'   with `"form-"` or `"form_"`.
#'   Tip: choose columns that are stable within a submission (not multi-row fields).
#' @param compare_cols Optional. Character or integer vector of columns to show
#'   when comparing duplicates. If `NULL` (default), you will be prompted to
#'   choose columns interactively (unless `prompt = FALSE`).
#' @param keep_map Optional. Non-interactive decisions for which rows to keep,
#'   keyed by submission ID. Supported formats:
#'   - Named list: `list(id1 = c(1,3), id2 = "all")`
#'   - Named character vector: `c(id1 = "1,3", id2 = "all")`
#'   - Data frame with columns `id` and `keep`
#'   Values can be row numbers (within each duplicate group), ranges like `"1:3"`,
#'   or keywords `"all"` / `"none"`.
#' @param prompt Logical. If `TRUE` (default), ask interactively. If `FALSE`,
#'   uses `keep_map` and `default_keep` without prompting.
#' @param default_keep Default action when `prompt = FALSE` and no `keep_map`
#'   entry exists for a given ID. Accepts the same formats as `keep_map` values.
#'   Default `"all"`.
#' @param return_flat Logical. If `TRUE` and `x` came from [flatten_submission_records()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints guidance and summaries.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Data frame after duplicate review}
#'   \item{summary}{List with counts and whether the review stopped early}
#'   \item{decisions}{Data frame describing what was kept/dropped for each ID}
#'   \item{flat}{If `return_flat = TRUE` and `x` was a flatten_submission_records list, the updated list}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc"))
#' out <- review_duplicate_rows(flat, id_col = "form_submissionid")
#' View(out$decisions)
#'
#' # Non-interactive example (scriptable)
#' df <- data.frame(
#'   submissionId = c("a", "a", "b", "b", "b"),
#'   email = c("x@example.com", "x@example.com", "x@example.com", "x@example.com", "x@example.com"),
#'   status = c("draft", "final", "test", "final", "final"),
#'   stringsAsFactors = FALSE
#' )
#' # Group 1 will be the email=x@example.com group (the only group in this example)
#' keep_map <- list(`1` = 2)
#' out2 <- review_duplicate_rows(
#'   df,
#'   id_col = "submissionId",
#'   key_cols = "email",
#'   compare_cols = c("submissionId", "email", "status"),
#'   keep_map = keep_map,
#'   prompt = FALSE,
#'   quiet = TRUE
#' )
#' }
review_duplicate_rows <- function(
		x,
		id_col = 1,
		key_cols = NULL,
		compare_cols = NULL,
		return_flat = FALSE,
		quiet = FALSE,
		keep_map = NULL,
		prompt = TRUE,
		default_keep = "all"
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.

	df <- extract_flat_df(x) # Normalize input to a flat data frame.
	df <- as.data.frame(df, stringsAsFactors = FALSE) # Avoid factor coercion.
	id_col_name <- resolve_id_col(df, id_col) # Resolve the submission ID column.

	ids <- as.character(df[[id_col_name]]) # Extract submission IDs as strings.

	prompt <- isTRUE(prompt) # Normalize prompt flag.
	if (!interactive()) prompt <- FALSE # Disable prompting in non-interactive mode.

	key_cols <- pick_duplicate_key_columns(df, id_col_name, key_cols, quiet = quiet, prompt = prompt) # Select grouping keys.
	default_compare <- default_non_form_cols(names(df), id_col_name, n = 3) # Suggest columns to compare.
	compare_default <- unique(c(key_cols, default_compare))
	if (length(compare_default) == 0) compare_default <- id_col_name # Ensure at least ID column.
	compare_cols <- pick_compare_columns(
		df,
		id_col_name,
		compare_cols,
		quiet = quiet,
		prompt = prompt,
		default_cols = compare_default
	)

	if (is.null(key_cols) || length(key_cols) == 0) {
		stop("key_cols is required to identify duplicates. Provide key_cols or set prompt = TRUE.")
	}

	key_info <- build_duplicate_keys(df, key_cols) # Build key strings per row.
	key_id <- key_info$key_id
	group_keys <- key_info$group_keys

	group_order <- match(unique(key_id), key_id) # Preserve original order of groups.
	group_ids <- unique(key_id)

	dup_groups <- list() # Accumulate duplicate groups to review.
	for (i in seq_along(group_ids)) {
		rows_idx <- which(key_id == group_ids[i])
		if (length(rows_idx) == 0) next
		group_unique_ids <- unique(ids[rows_idx])
		group_unique_ids <- group_unique_ids[!is.na(group_unique_ids) & group_unique_ids != ""]
		if (length(group_unique_ids) >= 2) {
			dup_groups[[length(dup_groups) + 1]] <- list(
				group_id = length(dup_groups) + 1L,
				key_id = group_ids[i],
				group_key = group_keys[rows_idx[1]],
				rows_idx = rows_idx,
				submission_ids = group_unique_ids
			)
		}
	}

	if (length(dup_groups) == 0) {
		if (!quiet) message("No duplicate submissions detected for the selected key columns.")
		out <- list( # Return early with no changes.
			data = df,
			summary = list(
				original_rows = nrow(df),
				kept_rows = nrow(df),
				removed_rows = 0L,
				duplicate_groups = 0L,
				processed_groups = 0L,
				stopped_early = FALSE
			),
			decisions = data.frame(
				group_id = integer(),
				group_key = character(),
				rows_total = integer(),
				submission_ids = character(),
				kept_ids = character(),
				dropped_ids = character(),
				kept_n = integer(),
				dropped_n = integer(),
				decision = character(),
				stringsAsFactors = FALSE
			)
		)
		if (audit_depth == 1) {
			maybe_write_audit("review_duplicate_rows", details = "no duplicates", data = df) # Record no-op review.
		}
		return(out)
	}

	keep_map <- normalize_keep_map(keep_map) # Normalize non-interactive keep decisions.
	if (!prompt && (is.null(keep_map) || length(keep_map) == 0)) {
		if (is.null(default_keep) || length(default_keep) == 0 || !nzchar(as.character(default_keep)[1])) {
			stop("Non-interactive mode requires keep_map or a valid default_keep.")
		}
	}

	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.
	rows_keep <- rep(TRUE, nrow(df)) # Track which rows to keep.
	decision_rows <- list() # Collect per-group decisions.
	stopped <- FALSE # Track early exit from review.

	if (!quiet) {
		message("Duplicate review: you will see each duplicate group and choose which submissions to keep.")
	}

	for (grp in dup_groups) {
		rows_idx <- grp$rows_idx
		group_id <- grp$group_id
		group_key <- grp$group_key
		group_unique_ids <- grp$submission_ids

		if (length(group_unique_ids) <= 1) next

		if (!quiet) {
			message("Duplicate group ", group_id, ": ", group_key, " (", length(group_unique_ids), " submission IDs)")
			highlight_cols <- setdiff(compare_cols, id_col_name)
			display_df <- build_compare_frame_by_id(
				df,
				rows_idx,
				ids,
				group_unique_ids,
				compare_cols,
				highlight_cols = highlight_cols
			)
			print_table_left(display_df, quiet = quiet)
		}

		keep_local <- NULL
		if (!is.null(keep_map)) {
			keep_local <- resolve_keep_for_group(keep_map, group_id = group_id, group_key = group_key, n_rows = length(group_unique_ids)) # Use non-interactive choice.
		}
		if (is.null(keep_local)) {
			if (prompt) {
				keep_local <- ask_keep_rows(length(group_unique_ids)) # Ask the user which rows to keep.
			} else {
				keep_local <- parse_keep_choice(default_keep, length(group_unique_ids)) # Use default non-interactive choice.
			}
		}

		if (identical(keep_local, "quit")) {
			stopped <- TRUE
			if (!quiet) message("Stopping duplicate review early. Rows not yet reviewed will be kept.")
			break
		}

		keep_local <- sort(unique(keep_local)) # Normalize keep indices.
		keep_local <- keep_local[keep_local >= 1 & keep_local <= length(group_unique_ids)]
		drop_local <- setdiff(seq_along(group_unique_ids), keep_local) # Compute drop indices.

		kept_ids <- group_unique_ids[keep_local]
		dropped_ids <- group_unique_ids[drop_local]

		if (length(dropped_ids) > 0) {
			rows_keep[ids %in% dropped_ids] <- FALSE # Mark rows for removal.
		}

		decision_rows[[length(decision_rows) + 1]] <- data.frame(
			group_id = as.integer(group_id),
			group_key = as.character(group_key),
			rows_total = length(rows_idx),
			submission_ids = paste(group_unique_ids, collapse = ", "),
			kept_ids = if (length(kept_ids) > 0) paste(kept_ids, collapse = ", ") else "",
			dropped_ids = if (length(dropped_ids) > 0) paste(dropped_ids, collapse = ", ") else "",
			kept_n = length(kept_ids),
			dropped_n = length(dropped_ids),
			decision = if (length(dropped_ids) == 0) "kept_all" else if (length(kept_ids) == 0) "dropped_all" else "kept_subset",
			stringsAsFactors = FALSE
		)

		if (audit_depth == 1) {
			detail <- paste0(
				"Group ", group_id, " (", group_key, "): kept ",
				length(kept_ids), " of ", length(group_unique_ids),
				if (length(dropped_ids) > 0) paste0(" (dropped IDs ", paste(dropped_ids, collapse = ", "), ")") else ""
			)
			maybe_write_audit("review_duplicate_rows", details = detail, data = df[rows_idx, , drop = FALSE]) # Record per-group decision.
		}
	}

	out_df <- df[rows_keep, , drop = FALSE] # Apply keep/drop decisions.
	decisions <- if (length(decision_rows) == 0) {
		data.frame(
			group_id = integer(),
			group_key = character(),
			rows_total = integer(),
			submission_ids = character(),
			kept_ids = character(),
			dropped_ids = character(),
			kept_n = integer(),
			dropped_n = integer(),
			decision = character(),
			stringsAsFactors = FALSE
		)
	} else {
		do.call(rbind, decision_rows)
	}

	out <- list( # Package outputs.
		data = out_df,
		summary = list(
			original_rows = nrow(df),
			kept_rows = nrow(out_df),
			removed_rows = nrow(df) - nrow(out_df),
			duplicate_groups = length(dup_groups),
			processed_groups = nrow(decisions),
			stopped_early = stopped
		),
		decisions = decisions
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- out_df # Update the flattened data.
		x$ColumnNames <- update_column_names(x$ColumnNames, names(out_df)) # Keep metadata in sync.
		out$flat <- x # Attach the updated flat list.
	}

	if (!quiet) {
		message("Removed ", out$summary$removed_rows, " row(s) across ", out$summary$processed_groups, " duplicate group(s).")
	}

	out # Return results.
}


#' Compact checkbox/multi-select columns into a single readable column
#'
#' When a question generates multiple TRUE/FALSE columns (e.g., select boxes),
#' this function combines them into a single comma-separated column.
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param sep Separator used in column names to split prefix and option.
#'   Default `"-"` (matches [flatten_submission_records()] naming).
#' @param combine_sep Separator used between selected options.
#' @param drop Logical. If `TRUE`, drop the original checkbox columns.
#' @param keep_empty Logical. If `TRUE`, keep empty strings instead of `NA`
#'   when no options are selected.
#' @param yes_values Values that should count as "selected".
#' @param no_values Values that should count as "not selected" (defaults include
#'   `FALSE`, `"No"`, `0`, and blank strings).
#' @param return_flat Logical. If `TRUE` and `x` came from [flatten_submission_records()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Data frame with compacted selection columns (the cleaned data)}
#'   \item{summary}{Data frame describing which columns were compacted}
#'   \item{flat}{If `return_flat = TRUE`, the updated flatten_submission_records-style list (for workflows that expect `FlatResponses`)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' compacted <- collapse_checkbox_selections(flat)
#' head(compacted$data)
#' }
collapse_checkbox_selections <- function(
		x,
		sep = "-",
		combine_sep = ", ",
		drop = TRUE,
		keep_empty = FALSE,
		yes_values = c(TRUE, "TRUE", "True", "true", "Yes", "YES", "yes", "Y", "y", 1, "1"),
		no_values = c(FALSE, "FALSE", "False", "false", "No", "NO", "no", "N", "n", 0, "0", "", " ",  "NA"),
		return_flat = FALSE,
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	df <- extract_flat_df(x) # Normalize input to a flat data frame.
	col_names <- names(df) # Capture column names for grouping.

	group_keys <- derive_prefix(col_names, sep = sep) # Identify shared prefixes.
	groups <- split(col_names, group_keys, drop = TRUE) # Group columns by prefix.
	groups <- groups[!is.na(names(groups))] # Drop columns without a recognized prefix.

	summary <- data.frame( # Track which groups were compacted.
		prefix = character(),
		new_column = character(),
		original_columns = character(),
		stringsAsFactors = FALSE
	)

	new_df <- df # Work on a copy of the data.

	for (prefix in names(groups)) {
		group_cols <- groups[[prefix]]
		if (length(group_cols) < 2) next

		is_checkbox_group <- vapply( # Ensure all columns look like checkbox values.
			group_cols,
			function(col_nm) {
				is_checkbox_like(
					df[[col_nm]],
					values_df = df,
					yes_values = yes_values,
					no_values = no_values
				)
			},
			logical(1)
		)
		if (!all(is_checkbox_group)) {
			next
		}

		options <- vapply(group_cols, option_suffix, character(1), sep = sep) # Derive option labels.

		new_col <- paste0(prefix, sep, "selected") # Default name for the compacted column.
		if (new_col %in% names(new_df)) {
			new_col <- make.unique(c(names(new_df), new_col))[length(names(new_df)) + 1] # Ensure uniqueness.
		}

		selected <- apply( # Build combined selection values per row.
			new_df[, group_cols, drop = FALSE],
			1,
			function(row_vals) {
				row_vals <- normalize_checkbox_values(row_vals)
				keep <- row_vals %in% normalize_checkbox_values(yes_values)
				if (!any(keep)) {
					if (keep_empty) "" else NA_character_
				} else {
					paste(options[keep], collapse = combine_sep)
				}
			}
		)

		new_df[[new_col]] <- selected # Append the compacted column.

		summary <- rbind( # Record the compaction summary.
			summary,
			data.frame(
				prefix = prefix,
				new_column = new_col,
				original_columns = paste(group_cols, collapse = ", "),
				stringsAsFactors = FALSE
			)
		)

		if (drop) {
			new_df <- new_df[, setdiff(names(new_df), group_cols), drop = FALSE] # Drop original columns.
		}
	}

	out <- list( # Package outputs.
		data = new_df,
		summary = summary
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- new_df # Update the flattened data.
		x$ColumnNames <- update_column_names(x$ColumnNames, names(new_df)) # Keep metadata in sync.
		out$flat <- x # Attach the updated flat list.
	}

	if (!quiet) {
		message("Compacted ", nrow(summary), " selection group(s).")
	}

	if (audit_depth == 1) {
		maybe_write_audit("collapse_checkbox_selections", data = out$data) # Record compaction action.
	}

	out # Return results.
}


# ---- Internal helpers -----------------------------------------------------------

#' Check whether input is a flatten_submission_records list
#'
#' @param x Object to test.
#' @return `TRUE` if `x` is a list with `FlatResponses`, otherwise `FALSE`.
#' @keywords internal
is_flat_list <- function(x) {
	is.list(x) && "FlatResponses" %in% names(x)
}

#' Extract a flat data frame
#'
#' @param x Data frame or flatten_submission_records list.
#' @return A data frame of responses.
#' @keywords internal
extract_flat_df <- function(x) {
	if (is_flat_list(x)) return(x$FlatResponses)
	if (!is.data.frame(x)) stop("x must be a data.frame or output from flatten_submission_records()")
	x
}

#' Resolve submission ID column name
#'
#' @param df Data frame of responses.
#' @param id_col Column name or index.
#' @return Valid column name for submission IDs.
#' @keywords internal
resolve_id_col <- function(df, id_col) {
	if (is.numeric(id_col)) {
		id_col_name <- names(df)[id_col] # Translate numeric index to column name.
	} else {
		id_col_name <- as.character(id_col) # Treat input as a column name.
	}
	if (is.na(id_col_name) || !id_col_name %in% names(df)) {
		stop("id_col '", id_col, "' not found in data")
	}
	id_col_name # Return validated column name.
}

#' Normalize column names
#'
#' @param names_vec Character vector of column names.
#' @param style Naming style ("snake", "lower", "upper", "title").
#' @param make_unique Logical. Ensure unique names.
#' @param transliterate Logical. Convert accents to ASCII.
#' @return A character vector of normalized names.
#' @keywords internal
normalize_names <- function(names_vec, style, make_unique, transliterate) {
	out <- names_vec # Start from original names.
	out <- trimws(out) # Trim surrounding whitespace.
	if (transliterate) {
		out <- iconv(out, from = "", to = "ASCII//TRANSLIT") # Best-effort transliteration.
	}
	out <- gsub("[^A-Za-z0-9]+", "_", out) # Replace non-alphanumerics with underscores.
	out <- gsub("_+", "_", out) # Collapse multiple underscores.
	out <- gsub("^_+|_+$", "", out) # Trim leading/trailing underscores.

	if (style == "snake" || style == "lower") {
		out <- tolower(out) # Lowercase for snake/lower styles.
	}
	if (style == "upper") {
		out <- toupper(out) # Uppercase style.
	}
	if (style == "title") {
		out <- tools::toTitleCase(tolower(gsub("_", " ", out))) # Title-case words.
		out <- gsub(" ", "_", out) # Restore underscores.
	}

	out[out == ""] <- "col" # Replace empty names with a placeholder.
	if (make_unique) out <- make.unique(out, sep = "_") # Ensure uniqueness.
	out # Return normalized names.
}

#' Update stored column name metadata
#'
#' @param column_names_df Column metadata data.frame (optional).
#' @param new_names New column names.
#' @return Updated metadata data.frame (or unchanged input).
#' @keywords internal
update_column_names <- function(column_names_df, new_names) {
	if (is.null(column_names_df)) return(column_names_df)
	if (!is.data.frame(column_names_df)) return(column_names_df)

	# If columns were dropped/added, the old metadata row count no longer matches.
	# Rebuild a minimal, consistent mapping so downstream steps (like the wizard)
	# don't see recycled names.
	if (nrow(column_names_df) != length(new_names)) {
		if ("Name" %in% names(column_names_df)) {
			return(data.frame(
				Number = seq_along(new_names),
				Name = new_names,
				stringsAsFactors = FALSE
			))
		}
		if ("Names" %in% names(column_names_df)) {
			return(data.frame(
				Number = seq_along(new_names),
				Names = new_names,
				stringsAsFactors = FALSE
			))
		}
		# Unknown schema: keep as-is.
		return(column_names_df)
	}

	if ("Name" %in% names(column_names_df)) {
		column_names_df$Name <- new_names # Update "Name" column when present.
	}
	if ("Names" %in% names(column_names_df)) {
		column_names_df$Names <- new_names # Update "Names" column when present.
	}
	column_names_df # Return the updated metadata.
}

#' Parse index selections
#'
#' @param input Character string of indices/ranges.
#' @param max_val Maximum allowed index.
#' @return Integer vector of selected indices.
#' @keywords internal
parse_index_input <- function(input, max_val) {
	input <- trimws(input) # Normalize whitespace.
	if (!nzchar(input)) return(integer(0))

	# Normalize ranges like "1: 5" -> "1:5"
	input <- gsub("\\s*:\\s*", ":", input)
	parts <- unlist(strsplit(input, "[,;\\s]+"))
	parts <- trimws(parts)
	parts <- parts[parts != ""]
	if (length(parts) == 0) return(integer(0))

	result <- integer(0) # Accumulate parsed indices.
	for (p in parts) {
		if (grepl("^[0-9]+:[0-9]+$", p)) {
			rng <- strsplit(p, ":")[[1]]
			start <- suppressWarnings(as.integer(rng[1]))
			end <- suppressWarnings(as.integer(rng[2]))
			if (!is.na(start) && !is.na(end)) {
				if (start <= end) {
					result <- c(result, seq(start, end)) # Ascending range.
				} else {
					result <- c(result, seq(start, end, by = -1)) # Descending range.
				}
			}
		} else if (grepl("^[0-9]+$", p)) {
			val <- suppressWarnings(as.integer(p))
			if (!is.na(val)) result <- c(result, val) # Add single index.
		}
	}

	result <- unique(result) # Drop duplicates.
	result <- result[result >= 1 & result <= max_val] # Enforce bounds.
	sort(result) # Return sorted indices.
}

#' Suggest default non-FormIO columns
#'
#' @param df_names Column names.
#' @param id_col_name Submission ID column.
#' @param n Number of columns to return.
#' @return A character vector of suggested columns.
#' @keywords internal
default_non_form_cols <- function(df_names, id_col_name, n = 3) {
	non_form <- df_names[!grepl("^form[-_]", df_names)] # Prefer non-form-prefixed columns.
	non_form <- setdiff(non_form, id_col_name) # Exclude ID column.
	if (length(non_form) == 0) {
		non_form <- setdiff(df_names, id_col_name) # Fallback to any non-ID columns.
	}
	head(non_form, n) # Return the first N suggestions.
}

#' Determine whether to use colored output
#'
#' @return `TRUE` if color output is enabled and supported.
#' @keywords internal
formior_use_color <- function() {
	opt <- getOption("formior.color") # Allow explicit user override.
	if (!is.null(opt)) return(isTRUE(opt))
	if (!interactive()) return(FALSE)
	no_color <- Sys.getenv("NO_COLOR") # Respect standard NO_COLOR flag.
	if (nzchar(no_color)) return(FALSE)
	isTRUE(crayon::has_color()) # Use crayon detection.
}

#' Strip ANSI styling safely
#'
#' @param x Character input with optional styling.
#' @return Plain text without ANSI styling.
#' @keywords internal
strip_style_safe <- function(x) {
	if (is.null(x)) return("")
	x <- as.character(x) # Normalize to character.
	if (isTRUE(formior_use_color())) {
		return(crayon::strip_style(x)) # Remove ANSI styles when color is enabled.
	}
	x # Return unchanged when no color.
}

#' Compute display width
#'
#' @param x Character input.
#' @return Display width in characters.
#' @keywords internal
display_width <- function(x) {
	nchar(strip_style_safe(x), type = "width") # Use width-aware counts.
}

#' Pad a string to the right
#'
#' @param x Character input.
#' @param width Target display width.
#' @return Right-padded string.
#' @keywords internal
pad_right <- function(x, width) {
	x <- as.character(x) # Normalize to character.
	w <- display_width(x) # Current display width.
	if (is.na(w)) w <- 0
	if (w >= width) return(x)
	paste0(x, strrep(" ", width - w)) # Append spaces to reach target width.
}

#' Print a data frame for review
#'
#' @param df Data frame to print.
#' @param quiet Logical. Suppress printed output when `TRUE`.
#' @return Invisibly returns `NULL`.
#' @keywords internal
print_table_left <- function(df, quiet = FALSE) {
	if (!is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
	if (!quiet) {
		df <- as.data.frame(df, stringsAsFactors = FALSE) # Avoid factor coercion.
		print(df, row.names = FALSE)
	}
	invisible(NULL)
}

#' Print a column selection table
#'
#' @param df_names Column names to display.
#' @param right Logical. Reserved for layout (unused).
#' @param max_rows Maximum rows before splitting into two columns.
#' @param color_numbers Logical. Colorize the index numbers when possible.
#' @param quiet Logical. Suppress printed output when `TRUE`.
#' @return Invisibly returns `NULL`.
#' @keywords internal
print_column_selection_table <- function(df_names, right = FALSE, max_rows = 25, color_numbers = TRUE, quiet = FALSE) {
	df_names <- as.character(df_names) # Normalize to character.
	n <- length(df_names) # Count columns.
	if (n == 0) return(invisible(NULL))

	format_no <- function(x) {
		x <- as.character(x) # Normalize indices to character.
		x[is.na(x)] <- ""
		if (isTRUE(color_numbers) && isTRUE(formior_use_color())) {
			return(cyan(x)) # Colorize when enabled.
		}
		x # Return plain text otherwise.
	}

	if (n <= max_rows) {
		df <- data.frame(No = format_no(seq_along(df_names)), Column = df_names, stringsAsFactors = FALSE)
		print_table_left(df, quiet = quiet) # Print a single-column list.
		return(invisible(NULL))
	}

	n_rows <- ceiling(n / 2) # Split into two columns.
	idx1 <- seq_len(n_rows)
	idx2 <- seq(from = n_rows + 1, length.out = n_rows)
	idx2 <- idx2[idx2 <= n]

	df1 <- data.frame(No = format_no(idx1), Column = df_names[idx1], stringsAsFactors = FALSE)
	df2 <- data.frame(No = format_no(idx2), Column = df_names[idx2], stringsAsFactors = FALSE)

	if (nrow(df2) < nrow(df1)) {
		pad <- nrow(df1) - nrow(df2) # Pad the second column to match rows.
		df2 <- rbind(
			df2,
			data.frame(No = format_no(rep(NA_integer_, pad)), Column = rep("", pad), stringsAsFactors = FALSE)
		)
	}

	out <- data.frame( # Build a two-column display table.
		No = df1$No,
		Column = df1$Column,
		No = df2$No,
		Column = df2$Column,
		check.names = FALSE,
		stringsAsFactors = FALSE
	)

	print_table_left(out, quiet = quiet) # Print the combined table.
	invisible(NULL)
}

#' Normalize keep map inputs
#'
#' Accepts multiple keep_map formats and returns a named list keyed by
#' `group_id` or `group_key`.
#'
#' @param keep_map Keep decisions in list, named vector, or data.frame form.
#' @return A named list of keep choices, or `NULL`.
#' @keywords internal
normalize_keep_map <- function(keep_map) {
	if (is.null(keep_map)) return(NULL) # Nothing to normalize.

	if (is.list(keep_map) && !is.data.frame(keep_map)) {
		if (!is.null(names(keep_map))) return(keep_map) # Already a named list.
	}

	if (is.character(keep_map) && !is.null(names(keep_map))) {
		out <- as.list(keep_map) # Convert named vector to list.
		return(out)
	}

	if (inherits(keep_map, "data.frame")) {
		if (!("keep" %in% names(keep_map))) {
			stop("keep_map data.frame must include a 'keep' column.")
		}
		key_col <- NULL
		if ("group_key" %in% names(keep_map)) key_col <- "group_key"
		if (is.null(key_col) && "group_id" %in% names(keep_map)) key_col <- "group_id"
		if (is.null(key_col)) {
			stop("keep_map data.frame must include 'group_key' or 'group_id'.")
		}
		out <- keep_map$keep # Extract keep instructions.
		names(out) <- as.character(keep_map[[key_col]]) # Key by group ID or key.
		return(as.list(out)) # Return as list for consistency.
	}

	stop("keep_map must be a named list/vector keyed by group_id/group_key, or a data frame with 'group_id'/'group_key' and 'keep'.")
}

#' Parse keep selection choices
#'
#' @param choice Selection input (numeric, logical, or character).
#' @param n_rows Number of available rows.
#' @return Integer indices to keep, or `NULL`.
#' @keywords internal
parse_keep_choice <- function(choice, n_rows) {
	if (is.null(choice)) return(NULL) # Nothing to parse.

	if (is.logical(choice) && length(choice) == 1) {
		return(if (isTRUE(choice)) seq_len(n_rows) else integer(0)) # Logical shortcut.
	}

	if (is.numeric(choice)) {
		idx <- unique(as.integer(choice)) # Normalize numeric indices.
		idx <- idx[!is.na(idx) & idx >= 1 & idx <= n_rows]
		return(idx)
	}

	choice <- as.character(choice)
	if (length(choice) == 0) return(NULL)
	if (length(choice) == 1) {
		val <- tolower(trimws(choice))
		if (val %in% c("all", "a", "keep")) return(seq_len(n_rows)) # Keep all.
		if (val %in% c("none", "drop", "delete")) return(integer(0)) # Keep none.
	}

	input <- paste(choice, collapse = ",") # Combine into a single string.
	parse_index_input(input, n_rows)
}

#' Resolve keep choices for a duplicate group
#'
#' @param keep_map Normalized keep map.
#' @param group_id Numeric group ID.
#' @param group_key Character group key.
#' @param n_rows Number of rows in the group.
#' @return Integer indices to keep, or `NULL`.
#' @keywords internal
resolve_keep_for_group <- function(keep_map, group_id, group_key, n_rows) {
	if (is.null(keep_map) || length(keep_map) == 0) return(NULL)
	if (!is.null(names(keep_map))) {
		if (!is.null(group_key) && group_key %in% names(keep_map)) {
			return(parse_keep_choice(keep_map[[group_key]], n_rows))
		}
		if (as.character(group_id) %in% names(keep_map)) {
			return(parse_keep_choice(keep_map[[as.character(group_id)]], n_rows))
		}
	}
	NULL
}

#' Pick duplicate key columns
#'
#' @param df Data frame of responses.
#' @param id_col_name Submission ID column.
#' @param key_cols Optional pre-selected key columns.
#' @param quiet Logical. Suppress messages.
#' @param prompt Logical. Allow interactive prompting.
#' @return Character vector of key column names.
#' @keywords internal
pick_duplicate_key_columns <- function(df, id_col_name, key_cols, quiet = FALSE, prompt = TRUE) {
	df_names <- names(df)
	if (!is.null(key_cols)) {
		if (is.numeric(key_cols)) {
			key_cols <- df_names[key_cols]
		}
		key_cols <- unique(as.character(key_cols))
		key_cols <- key_cols[key_cols %in% df_names]
		key_cols <- setdiff(key_cols, id_col_name)
		if (length(key_cols) > 0) return(key_cols)
	}

	if (!interactive() || !prompt) {
		return(character(0)) # Non-interactive mode returns no key columns.
	}

	default_cols <- default_non_form_cols(df_names, id_col_name, n = 3)
	default_label <- if (length(default_cols) > 0) paste(default_cols, collapse = ", ") else "(none)"

	if (!quiet) {
		message("Choose columns that define a duplicate submission (e.g., username, email, project ID).")
		print_column_selection_table(df_names, right = FALSE, max_rows = 25, quiet = quiet)
		if (length(default_cols) > 0) {
			message("Suggested default columns: ", default_label)
		}
	}

	prompt_txt <- paste0(
		"Duplicate key columns (names or numbers). Press Enter for default: ",
		default_label,
		" (type 'suggest' to reprint defaults)\n> "
	)

	repeat {
		input <- readline(prompt_txt)
		input <- trimws(input)
		if (tolower(input) %in% c("suggest", "suggested", "s")) {
			if (!quiet) message("Suggested default columns: ", default_label)
			next
		}
		if (!nzchar(input)) {
			return(default_cols)
		}

		if (tolower(input) %in% c("all", "everything")) {
			return(setdiff(df_names, id_col_name))
		}

		if (all(grepl("^[0-9,:;\\s]+$", input))) {
			idx <- parse_index_input(input, length(df_names))
			cols <- df_names[idx]
			cols <- setdiff(cols, id_col_name)
			return(unique(cols))
		}

		parts <- unlist(strsplit(input, "[,;]+"))
		parts <- trimws(parts)
		parts <- parts[parts != ""]
		cols <- parts[parts %in% df_names]
		cols <- setdiff(cols, id_col_name)
		return(unique(cols))
	}
}

#' Pick columns to compare duplicates
#'
#' @param df Data frame of responses.
#' @param id_col_name Submission ID column.
#' @param compare_cols Optional pre-selected columns.
#' @param quiet Logical. Suppress messages.
#' @param prompt Logical. Allow interactive prompting.
#' @param default_cols Default columns to use when prompting.
#' @return Character vector of comparison column names.
#' @keywords internal
pick_compare_columns <- function(df, id_col_name, compare_cols, quiet = FALSE, prompt = TRUE, default_cols = NULL) {
	df_names <- names(df)
	if (is.null(default_cols)) {
		default_cols <- unique(c(id_col_name, head(setdiff(df_names, id_col_name), 4)))
	} else {
		default_cols <- unique(default_cols)
	}
	if (length(default_cols) == 0) {
		default_cols <- id_col_name
	}

	if (!is.null(compare_cols)) {
		if (is.numeric(compare_cols)) {
			compare_cols <- df_names[compare_cols]
		}
		compare_cols <- unique(as.character(compare_cols))
		compare_cols <- compare_cols[compare_cols %in% df_names]
		if (length(compare_cols) == 0) compare_cols <- default_cols
		if (!id_col_name %in% compare_cols) compare_cols <- c(id_col_name, compare_cols)
		return(compare_cols)
	}

	if (!interactive() || !prompt) {
		return(default_cols) # Use defaults in non-interactive mode.
	}

	if (!quiet) {
		message("Choose columns to display for duplicate comparisons.")
		print_column_selection_table(df_names, right = FALSE, max_rows = 25, quiet = quiet)
		if (length(default_cols) > 0) {
			message("Suggested default columns: ", paste(default_cols, collapse = ", "))
		}
	}

	default_label <- paste(default_cols, collapse = ", ")
	prompt <- paste0(
		"Columns to display (names or numbers, comma-separated). Press Enter for default: ",
		default_label,
		" (type 'suggest' to reprint defaults)\n> "
	)

	repeat {
		input <- readline(prompt)
		input <- trimws(input)
		if (tolower(input) %in% c("suggest", "suggested", "s")) {
			if (!quiet) message("Suggested default columns: ", default_label)
			next
		}
		if (!nzchar(input)) {
			return(default_cols)
		}

		if (tolower(input) %in% c("all", "everything")) {
			return(df_names)
		}

		if (all(grepl("^[0-9,:;\\s]+$", input))) {
			idx <- parse_index_input(input, length(df_names))
			cols <- df_names[idx]
			if (length(cols) == 0) cols <- default_cols
			if (!id_col_name %in% cols) cols <- c(id_col_name, cols)
			return(unique(cols))
		}

		parts <- unlist(strsplit(input, "[,;]+"))
		parts <- trimws(parts)
		parts <- parts[parts != ""]
		cols <- parts[parts %in% df_names]
		if (length(cols) == 0) cols <- default_cols
		if (!id_col_name %in% cols) cols <- c(id_col_name, cols)
		return(unique(cols))
	}
}

#' Build duplicate group keys
#'
#' @param df Data frame of responses.
#' @param key_cols Columns that define a duplicate group.
#' @return A list with `key_id` and `group_keys`.
#' @keywords internal
build_duplicate_keys <- function(df, key_cols) {
	key_vals <- lapply(key_cols, function(col) {
		vals <- coerce_repeat_values(df[[col]])
		vals <- as.character(vals)
		vals[is.na(vals) | vals == ""] <- "<NA>"
		vals
	})
	key_frame <- as.data.frame(key_vals, stringsAsFactors = FALSE)
	names(key_frame) <- key_cols

	key_id <- do.call(paste, c(key_frame, sep = "\r"))
	group_keys <- apply(key_frame, 1, function(row) {
		paste(paste0(key_cols, "=", row), collapse = " | ")
	})

	list(key_id = key_id, group_keys = group_keys) # Return IDs and human-readable keys.
}

#' Build a comparison table for duplicate groups
#'
#' @param df Data frame of responses.
#' @param rows_idx Row indices for the group.
#' @param ids Submission IDs for all rows.
#' @param unique_ids Unique IDs in the group.
#' @param compare_cols Columns to display.
#' @param highlight_cols Columns to highlight when values differ.
#' @return A data frame ready for display.
#' @keywords internal
build_compare_frame_by_id <- function(df, rows_idx, ids, unique_ids, compare_cols, highlight_cols = NULL) {
	display_rows <- vapply(unique_ids, function(id) {
		rows_idx[which(ids[rows_idx] == id)[1]]
	}, integer(1))

	out <- df[display_rows, compare_cols, drop = FALSE] # Pick one row per unique ID.
	out <- as.data.frame(out, stringsAsFactors = FALSE) # Avoid factor coercion.

	for (col in names(out)) {
		out[[col]] <- coerce_repeat_values(out[[col]]) # Normalize list/data-frame columns.
		if (inherits(out[[col]], "POSIXt") || inherits(out[[col]], "Date")) {
			out[[col]] <- as.character(out[[col]]) # Format dates for display.
		}
	}

	if (!is.null(highlight_cols)) {
		highlight_cols <- intersect(highlight_cols, names(out))
		use_color <- formior_use_color() # Determine if we can colorize output.
		for (col in highlight_cols) {
			vals <- as.character(out[[col]])
			non_empty <- which(!is.na(vals) & vals != "")
			if (length(non_empty) == 0) next

			base <- vals[non_empty[1]]
			diff_idx <- which(!is.na(vals) & vals != "" & vals != base)
			if (length(diff_idx) > 0) {
				if (isTRUE(use_color)) {
					vals[diff_idx] <- red(vals[diff_idx]) # Colorize differing values.
				}
				out[[col]] <- vals # Update highlighted column.
			}
		}
	}

	row_counts <- vapply(unique_ids, function(id) sum(ids[rows_idx] == id), integer(1)) # Count rows per ID.

	data.frame(
		Option = seq_along(unique_ids),
		SubmissionId = unique_ids,
		RowCount = row_counts,
		out,
		check.names = FALSE,
		stringsAsFactors = FALSE
	)
}

#' Prompt for rows to keep
#'
#' @param n_rows Number of rows in the duplicate group.
#' @return Integer indices to keep, or `"quit"` to stop.
#' @keywords internal
ask_keep_rows <- function(n_rows) {
	repeat {
		input <- readline(paste0("Submissions to keep (1-", n_rows, "; e.g., 1,3 or 2:4; 'all', 'none', 'skip', 'q'): "))
		input <- trimws(tolower(input)) # Normalize user input.

		if (!nzchar(input) || input %in% c("all", "a", "skip", "s")) {
			return(seq_len(n_rows)) # Default to keeping all.
		}
		if (input %in% c("none", "drop", "delete")) {
			confirm <- readline("Drop all rows for this ID? [y/N]: ") # Confirm destructive action.
			if (tolower(trimws(confirm)) %in% c("y", "yes")) {
				return(integer(0)) # Drop all rows in this group.
			}
			next
		}
		if (input %in% c("q", "quit", "exit")) {
			return("quit") # Signal early exit.
		}

		idx <- parse_index_input(input, n_rows) # Parse numeric selections.
		if (length(idx) == 0) {
			message("Please enter one or more valid row numbers.")
			next
		}
		return(idx) # Return valid selections.
	}
}

#' Pick the first non-missing value
#'
#' @param x Vector of values.
#' @return The first non-`NA` value (typed), or `NA` if none.
#' @keywords internal
pick_first_non_na <- function(x) {
	x <- x[!is.na(x)] # Drop missing values.
	# Preserve the input type (e.g., character -> NA_character_)
	x[1]
}

#' Coerce repeated values to stable text
#'
#' @param values Vector or list column values.
#' @return A vector with list/data.frame values coerced to text.
#' @keywords internal
coerce_repeat_values <- function(values) {
	if (is.factor(values)) values <- as.character(values) # Preserve text from factors.

	# Some tidyverse workflows can produce "data.frame columns" (e.g., nested tibbles).
	# As a column, these have one row per submission, but `split()` can't operate on them.
	# Convert each row to a single JSON/text value so the column behaves like a vector.
	if (inherits(values, "data.frame")) {
		if (nrow(values) == 0) return(character(0))
		return(vapply(seq_len(nrow(values)), function(i) {
			list_element_to_text(values[i, , drop = FALSE])
		}, character(1)))
	}

	# FormIO exports can include "object" fields that remain list-columns after flattening.
	# Convert to a single JSON/text value per row so downstream wrangling and export remain stable.
	if (is.list(values) && !inherits(values, "POSIXt") && !inherits(values, "Date")) {
		return(vapply(values, list_element_to_text, character(1)))
	}

	values # Return unchanged when already atomic.
}


#' Convert nested list elements to text
#'
#' @param x List, data.frame, or atomic value.
#' @return A character representation (or `NA_character_`).
#' @keywords internal
list_element_to_text <- function(x) {
	if (is.null(x) || length(x) == 0) return(NA_character_)

	# Common cases: scalar atomic values
	if (is.atomic(x) && length(x) == 1) return(as.character(x))
	if (is.atomic(x) && length(x) > 1) return(paste(as.character(x), collapse = ", "))

	# For nested lists / data.frames, use JSON for a compact, readable representation.
	json <- tryCatch(
		as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")),
		error = function(e) NA_character_
	)

	json # Return JSON representation.
}

#' Pick an automatic repeat strategy
#'
#' @param values Vector of column values.
#' @return A strategy name.
#' @keywords internal
auto_strategy <- function(values) {
	if (is_numeric_like(values)) return("sum")
	if (is_checkbox_like(values)) return("count_yes")
	"concat"
}

#' Apply a repeat-resolution strategy
#'
#' @param values Vector of values for one submission.
#' @param strategy Strategy name.
#' @param sep Separator for concatenation.
#' @param unique Logical. Remove duplicates before concatenation/first/last.
#' @return A single resolved value.
#' @keywords internal
apply_repeat_strategy <- function(values, strategy, sep, unique) {
	# Keep factors as text to avoid confusing numeric coercion and vapply type mismatches.
	if (is.factor(values)) values <- as.character(values)

	values <- values[!is.na(values)] # Drop missing values.
	if (unique && strategy %in% c("concat", "first", "last")) {
		values <- unique(values) # Optionally remove duplicates.
	}

	if (strategy == "concat") {
		return(if (length(values) == 0) NA_character_ else paste(values, collapse = sep))
	}
	if (strategy == "first") {
		# values[1] returns a typed NA when length(values) == 0
		return(values[1])
	}
	if (strategy == "last") {
		if (length(values) == 0) return(values[1])
		return(values[length(values)])
	}
	if (strategy == "sum") {
		return(sum(suppressWarnings(as.numeric(values)), na.rm = TRUE))
	}
	if (strategy == "mean") {
		return(mean(suppressWarnings(as.numeric(values)), na.rm = TRUE))
	}
	if (strategy == "count") {
		return(length(values))
	}
	if (strategy == "count_yes") {
		values <- normalize_checkbox_values(values)
		return(sum(values %in% normalize_checkbox_values(default_yes_values()), na.rm = TRUE)) # Count checked values.
	}

	if (length(values) == 0) NA else values[1] # Fallback to first value.
}

#' Check if values are numeric-like
#'
#' @param values Vector of values.
#' @return `TRUE` if values appear numeric.
#' @keywords internal
is_numeric_like <- function(values) {
	if (is.factor(values)) values <- as.character(values) # Preserve numeric text.
	if (all(is.na(values))) return(FALSE)
	num <- suppressWarnings(as.numeric(values)) # Attempt numeric coercion.
	any(!is.na(num)) && sum(is.na(num)) <= sum(is.na(values))
}

#' Default values treated as "yes"
#'
#' @return A vector of values considered affirmative.
#' @keywords internal
default_yes_values <- function() {
	c(TRUE, "TRUE", "True", "true", "Yes", "YES", "yes", "Y", "y", 1, "1")
}

#' Determine if values resemble checkbox selections
#'
#' @param values Vector of values.
#' @param values_df Optional data frame (unused, for compatibility).
#' @param yes_values Values treated as selected.
#' @param no_values Values treated as not selected.
#' @return `TRUE` if values look like checkbox inputs.
#' @keywords internal
is_checkbox_like <- function(values, values_df = NULL, yes_values = default_yes_values(), no_values = NULL) {
	if (is.logical(values)) return(TRUE) # Logical values are checkbox-like.
	if (is.numeric(values)) {
		vals <- values[!is.na(values)]
		if (length(vals) == 0) return(TRUE)
		return(all(vals %in% c(0, 1))) # Numeric checkbox values should be 0/1.
	}
	if (is.null(no_values)) {
		no_values <- c(FALSE, "FALSE", "False", "false", "No", "NO", "no", "N", "n", 0, "0", "", " ")
	}

	values <- normalize_checkbox_values(values)
	values <- values[!is.na(values)]
	if (length(values) == 0) return(TRUE)

	allowed <- c(normalize_checkbox_values(yes_values), normalize_checkbox_values(no_values))
	all(values %in% allowed) # Accept only yes/no values.
}

#' Normalize checkbox values for comparison
#'
#' @param values Vector of values.
#' @return Normalized character vector.
#' @keywords internal
normalize_checkbox_values <- function(values) {
	if (is.factor(values)) values <- as.character(values) # Preserve text from factors.
	values <- trimws(as.character(values)) # Trim whitespace.
	values[values == "NA"] <- NA_character_ # Treat literal "NA" as missing.
	values
}

#' Derive prefixes from column names
#'
#' @param names_vec Column names.
#' @param sep Separator between prefix and suffix.
#' @return Prefixes or `NA` when no separator is present.
#' @keywords internal
derive_prefix <- function(names_vec, sep) {
	pattern <- paste0(sep, "[^", sep, "]+$")
	prefixes <- sub(pattern, "", names_vec)
	prefixes[prefixes == names_vec] <- NA_character_ # Mark names without separator.
	prefixes
}

#' Extract option suffix from a column name
#'
#' @param name Column name.
#' @param sep Separator between prefix and suffix.
#' @return Suffix portion of the name.
#' @keywords internal
option_suffix <- function(name, sep) {
	pattern <- paste0("^.*", sep)
	sub(pattern, "", name) # Remove the prefix portion.
}

#' Guess a timestamp column name
#'
#' @param names_vec Column names.
#' @return The first matching timestamp column name, or `NULL`.
#' @keywords internal
guess_time_col <- function(names_vec) {
	candidates <- c(
		"modified", "created", "updated", "submitted",
		"submissionDate", "submittedDate", "createdDate", "modifiedDate",
		"_createdAt", "_updatedAt", "lastModified", "timestamp"
	)
	candidates[candidates %in% names_vec][1] %||% NULL
}

#' Pick rows to keep for deduplication
#'
#' @param df Data frame of responses.
#' @param id_col Submission ID column name.
#' @param time_col Optional time column name.
#' @param keep One of "last" or "first".
#' @return Integer indices of rows to keep.
#' @keywords internal
pick_dedup_rows <- function(df, id_col, time_col, keep) {
	ids <- df[[id_col]]
	unique_ids <- ids[!duplicated(ids)]

	if (!is.null(time_col)) {
		times <- suppressWarnings(as.POSIXct(df[[time_col]], tz = "UTC")) # Parse time column if present.
		if (all(is.na(times))) {
			time_col <- NULL # Disable time-based ordering when parsing fails.
		}
	}

	selected <- integer(0) # Accumulate selected row indices.
	for (id in unique_ids) {
		idx <- which(ids == id)
		if (length(idx) == 1) {
			selected <- c(selected, idx) # Keep single rows.
			next
		}

		if (!is.null(time_col)) {
			times <- suppressWarnings(as.POSIXct(df[[time_col]][idx], tz = "UTC")) # Parse times for this ID.
			if (keep == "last") {
				best <- idx[which.max(times)] # Keep most recent.
			} else {
				best <- idx[which.min(times)] # Keep earliest.
			}
		} else {
			best <- if (keep == "last") idx[length(idx)] else idx[1] # Fallback to row order.
		}

		selected <- c(selected, best) # Add chosen row.
	}

	selected # Return selected indices.
}

#' Get typed NA for a vector
#'
#' @param values Vector used to infer type.
#' @return An NA value of the appropriate type.
#' @keywords internal
na_value_for <- function(values) {
	if (is.factor(values)) values <- as.character(values) # Normalize factors.
	if (inherits(values, "Date")) return(as.Date(NA))
	if (inherits(values, "POSIXt")) return(as.POSIXct(NA))
	if (is.numeric(values)) return(NA_real_)
	if (is.logical(values)) return(NA)
	NA_character_
}

#' Provide a typed default for vapply
#'
#' @param strategy Strategy name.
#' @param values Vector used to infer type.
#' @return A typed NA value matching the strategy output.
#' @keywords internal
strategy_fun_value <- function(strategy, values) {
	if (strategy %in% c("sum", "mean", "count", "count_yes")) return(NA_real_) # Numeric output.
	if (strategy == "concat") return(NA_character_) # Character output.
	na_value_for(values) # Fallback to column-typed NA.
}
