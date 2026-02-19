#' Adjust submissions by ID (delete or edit specific values)
#'
#' Sometimes you need to make small, targeted fixes before you export:
#' remove test submissions, correct a value for one submission, or blank out
#' a field for privacy reasons.
#'
#' This helper lets you:
#' - Delete submission(s) by ID (remove rows)
#' - Update one or more column values for specific submission ID(s)
#'
#' It works on either a plain data frame or the list produced by
#' [flatten_submission_records()]. If audit logging is active (see [start_audit_log()]),
#' this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param id_col Integer or character. Submission ID column (default 1).
#' @param delete_ids Optional character vector of submission IDs to delete.
#' @param updates Optional updates to apply. Supported formats:
#' - A data.frame with columns `id`, `column`, `value`
#' - A data.frame with columns `submissionId`/`submission_id`, `column`, `value`
#' - A list of lists, each with elements `id`, `column`, `value`
#'
#' Values are coerced to the target column type when possible. Use `"NA"` (or
#' an actual `NA`) to set a value to missing.
#' @param return_flat Logical. If `TRUE` and `x` came from [flatten_submission_records()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Updated data frame (the cleaned data)}
#'   \item{summary}{Data frame summarizing deletions/updates}
#'   \item{changes}{Data frame listing each requested update and its status}
#'   \item{flat}{If `return_flat = TRUE`, the updated flatten_submission_records-style list (for workflows that expect `FlatResponses`)}
#' }
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   submissionId = c("a", "b", "c"),
#'   status = c("ok", "test", "ok"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Delete one submission and update a value
#' out <- apply_submission_updates(
#'   df,
#'   id_col = "submissionId",
#'   delete_ids = "b",
#'   updates = data.frame(id = "c", column = "status", value = "review", stringsAsFactors = FALSE),
#'   quiet = TRUE
#' )
#' out$data
apply_submission_updates <- function(
		x,
		id_col = 1,
		delete_ids = NULL,
		updates = NULL,
		return_flat = FALSE,
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	df <- extract_flat_df(x) # Normalize input to a flat data frame.
	df <- as.data.frame(df, stringsAsFactors = FALSE) # Avoid factor coercion for updates.
	id_col_name <- resolve_id_col(df, id_col) # Resolve the submission ID column name.

	n_before <- nrow(df) # Track starting row count for summary reporting.
	change_rows <- list() # Collect change rows for the detailed audit table.

	# ---- Deletes ----------------------------------------------------------------
	deleted_rows <- 0L # Count of rows removed.
	deleted_ids <- character(0) # Track which IDs were deleted.
	if (!is.null(delete_ids) && length(delete_ids) > 0) {
		delete_ids <- unique(as.character(delete_ids)) # Normalize IDs to unique strings.
		delete_ids <- delete_ids[!is.na(delete_ids) & delete_ids != ""] # Drop blanks.

		if (length(delete_ids) > 0) {
			keep <- !(as.character(df[[id_col_name]]) %in% delete_ids) # Flag rows to keep.
			deleted_rows <- sum(!keep) # Count deleted rows.
			deleted_ids <- unique(as.character(df[[id_col_name]][!keep])) # Capture deleted IDs.
			df <- df[keep, , drop = FALSE] # Remove deleted rows.

			change_rows[[length(change_rows) + 1]] <- data.frame( # Record delete summary row.
				action = "delete",
				id = paste(deleted_ids, collapse = ", "),
				column = NA_character_,
				old_value = NA_character_,
				new_value = NA_character_,
				rows_affected = as.integer(deleted_rows),
				status = if (deleted_rows > 0) "deleted" else "no_match",
				message = if (deleted_rows > 0) NA_character_ else "No matching submission IDs found.",
				stringsAsFactors = FALSE
			)
		}
	}

	# ---- Updates ----------------------------------------------------------------
	updates_df <- normalize_adjust_updates(updates) # Normalize updates into a consistent data frame.
	applied_updates <- 0L # Track how many updates were applied.
	if (!is.null(updates_df) && nrow(updates_df) > 0) {
		for (i in seq_len(nrow(updates_df))) {
			id_val <- as.character(updates_df$id[i]) # Submission ID to update.
			col_nm <- as.character(updates_df$column[i]) # Column name to update.
			new_raw <- updates_df$value[[i]] # Raw new value (may be list).

			if (is.na(id_val) || !nzchar(id_val)) {
				change_rows[[length(change_rows) + 1]] <- data.frame( # Record invalid ID.
					action = "update",
					id = id_val,
					column = col_nm,
					old_value = NA_character_,
					new_value = to_update_text(new_raw),
					rows_affected = 0L,
					status = "invalid_id",
					message = "Missing submission ID.",
					stringsAsFactors = FALSE
				)
				next
			}

			if (is.na(col_nm) || !nzchar(col_nm) || !col_nm %in% names(df)) {
				change_rows[[length(change_rows) + 1]] <- data.frame( # Record missing column.
					action = "update",
					id = id_val,
					column = col_nm,
					old_value = NA_character_,
					new_value = to_update_text(new_raw),
					rows_affected = 0L,
					status = "column_not_found",
					message = paste0("Column not found: ", col_nm),
					stringsAsFactors = FALSE
				)
				next
			}

			rows <- which(as.character(df[[id_col_name]]) == id_val) # Locate rows for this submission ID.
			if (length(rows) == 0) {
				change_rows[[length(change_rows) + 1]] <- data.frame( # Record missing ID.
					action = "update",
					id = id_val,
					column = col_nm,
					old_value = NA_character_,
					new_value = to_update_text(new_raw),
					rows_affected = 0L,
					status = "id_not_found",
					message = "Submission ID not found in data.",
					stringsAsFactors = FALSE
				)
				next
			}

			old_preview <- to_update_text(df[[col_nm]][rows[1]]) # Capture the old value for the log.
			new_val <- coerce_update_value(new_raw, df[[col_nm]]) # Coerce new value to column type.

			if (is.factor(df[[col_nm]])) {
				lvls <- union(levels(df[[col_nm]]), as.character(new_val)) # Ensure new values exist in factor levels.
				levels(df[[col_nm]]) <- lvls
				df[[col_nm]][rows] <- as.character(new_val) # Assign as character to avoid invalid levels.
			} else if (is.list(df[[col_nm]]) && !inherits(df[[col_nm]], "POSIXt") && !inherits(df[[col_nm]], "Date")) {
				df[[col_nm]][rows] <- rep(list(new_val), length(rows)) # Preserve list-column structure.
			} else {
				df[[col_nm]][rows] <- rep(new_val, length(rows)) # Assign scalar values across rows.
			}

			applied_updates <- applied_updates + 1L # Increment update counter.
			change_rows[[length(change_rows) + 1]] <- data.frame( # Record update details.
				action = "update",
				id = id_val,
				column = col_nm,
				old_value = old_preview,
				new_value = to_update_text(new_val),
				rows_affected = as.integer(length(rows)),
				status = "updated",
				message = NA_character_,
				stringsAsFactors = FALSE
			)
		}
	}

	n_after <- nrow(df) # Track final row count for summary reporting.

	changes <- if (length(change_rows) == 0) { # Build the detailed changes table.
		data.frame(
			action = character(),
			id = character(),
			column = character(),
			old_value = character(),
			new_value = character(),
			rows_affected = integer(),
			status = character(),
			message = character(),
			stringsAsFactors = FALSE
		)
	} else {
		do.call(rbind, change_rows)
	}

	summary <- data.frame( # Build the summary metrics table.
		metric = c("rows_before", "rows_after", "rows_deleted", "updates_applied"),
		value = c(n_before, n_after, deleted_rows, applied_updates),
		stringsAsFactors = FALSE
	)

	out <- list( # Package results together.
		data = df,
		summary = summary,
		changes = changes
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- df # Update the flattened data.
		x$ColumnNames <- update_column_names(x$ColumnNames, names(df)) # Keep column metadata in sync.
		out$flat <- x # Attach the updated flat list.
	}

	if (!quiet) {
		if (deleted_rows > 0) message("Deleted ", deleted_rows, " row(s).")
		if (applied_updates > 0) message("Applied ", applied_updates, " update(s).")
		if (deleted_rows == 0 && applied_updates == 0) message("No changes applied.")
	}

	if (audit_depth == 1) {
		maybe_write_audit( # Record the adjustment action.
			"apply_submission_updates",
			details = paste0("deleted_rows=", deleted_rows, "; updates_applied=", applied_updates),
			data = df
		)
	}

	out # Return the output list.
}


# ---- Internal helpers ---------------------------------------------------------

#' Normalize update inputs
#'
#' Coerces supported update formats into a standard data frame with columns
#' `id`, `column`, and `value`.
#'
#' @param updates Updates in data.frame or list form.
#' @return A data.frame with normalized columns, or `NULL` if no updates.
#' @keywords internal
normalize_adjust_updates <- function(updates) {
	if (is.null(updates)) return(NULL) # Nothing to normalize.

	if (inherits(updates, "data.frame")) {
		df <- as.data.frame(updates, stringsAsFactors = FALSE) # Avoid factor coercion.
		id_nm <- intersect(names(df), c("id", "submissionId", "submission_id"))[1] %||% NULL # Find ID column.
		col_nm <- intersect(names(df), c("column", "col", "field"))[1] %||% NULL # Find column name.
		val_nm <- intersect(names(df), c("value", "new_value", "newValue"))[1] %||% NULL # Find value column.

		if (is.null(id_nm) || is.null(col_nm) || is.null(val_nm)) {
			stop("updates must include columns id/submissionId, column, and value.")
		}

		values <- df[[val_nm]] # Pull the update values.
		if (!is.list(values)) values <- as.list(values) # Keep list structure for list-columns.

		return(data.frame(
			id = as.character(df[[id_nm]]),
			column = as.character(df[[col_nm]]),
			value = I(values),
			stringsAsFactors = FALSE
		))
	}

	if (is.list(updates)) {
		if (length(updates) == 0) return(NULL) # Empty list yields no updates.
		rows <- lapply(updates, function(u) {
			if (!is.list(u)) return(NULL) # Ignore non-list entries.
			list(
				id = u$id %||% u$submissionId %||% u$submission_id %||% NA_character_, # Normalize ID key.
				column = u$column %||% u$col %||% u$field %||% NA_character_, # Normalize column key.
				value = list(u$value %||% u$new_value %||% u$newValue %||% NA) # Normalize value key.
			)
		})
		rows <- rows[!vapply(rows, is.null, logical(1))] # Drop invalid rows.
		if (length(rows) == 0) return(NULL) # No usable updates.

		df <- do.call(rbind, lapply(rows, function(r) { # Build a data frame from list rows.
			data.frame(
				id = as.character(r$id),
				column = as.character(r$column),
				value = I(r$value),
				stringsAsFactors = FALSE
			)
		}))
		rownames(df) <- NULL # Drop row names for a clean data frame.
		return(df) # Return the normalized updates.
	}

	stop("updates must be a data.frame or a list.")
}


#' Coerce an update value to a target column type
#'
#' Applies type-aware coercion and handles missing values.
#'
#' @param value The new value provided by the user.
#' @param template The existing column used to infer type.
#' @return A value coerced to the column's type.
#' @keywords internal
coerce_update_value <- function(value, template) {
	# Unwrap common cases
	if (is.list(value) && length(value) == 1 && !inherits(value, "data.frame")) {
		value <- value[[1]] # Unwrap single-item list values.
	}
	if (is.null(value) || length(value) == 0) {
		return(na_value_for(template)) # Return the appropriate NA for the column.
	}

	# Treat "NA" (and blank strings) as missing
	if (is.character(value)) {
		value <- trimws(value)[1] # Trim and take the first string value.
		if (!nzchar(value) || tolower(value) == "na") {
			return(na_value_for(template)) # Convert blank/NA strings to missing values.
		}
	}

	if (inherits(template, "Date")) {
		out <- suppressWarnings(as.Date(value)) # Try to parse as Date.
		if (is.na(out)) return(as.Date(NA)) # Return typed NA on parse failure.
		return(out) # Return parsed Date.
	}
	if (inherits(template, "POSIXt")) {
		out <- suppressWarnings(as.POSIXct(value, tz = "UTC")) # Try to parse as timestamp.
		if (is.na(out)) return(as.POSIXct(NA)) # Return typed NA on parse failure.
		return(out) # Return parsed timestamp.
	}

	if (is.logical(template)) {
		return(parse_logical_value(value)) # Coerce truthy/falsey strings to logical.
	}
	if (is.numeric(template)) {
		out <- suppressWarnings(as.numeric(value)) # Parse numeric values.
		if (is.na(out)) return(NA_real_) # Return typed NA on parse failure.
		return(out) # Return parsed numeric value.
	}

	if (is.factor(template)) {
		return(as.character(value)) # Update factors using character values.
	}

	if (is.list(template) && !inherits(template, "POSIXt") && !inherits(template, "Date")) {
		# Keep list columns as-is (best-effort).
		return(value) # Preserve list structure for list-columns.
	}

	as.character(value)[1] # Fallback to a single character value.
}


#' Parse logical inputs
#'
#' Converts common string/number inputs into logical values.
#'
#' @param x Input to parse.
#' @return `TRUE`, `FALSE`, or `NA`.
#' @keywords internal
parse_logical_value <- function(x) {
	if (is.logical(x)) return(as.logical(x)[1]) # Preserve logical input.
	if (is.numeric(x)) return(as.logical(x != 0)[1]) # Treat non-zero as TRUE.

	chr <- tolower(trimws(as.character(x)[1])) # Normalize to a lowercase string.
	if (!nzchar(chr) || chr == "na") return(NA) # Treat blanks as missing.

	if (chr %in% c("true", "t", "yes", "y", "1")) return(TRUE)
	if (chr %in% c("false", "f", "no", "n", "0")) return(FALSE)
	NA
}


#' Format update values for logging
#'
#' Converts values into a short, human-readable string (or list text).
#'
#' @param x Value to format.
#' @return A single character representation (or `NA_character_`).
#' @keywords internal
to_update_text <- function(x) {
	if (is.null(x) || length(x) == 0) return(NA_character_) # Treat missing values as NA.
	if (is.list(x) && !inherits(x, "data.frame") && length(x) == 1) x <- x[[1]] # Unwrap single-item list values.
	if (is.list(x) || inherits(x, "data.frame")) return(list_element_to_text(x)) # Use list formatting for nested values.
	if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.character(x)[1]) # Format dates/times as strings.
	as.character(x)[1] # Fallback to a single character value.
}
