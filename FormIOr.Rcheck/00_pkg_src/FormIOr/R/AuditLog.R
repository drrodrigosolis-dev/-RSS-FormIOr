#' Start an audit log for FormIOr actions
#'
#' Creates a new audit log file and turns on automatic logging for
#' FormIOr functions. Each subsequent action (downloads, cleaning,
#' exports, etc.) will append a new row to the log.
#'
#' The audit log is designed to be "Excel friendly": it is a simple CSV/TSV
#' with one row per action, including a timestamp, an action name, optional
#' details, and (when available) row/column counts for the dataset being
#' processed.
#'
#' If you do *not* start a log, many FormIOr functions will (once per R session)
#' ask whether you want to begin logging. Starting a log here avoids that prompt
#' and gives you control over the file location.
#'
#' @param file Path to the audit log file. Required.
#' @param overwrite Logical. If `FALSE` (default), stop if the file exists.
#' @param append Logical. If `TRUE`, append to an existing log instead of
#'   erroring. Useful when resuming a previous session.
#' @param quiet Logical. If `FALSE`, prints a short message.
#'
#' @return Invisibly returns the first log entry (a data.frame row).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' log_file <- tempfile(fileext = ".csv")
#' StartAuditLog(log_file, overwrite = TRUE)
#'
#' # Run some FormIOr steps (each will append a row when possible)
#' flat <- FlattenSubmissions(FoodTypes)
#' norm <- NormalizeColumnNames(flat, quiet = TRUE)
#'
#' StopAuditLog()
#' read.csv(log_file, stringsAsFactors = FALSE)
#' }
StartAuditLog <- function(file = NULL, overwrite = FALSE, append = FALSE, quiet = FALSE) {
	if (is.null(file)) {
		stop("file is required. Provide a file path (e.g., tempfile(fileext = '.csv')).")
	}
	file <- as.character(file)[1]
	if (is.na(file) || !nzchar(file)) {
		stop("file is required. Provide a file path (e.g., tempfile(fileext = '.csv')).")
	}
	file_exists <- file.exists(file) # Check whether the log already exists.
	if (file_exists) {
		if (isTRUE(overwrite)) {
			file.remove(file) # Remove existing log so we can start fresh.
			file_exists <- FALSE
		} else if (!isTRUE(append)) {
			stop("Audit log already exists. Set overwrite = TRUE to replace it, or append = TRUE to continue it.")
		}
	}

	entry <- WriteAuditLog( # Write the initial "log started" entry.
		action = "log_started",
		details = if (isTRUE(append) && file_exists) "Audit log resumed" else "Audit log started",
		file = file,
		append = isTRUE(append) && file_exists,
		quiet = TRUE
	)

	state <- get_audit_state() # Load current audit state.
	state$active <- TRUE # Mark logging as active.
	state$file <- file # Store the log file path.
	state$prompted <- TRUE # Avoid prompting again in this session.
	set_audit_state(state) # Persist updated state.

	if (!quiet) {
		message("Audit log started: ", file)
	}

	invisible(entry) # Return the first entry invisibly.
}


#' Stop audit logging for FormIOr
#'
#' Turns off automatic logging. The log file is not deleted.
#' You can start a new log later using [StartAuditLog()].
#'
#' @param quiet Logical. If `FALSE`, prints a short message.
#'
#' @return Invisibly returns `TRUE` when logging was active, `FALSE` otherwise.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' StopAuditLog()
#' }
StopAuditLog <- function(quiet = FALSE) {
	state <- get_audit_state() # Load current audit state.
	was_active <- isTRUE(state$active) # Capture prior status for messaging.

	state$active <- FALSE # Disable logging.
	set_audit_state(state) # Persist updated state.

	if (!quiet) {
		if (was_active) {
			message("Audit logging paused.")
		} else {
			message("Audit logging was already off.")
		}
	}

	invisible(was_active) # Return whether logging was previously active.
}


#' Check whether audit logging is active
#'
#' This is most useful in scripts when you want to conditionally add a manual
#' note using [WriteAuditLog()] only when logging is enabled.
#'
#' @return `TRUE` if an audit log is active, otherwise `FALSE`.
#'
#' @export
#'
#' @examples
#' IsAuditLogActive()
IsAuditLogActive <- function() {
	state <- get_audit_state() # Load current audit state.
	isTRUE(state$active) # Return TRUE when active.
}


# ---- Internal helpers ---------------------------------------------------------

#' Read audit log state
#'
#' Fetches the audit logging settings from R options and fills defaults.
#'
#' @return A list of audit state fields.
#' @keywords internal
get_audit_state <- function() {
	state <- getOption("formior.audit") # Load state from options.
	if (!is.list(state)) state <- list() # Ensure state is list-like.

	if (is.null(state$active)) state$active <- FALSE # Default to inactive.
	if (is.null(state$file)) state$file <- NULL # Default to no file.
	if (is.null(state$prompted)) state$prompted <- FALSE # Default to not prompted.
	if (is.null(state$depth)) state$depth <- 0L # Default nesting depth.

	state # Return the normalized state list.
}


#' Persist audit log state
#'
#' Stores the audit state in R options.
#'
#' @param state Audit state list.
#' @return The stored state (invisibly).
#' @keywords internal
set_audit_state <- function(state) {
	options(formior.audit = state) # Persist to options.
	invisible(state) # Return the state invisibly.
}


#' Increment audit nesting depth
#'
#' Tracks nested calls so prompts/logs happen only at top level.
#'
#' @return Updated depth.
#' @keywords internal
audit_enter <- function() {
	state <- get_audit_state() # Load current state.
	state$depth <- state$depth + 1L # Increment nesting depth.
	set_audit_state(state) # Persist updated state.
	state$depth # Return new depth.
}


#' Decrement audit nesting depth
#'
#' Ensures nesting depth does not fall below zero.
#'
#' @return Updated depth (invisibly).
#' @keywords internal
audit_exit <- function() {
	state <- get_audit_state() # Load current state.
	state$depth <- max(0L, state$depth - 1L) # Decrement with floor at zero.
	set_audit_state(state) # Persist updated state.
	invisible(state$depth) # Return new depth invisibly.
}


#' Prompt to start audit logging
#'
#' Interactively asks the user to start logging once per session.
#'
#' @return `TRUE` if a log was started, otherwise `FALSE` (invisibly).
#' @keywords internal
maybe_prompt_audit_log <- function() {
	state <- get_audit_state() # Load current state.
	if (isTRUE(state$active) || isTRUE(state$prompted) || !interactive()) {
		return(invisible(FALSE)) # Skip prompt when already active or not interactive.
	}

	answer <- readline("No active audit log. Start one now? [y/N]: ") # Prompt user.
	state$prompted <- TRUE # Avoid prompting again in this session.
	set_audit_state(state) # Persist prompted state.
	if (tolower(answer) %in% c("y", "yes")) {
		log_file <- state$file
		if (is.null(log_file) || !nzchar(log_file)) {
			log_file <- readline("Audit log file path (leave blank to cancel): ")
			log_file <- trimws(log_file)
		}
		if (!nzchar(log_file)) return(invisible(FALSE))
		append_mode <- file.exists(log_file) # Resume if file already exists.
		StartAuditLog(file = log_file, overwrite = FALSE, append = append_mode, quiet = TRUE) # Start logging silently.
		return(invisible(TRUE)) # Indicate that logging started.
	}

	invisible(FALSE) # Indicate that logging was not started.
}


#' Write an audit entry if logging is active
#'
#' Convenience wrapper that checks state before appending to the log.
#'
#' @param action Action name to record.
#' @param details Optional details string.
#' @param data Optional data frame to summarize.
#' @return The result of `WriteAuditLog()` (invisibly) or `NULL`.
#' @keywords internal
maybe_write_audit <- function(action, details = NULL, data = NULL) {
	state <- get_audit_state() # Load current state.
	if (!isTRUE(state$active) || is.null(state$file)) {
		return(invisible(NULL)) # Skip when logging is inactive.
	}

	WriteAuditLog( # Append a log row.
		action = action,
		details = details,
		file = state$file,
		data = data,
		append = TRUE,
		quiet = TRUE
	)
}
