#' Add Hierarchical Sections to FormIO Response Columns
#'
#' Assigns hierarchical section labels to columns in flattened FormIO response data
#' (up to 3 levels deep). You can run it interactively with prompts, or
#' non-interactively by supplying `depth`, `section_names`, and `section_rows`.
#' The function handles input that may be a data frame, a list from
#' \code{flatten_submission_records()}, or raw output from \code{fetch_form_responses()}.
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame (possibly with nested list-columns), or a list containing \code{FlatResponses} (a flattened data frame) or \code{submission_data} (from \code{fetch_form_responses()} with \code{content.only = FALSE}).
#' @param depth Optional section depth (`1`, `2`, or `3`). If `NULL`, prompts.
#' @param section_names Optional section names by level. Provide a list where
#'   each element is a character vector for one level.
#' @param section_rows Optional row assignments by level/section. Provide a list
#'   where each element corresponds to a level and contains row selectors for
#'   each section (numeric vectors, ranges like `"1:5"`, or comma strings).
#'
#' @details
#' The function first extracts or flattens the input to obtain a flat data frame of responses. It then prompts the user to:
#' \itemize{
#'   \item Select the depth of sections (1, 2, or 3).
#'   \item Provide comma-separated names for sections at each level (no spaces or special characters).
#'   \item Assign row numbers (from the displayed column list) to each section at each level. Row numbers can be single values, comma-separated lists, or ranges (e.g., "1:5,8,10:12").
#' }
#' If `depth`, `section_names`, and `section_rows` are supplied, prompts are
#' skipped and the function runs non-interactively. Empty assignments are filled
#' with "General". The interactive prompt flow uses console clearing (\code{\014})
#' and colored prompts (requires the \code{crayon} package).
#'
#' @return A list with two elements:
#' \item{FlatResponses}{The original flattened data frame of responses.}
#' \item{Sections}{A \code{tibble} with columns \code{No} (column number), \code{Names} (original column names), and \code{Level-1}, \code{Level-2}, \code{Level-3} (section assignments, filled with "General" if empty).}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming FoodTypes is a sample dataset with possible nests
#' data("FoodTypes")
#' sectioned <- assign_section_hierarchy(FoodTypes)
#' print(sectioned$Sections)
#' }
#'
#' @importFrom tibble tibble add_column
#' @importFrom dplyr mutate across
#' @importFrom crayon inverse red bold blurred `%+%`
#'
#'
assign_section_hierarchy <- function(x, depth = NULL, section_names = NULL, section_rows = NULL) {
	if (!interactive() && (is.null(depth) || is.null(section_names) || is.null(section_rows))) {
		stop("assign_section_hierarchy() is interactive unless depth, section_names, and section_rows are provided.")
	}
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	# Improved input handling
	if (inherits(x, "data.frame")) {
		Data <- x # Use the input directly when it is already a data frame.
	} else if (is.list(x) && !is.null(x$FlatResponses)) {
		Data <- x$FlatResponses # Use the flattened responses from a previous step.
	} else if (is.list(x) && !is.null(x$submission_data)) {
		Data <- x$submission_data # Use raw submissions when provided in the list.
	} else {
		stop("Input must be a data frame or a list containing 'FlatResponses' or 'submission_data'")
	}

	# Check for nested columns and flatten if necessary
	if (any(sapply(Data, is.list))) {
		message("Detected nested columns; flattening...")
		Flat <- flatten_submission_records(Data) # Flatten list-columns for interactive selection.
		Data <- Flat$FlatResponses # Keep only the flattened data for downstream steps.
	}

	CtrlDF <- tibble(No = 1:length(colnames(Data)), Names = colnames(Data)) # Create the column index table.
	print(CtrlDF, n = nrow(CtrlDF)) # Show all columns so the user can choose section assignments.

	Depth <- prompt_section_depth(depth = depth) # Resolve depth from args or ask the user.

	for (i in 1:Depth) {
		CtrlDF <- CtrlDF %>% add_column("Level-{i}" := "") # Add empty section columns to fill in.
	}

	cat("\014") # Clear the console before collecting section assignments.
	print(CtrlDF, n = nrow(CtrlDF)) # Reprint the table with new level columns.

	Sections <- prompt_section_names(DepthAsked = Depth, section_names = section_names) # Resolve section names from args or prompts.
	Output <- assign_sections_to_columns(Sections = Sections, df = CtrlDF, section_rows = section_rows) # Map section names to columns.
	Data <- list(FlatResponses = Data, Sections = Output) # Package data and section mapping together.

	if (audit_depth == 1) {
		maybe_write_audit("assign_section_hierarchy", data = Data$FlatResponses) # Record that sections were assigned.
	}

	return(Data) # Return the flattened data and the section assignment table.
}

#' Prompt for section depth
#'
#' Interactively asks the user to choose the number of section levels (1–3).
#'
#' @return An integer depth between 1 and 3.
#' @keywords internal
prompt_section_depth <- function(depth = NULL) {
	if (!is.null(depth)) {
		DepthAsked <- suppressWarnings(as.integer(depth[1]))
		if (!(DepthAsked %in% c(1L, 2L, 3L))) {
			stop("depth must be 1, 2, or 3.")
		}
		return(DepthAsked)
	}
	if (!interactive()) {
		stop("depth must be provided in non-interactive sessions.")
	}
	while (TRUE) {
		cat("\014") # Clear the console for a clean prompt.
		cat(inverse("Select the depth of sections that you want to work with (1, 2 or 3)")) # Highlight the question.
		DepthAsked <- as.numeric(readline()) # Read the user choice as a number.
		if (DepthAsked %in% c(1, 2, 3)) return(DepthAsked) # Accept only valid options.
		cat("\014") # Clear the console before showing the error.
		cat(red("please select a number between 1 and 3")) # Show validation feedback.
		Sys.sleep(2) # Pause so the user can read the message.
	}
}

#' Prompt for section names at each level
#'
#' Collects comma-separated section names for each requested level.
#'
#' @param DepthAsked Integer depth selected by the user.
#' @param section_names Optional list of section names by level.
#' @return A list of character vectors, one per section level.
#' @keywords internal
prompt_section_names <- function(DepthAsked, section_names = NULL) {
	if (!is.null(section_names)) {
		if (is.character(section_names) && DepthAsked == 1L) {
			section_names <- list(section_names)
		}
		if (!is.list(section_names)) {
			stop("section_names must be a list with one character vector per level.")
		}
		if (length(section_names) < DepthAsked) {
			stop("section_names must include at least ", DepthAsked, " level(s).")
		}

		SectionsList <- vector("list", DepthAsked)
		for (i in seq_len(DepthAsked)) {
			level_names <- trimws(as.character(section_names[[i]]))
			level_names <- level_names[nzchar(level_names)]
			if (length(level_names) == 0) {
				stop("section_names[[", i, "]] must contain at least one non-empty name.")
			}
			SectionsList[[i]] <- level_names
		}
		return(SectionsList)
	}

	if (!interactive()) {
		stop("section_names must be provided in non-interactive sessions.")
	}

	SectionsList <- list() # Initialize the list that stores each level's names.
	for (i in 1:(min(3, DepthAsked))) {
		cat(inverse(paste("Please provide the names for the sections at level", i)) %+% blurred("\n Separate each name with a comma and do not use spaces or special characters"))
		SectionNames <- readline() # Capture the raw comma-separated input.
		SectionNames <- trimws(strsplit(SectionNames, split = ",")[[1]]) # Split and trim whitespace.
		SectionsList[[i]] <- SectionNames # Store this level's names.
	}
	return(SectionsList) # Return the list of section names by level.
}

#' Assign section names to columns
#'
#' Walks the user through mapping columns to section labels at each level.
#'
#' @param Sections List of section name vectors.
#' @param df A data frame of column numbers and names with empty level columns.
#' @param section_rows Optional row assignments by level/section.
#' @return The input data frame with section assignments filled in.
#' @keywords internal
assign_sections_to_columns <- function(Sections, df, section_rows = NULL) {
	for (i in 1:length(Sections)) {
		for (j in 1:length(Sections[[i]])) {
			CurrentSection <- Sections[[i]][j] # Current section label to assign.
			selection <- NULL
			if (!is.null(section_rows) && length(section_rows) >= i) {
				level_rows <- section_rows[[i]]
				if (is.list(level_rows)) {
					if (!is.null(names(level_rows)) && CurrentSection %in% names(level_rows)) {
						selection <- level_rows[[CurrentSection]]
					} else if (length(level_rows) >= j) {
						selection <- level_rows[[j]]
					}
				} else if (j == 1L) {
					selection <- level_rows
				}
			}

			if (is.null(selection)) {
				if (!interactive()) {
					stop(
						"section_rows is missing an assignment for level ",
						i,
						", section '",
						CurrentSection,
						"'."
					)
				}
				cat("\014") # Clear the console between prompts.
				print(df, n = nrow(df)) # Show the current mapping table.
				cat(inverse("Please provide the row number(s) of the items that will belong to " %+% bold(red(CurrentSection)) %+% " at Depth " %+% bold(red(i))))
				cat("\n")
				cat(blurred("use only numbers, separated by commas, ranges with :"))
				cat("\n")
				NoRowsAssigned <- get_numbers("") # Parse user input into numeric row indices.
			} else {
				NoRowsAssigned <- get_numbers(input = selection) # Parse supplied row selectors.
			}

			NoRowsAssigned <- as.integer(NoRowsAssigned)
			NoRowsAssigned <- sort(unique(NoRowsAssigned[!is.na(NoRowsAssigned) & NoRowsAssigned >= 1 & NoRowsAssigned <= nrow(df)]))
			if (length(NoRowsAssigned) > 0) {
				df[NoRowsAssigned, i + 2] <- CurrentSection # Assign the label to selected rows.
			}
			print(df, n = nrow(df)) # Show the updated table after each assignment.
		}
		print(df, n = nrow(df)) # Reprint after each level for review.
	}
	df <- df %>% mutate(across(all_of(colnames(df)), ~ ifelse(. == "", "General", .))) # Fill empty cells with "General".
	print(df, n = nrow(df)) # Show the final mapping table.
	return(df) # Return the completed assignment table.
}

#' Parse row number input
#'
#' Converts comma-separated values and ranges into a sorted vector of row indices.
#'
#' @param prompt Prompt text to display (empty string for none).
#' @param input Optional supplied row selector(s). If `NULL`, prompts.
#' @return An integer vector of selected row numbers.
#' @keywords internal
get_numbers <- function(prompt = "Enter numbers (comma-separated, ranges with : ok): ", input = NULL) {
	if (is.null(input)) {
		input <- readline(prompt = prompt) # Read the raw user input.
	} else {
		input <- paste(as.character(input), collapse = ",")
	}
	input <- trimws(input)
	if (!nzchar(input)) {
		return(integer(0))
	}

	# Split by comma
	parts <- strsplit(input, ",")[[1]]
	parts <- trimws(parts) # Remove extra spaces from each token.

	result <- integer(0) # Accumulate parsed indices.

	for (p in parts) {
		# Check if it's a range (contains ":")
		if (grepl(":", p)) {
			# Split range like "8:15" -> c("8", "15")
			rng <- strsplit(p, ":")[[1]]
			rng <- trimws(rng) # Normalize range endpoints.

			# Must have exactly 2 parts
			if (length(rng) != 2) {
				warning("Invalid range format: ", p, " -> skipping")
				next
			}

			# Try to convert to numbers
			start <- suppressWarnings(as.numeric(rng[1]))
			end <- suppressWarnings(as.numeric(rng[2]))

			if (is.na(start) || is.na(end)) {
				warning("Non-numeric in range: ", p, " -> skipping")
				next
			}

			if (start <= end) {
				result <- c(result, seq(from = start, to = end, by = 1)) # Add ascending range.
			} else {
				# Optional: allow descending ranges
				result <- c(result, seq(from = start, to = end, by = -1)) # Add descending range.
			}
		}
		# Single number
		else {
			val <- suppressWarnings(as.numeric(p)) # Attempt to parse a single number.
			if (is.na(val)) {
				warning("Invalid number: ", p, " -> skipped")
			} else {
				result <- c(result, val) # Add the parsed index.
			}
		}
	}

	# Remove possible duplicates and sort (optional but often desired)
	result <- sort(unique(result)) # Normalize selections for stable ordering.

	return(result) # Return the parsed row indices.
}
