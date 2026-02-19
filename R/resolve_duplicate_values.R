#' Clean duplicated rows caused by multi-value fields in flattened FormIO data
#'
#' Interactive function designed for users with little or no R experience.
#' It helps resolve columns that contain multiple values per submission
#' (e.g. repeating sections, "add another" items, multi-select questions).
#'
#' Shows a summary of problematic columns first, then asks the user one column
#' at a time how to handle each one (concatenate, keep first, sum, count,
#' remove the column entirely, etc.).
#'
#' @param x List returned by [flatten_submission_records()], must contain `$FlatResponses`
#' @param multi_counts Optional. Output from [count_multivalue_fields()]. If `NULL`,
#'   it is computed automatically.
#' @param id_col Character or integer. Name or position of the submission ID
#'   column (usually `"submissionId"` or column 1). Default: 1.
#' @param ask_threshold Numeric. Only prompt about columns where the maximum
#'   number of distinct values per submission exceeds this value.
#'   Default = 1.05 (catches almost all real multiples while ignoring noise).
#' @param quiet Logical. Suppress most messages and summaries? Default `FALSE`.
#' @param dry_run Logical. If `TRUE`, shows what would happen without actually
#'   modifying the data. Default `FALSE`.
#' @param strategies Optional data frame with columns `column` and `strategy`
#'   used when `prompt = FALSE`.
#' @param prompt Logical. If `TRUE` (default), ask interactively for each column.
#' @param default_strategy Strategy used in non-interactive mode when a column is
#'   not in `strategies`.
#'
#' @return A list with two components:
#'   \describe{
#'     \item{cleaned}{A tibble containing the cleaned, deduplicated responses}
#'     \item{decisions}{A tibble recording which strategy was applied (or if
#'       the column was removed) for each processed column}
#'   }
#'
#' @details
#' Most strategies reduce the data to one row per submission.
#' Choosing "remove" drops the column completely from the output.
#' All decisions are logged so you can review or reproduce the cleaning steps.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Typical workflow
#' responses <- fetch_form_responses(form_id = "your-form-id", api_key = "your-api-key")
#' flat <- flatten_submission_records(responses)
#'
#' # Run interactive cleaning
#' result <- resolve_duplicate_values(flat)
#'
#' # View the cleaned data
#' View(result$cleaned)
#'
#' # See what was done to each column
#' result$decisions
#'
#' # Dry run to preview changes
#' resolve_duplicate_values(flat, dry_run = TRUE)
#' }

resolve_duplicate_values <- function(
		x,
		multi_counts = NULL,
		id_col = 1,
		ask_threshold = 1.05,
		quiet = FALSE,
		dry_run = FALSE,
		strategies = NULL,
		prompt = interactive(),
		default_strategy = "concat_comma"
) {
	prompt <- isTRUE(prompt)
	if (prompt && !interactive()) {
		stop("prompt = TRUE requires an interactive R session. Use prompt = FALSE for scripted runs.")
	}

	# ------ Input checks ------------------------------------------------------------
	if (!is.list(x) || !"FlatResponses" %in% names(x)) {
		stop("x must be output from flatten_submission_records()")
	}

	df <- x$FlatResponses
	if (!is.data.frame(df)) stop("FlatResponses must be a data.frame")

	# Normalize id_col to name
	if (is.numeric(id_col)) {
		id_col_name <- names(df)[id_col]
	} else {
		id_col_name <- as.character(id_col)
	}
	if (!id_col_name %in% names(df)) {
		stop("id_col '", id_col_name, "' not found in data")
	}

	# Compute or use multi_counts
	if (is.null(multi_counts)) {
		if (!quiet) message("Scanning for multi-value columns (may take a few seconds)...")
		multi_counts <- count_multivalue_fields(x, id_col = which(names(df) == id_col_name))
	}

	# Show summary of problematic columns
	if (!quiet) {
		summarize_multi_columns(multi_counts,
														id_col = id_col_name,
														min_distinct = ask_threshold,
														quiet = quiet)
	}

	# Identify columns to potentially fix
	max_per_col <- multi_counts %>%
		summarise(across(-all_of(id_col_name), ~ max(.x, na.rm = TRUE))) %>%
		pivot_longer(everything(),
								 names_to = "column",
								 values_to = "max_distinct") %>%
		filter(max_distinct > ask_threshold) %>%
		arrange(desc(max_distinct))

	if (nrow(max_per_col) == 0) {
		if (!quiet) message("No columns need fixing - returning original data.")
		return(list(cleaned = df, decisions = tibble(column = character(), strategy = character())))
	}

	# -- Guided interactive loop -------------------------------------------------
	decisions <- tibble(column = character(), strategy = character())

	current_df <- df

	strategy_aliases <- c(
		concat_comma = "concat_comma",
		concat_semicolon = "concat_semicolon",
		first = "first",
		last = "last",
		sum = "sum",
		mean = "mean",
		count = "count",
		count_yes = "count_yes",
		skip = "skip",
		remove = "remove"
	)
	if (!default_strategy %in% names(strategy_aliases)) {
		stop("default_strategy must be one of: ", paste(names(strategy_aliases), collapse = ", "))
	}

	strategy_map <- NULL
	if (!is.null(strategies)) {
		strategy_df <- as.data.frame(strategies, stringsAsFactors = FALSE)
		if (!all(c("column", "strategy") %in% names(strategy_df))) {
			stop("strategies must include columns 'column' and 'strategy'.")
		}
		strategy_df$column <- as.character(strategy_df$column)
		strategy_df$strategy <- as.character(strategy_df$strategy)
		bad <- !strategy_df$strategy %in% names(strategy_aliases)
		if (any(bad)) {
			stop("Unsupported strategies: ", paste(unique(strategy_df$strategy[bad]), collapse = ", "))
		}
		strategy_map <- stats::setNames(strategy_aliases[strategy_df$strategy], strategy_df$column)
	}

	for (i in seq_len(nrow(max_per_col))) {

		col_name <- max_per_col$column[i]
		max_d    <- max_per_col$max_distinct[i]

		# Get preview for display
		preview_vals <- unique(current_df[[col_name]][!is.na(current_df[[col_name]])])
		preview <- if (length(preview_vals) == 0) "- all missing -" else
			paste0(paste(head(preview_vals, 4), collapse = ", "), if (length(preview_vals) > 4) ", ..." else "")

		if (isTRUE(prompt)) {
			answer <- ask_strategy_for_column(
				col_name = col_name,
				max_distinct = max_d,
				preview = preview,
				col_data = current_df[[col_name]]
			)

			if (identical(answer$action, "quit")) {
				if (!quiet) message("Quitting early - returning data processed so far.")
				break
			}

			strategy <- answer$strategy
		} else {
			if (!is.null(strategy_map) && col_name %in% names(strategy_map)) {
				strategy <- unname(strategy_map[[col_name]])
			} else {
				strategy <- unname(strategy_aliases[[default_strategy]])
			}
		}


		# Record decision
		decisions <- decisions %>%
			add_row(column = col_name, strategy = strategy)

		if (strategy == "skip") {
			if (!quiet) message("Skipping ", col_name, " (left as-is)")
			next
		}
		if (strategy == "remove") {
			if (!quiet) {
				cat("Removing column", crayon::bold(col_name), "from the output...\n")
			}
			if (!dry_run) {
				current_df <- current_df %>% select(-all_of(col_name))
			} else {
				if (!quiet) message("(dry run - column not actually removed)")
			}
			next
		}
		if (!quiet) {
			cat("Applying:", crayon::green(strategy), "to", crayon::bold(col_name), "...\n")
		}

		if (!dry_run) {
			current_df <- apply_strategy(
				data     = current_df,
				col_name = col_name,
				id_col   = id_col_name,
				strategy = strategy
			)
		} else {
			if (!quiet) message("(dry run - no change made)")
		}
	}

	# Final message
	if (!quiet) {
		cat("\nDone!\n")
		cat("Processed", nrow(decisions), "columns.\n")
		if (dry_run) cat("Dry run complete - no data was changed.\n")
		cat("Use result$cleaned to get the final table.\n\n")
	}

	list(
		cleaned   = current_df,
		decisions = decisions
	)
}





#' Print a human-friendly summary of multi-value columns
#'
#' Displays a concise table of columns that contain more than one distinct value
#' per submission, including the maximum number of distinct answers seen and
#' a short preview of values.
#'
#' Intended mainly for internal use by [resolve_duplicate_values()], but can be called directly.
#'
#' @param multi_counts Data frame from [count_multivalue_fields()]
#' @param id_col Integer or character. Submission ID column (default 1)
#' @param min_distinct Numeric. Only show columns with max distinct >= this value
#'   (default 1.01 to catch almost all real cases)
#' @param n_preview Integer. Number of example values to show per column
#'
#' @return Invisibly returns the filtered summary tibble; mainly called for side effect (printing)
#' @noRd
summarize_multi_columns <- function(multi_counts,
																		id_col = 1,
																		min_distinct = 1.01,
																		n_preview = 5,
																		quiet = FALSE) {

	if (!inherits(multi_counts, "data.frame")) {
		stop("multi_counts must be a data.frame from count_multivalue_fields()")
	}

	# Compute max distinct per column
	summary_df <- multi_counts %>%
		summarise(across(-all_of(id_col), ~ max(.x, na.rm = TRUE))) %>%
		pivot_longer(everything(),
								 names_to = "column",
								 values_to = "max_distinct") %>%
		filter(max_distinct >= min_distinct) %>%
		arrange(desc(max_distinct))

	if (nrow(summary_df) == 0) {
		if (!quiet) {
			message("No columns were found with multiple distinct values per submission.")
			message("Your data looks already flattened/clean - no duplicates to fix.")
		}
		return(invisible(NULL))
	}

	if (!quiet) {
		message("The following columns have multiple answers per person:")
	}

	# Add short preview of actual values (first few non-NA per column)
	previews <- map_chr(summary_df$column, ~ {
		vals <- multi_counts[[.x]][!is.na(multi_counts[[.x]])]
		if (length(vals) == 0) return("- all missing -")
		uniq <- unique(vals)
		if (length(uniq) <= n_preview) {
			paste(uniq, collapse = ", ")
		} else {
			paste0(paste(head(uniq, n_preview), collapse = ", "), ", ...")
		}
	})

	display_df <- summary_df %>%
		mutate(preview = previews,
					 max_distinct = as.integer(max_distinct)) %>%
		select(column, max_distinct, preview)

	if (!quiet) {
		print(display_df, n = nrow(display_df), width = Inf)
		message("Columns with max_distinct > 1 are likely repeating sections or multi-select questions.")
		message("We can help combine them - e.g. join with commas, keep first value, sum numbers, etc.")
	}

	invisible(display_df)
}






##############



#' @noRd
#'
ask_strategy_for_column <- function(col_name, max_distinct, preview, col_data) {
	if (!interactive()) {
		stop("ask_strategy_for_column() requires an interactive R session.")
	}

	cat("\014")  # clear console - feels cleaner for non-coders
	cat("--------------------------------------------------------------\n")
	cat(crayon::blurred("Column:"), crayon::bgWhite( crayon::black(col_name), "\n"))
	cat(crayon::blurred("Found up to"), crayon::red(max_distinct), "different answers per person\n")
	cat(crayon::blurred("Example values:"), preview, "\n\n")

	# Quick type guess
	is_numeric <- all(is.numeric(col_data) | is.na(col_data))
	is_logical_like <- all(col_data %in% c(TRUE, FALSE, NA, 0, 1, "Yes", "No", "yes", "no", "") | is.na(col_data))
	yesno_patterns <- c(
		"Yes", "yes", "YES", "Y", "y",
		"No", "no", "NO", "N", "n",
		"True", "true", "TRUE",
		"False", "false", "FALSE",
		"", NA, " ", "None", "none", "N/A", "n/a", "NA"
	)

	prop_boring <- mean(col_data %in% yesno_patterns, na.rm = TRUE)
	n_unique <- n_distinct(na.omit(col_data))

	looks_checkbox <- prop_boring > 0.80 && n_unique <= 8

	cat("This looks like: ")
	if (looks_checkbox)      cat(crayon::blue("probably checkboxes or Yes/No\n"))
	else if (is_numeric)     cat(crayon::green("numeric values\n"))
	else if (is_logical_like) cat(crayon::magenta("true/false or yes/no style\n"))
	else                     cat(crayon::yellow("free text or categories\n"))

	cat(crayon::underline(crayon::bold("\nHow should we combine multiple answers for this question?\n\n")))

	options <- c(
		"1" = "Concatenate with commas (e.g. Apples, Bananas, Oranges)",
		"2" = "Concatenate with semicolons",
		"3" = "Keep only the first answer",
		"4" = "Keep only the last answer",
		"5" = "Sum the values (only if numbers)",
		"6" = "Count how many answers were given (ignores what they are)",
		"7" = "Skip - leave as is (keep all duplicate rows)",
		"9" = "Remove this column completely from the final table",
		"s" = "Show 5 full example rows for this column",
		"q" = "Quit and return current data"
	)

	if (is_numeric) {
		options <- c(options, "m" = "Calculate the mean/average")
	}
	if (looks_checkbox) {
		options <- c("c" = "Count how many 'Yes' answers (or equivalent)")
	}

	# Print numbered options
	walk2(names(options), options, ~ cat(crayon::cyan(.x), ": ", .y, "\n"))

	cat("\nType number/letter or press Enter for default (usually 1)\n> ")

	choice <- readline()

	if (choice == "") choice <- "1"
	if (choice == "s") {
		# TODO: show sample rows - we can add this later
		cat("Sample view not implemented yet - please choose an option.\n")
		choice <- readline("Enter choice: ")
	}
	if (choice == "q") {
		return(list(action = "quit"))
	}

	strategy <- switch(choice,
										 "1" = "concat_comma",
										 "2" = "concat_semicolon",
										 "3" = "first",
										 "4" = "last",
										 "5" = "sum",
										 "6" = "count_non_na",
										 "m" = "mean",
										 "c" = "count_yes",
										 "7" = "skip",
										 "9" = "remove",              #  new
										 "s" = { "skip" },
										 "q" = { return(list(action = "quit")) },
										 { message("Invalid choice - defaulting to concatenate with commas"); "concat_comma" }
	)

	list(
		column = col_name,
		strategy = strategy,
		user_choice = choice
	)
}



#################


#' Apply chosen strategy to collapse multi-value column within each submission
#'
#' Internal dispatcher - do not export.
#'
#' @param data data.frame or tibble
#' @param col_name bare column name (symbol or string)
#' @param id_col bare or quoted name of submission ID column
#' @param strategy string name of strategy
#' @param ... passed to specific clean_* functions
#'
#' @return Modified data (with one value per group in the target column)
#' @noRd

apply_strategy <- function(data, col_name, id_col, strategy, ...) {

	col_sym  <- sym(col_name)
	id_sym   <- sym(id_col)

	switch(strategy,
				 "concat_comma"     = clean_concat(data, !!col_sym, !!id_sym, sep = ", ", ...),
				 "concat_semicolon" = clean_concat(data, !!col_sym, !!id_sym, sep = "; ", ...),
				 "first"            = clean_first( data, !!col_sym, !!id_sym, ...),
				 "last"             = clean_last( data, !!col_sym, !!id_sym, ...),
				 "sum"              = clean_aggregate(data, !!col_sym, !!id_sym, fun = sum, na.rm = TRUE, ...),
				 "mean"             = clean_aggregate(data, !!col_sym, !!id_sym, fun = mean, na.rm = TRUE, ...),
				 "count_non_na"     = clean_count_non_na(data, !!col_sym, !!id_sym, ...),
				 "count_yes"        = clean_count_yes(data, !!col_sym, !!id_sym, ...),
				 "skip"             = data,
				 {
				 	warning("Unknown strategy '", strategy, "' - skipping column")
				 	data
				 }
	)
}


#' Concatenate values within each group with a separator
#' @noRd
clean_concat <- function(data, col, id_col, sep = ", ", na.rm = TRUE, na_string = "") {

	col <- ensym(col)
	id_col <- ensym(id_col)

	if (na.rm) {
		data <- data %>% mutate(!!col := replace(!!col, is.na(!!col), na_string))
	}

	collapsed <- data %>%
		group_by(!!id_col) %>%
		summarise(
			!!col := paste(!!col, collapse = sep),
			.groups = "drop"
		)

	# Join back - keeps original row structure but replaces multi-rows with one
	# (we'll handle row deduplication in the main function)
	data %>%
		select(-!!col) %>%
		distinct() %>%               # keep one row per submission
		left_join(collapsed, by = as.character(ensym(id_col)))
}


#' Keep first / last value per group
#' @noRd
clean_first <- function(data, col, id_col) {
	col <- ensym(col)
	id_col <- ensym(id_col)

	data %>%
		group_by(!!id_col) %>%
		arrange(.data[[as.character(id_col)]]) %>%  # stable sort if needed
		slice(1) %>%
		ungroup()
}


clean_last <- function(data, col, id_col) {
	col <- ensym(col)
	id_col <- ensym(id_col)

	data %>%
		group_by(!!id_col) %>%
		arrange(desc(row_number())) %>%   # crude but works
		slice(1) %>%
		ungroup()
}


#' Numeric aggregation (sum, mean, etc.)
#' @noRd
clean_aggregate <- function(data, col, id_col, fun = sum, na.rm = TRUE) {

	col <- ensym(col)
	id_col <- ensym(id_col)

	# Coerce to numeric if possible
	data <- data %>% mutate(!!col := suppressWarnings(as.numeric(!!col)))

	collapsed <- data %>%
		group_by(!!id_col) %>%
		summarise(
			!!col := fun(!!col, na.rm = na.rm),
			.groups = "drop"
		)

	data %>%
		select(-!!col) %>%
		distinct() %>%
		left_join(collapsed, by = as.character(ensym(id_col)))
}


#' Count non-missing values per submission
#' @noRd
clean_count_non_na <- function(data, col, id_col) {

	col <- ensym(col)
	id_col <- ensym(id_col)

	collapsed <- data %>%
		group_by(!!id_col) %>%
		summarise(
			!!col := sum(!is.na(!!col)),
			.groups = "drop"
		)

	data %>%
		select(-!!col) %>%
		distinct() %>%
		left_join(collapsed, by = as.character(ensym(id_col)))
}


#' Count "positive" responses (Yes / TRUE / 1 / checked, etc.)
#' @noRd
clean_count_yes <- function(data, col, id_col,
														positive_values = c("Yes", "yes", "YES", "Y", "1", TRUE, 1)) {

	col <- ensym(col)
	id_col <- ensym(id_col)

	collapsed <- data %>%
		group_by(!!id_col) %>%
		summarise(
			!!col := sum(!!col %in% positive_values, na.rm = TRUE),
			.groups = "drop"
		)

	data %>%
		select(-!!col) %>%
		distinct() %>%
		left_join(collapsed, by = as.character(ensym(id_col)))
}


