#' Summarize a single field in a FormIO response dataset
#'
#' Designed for non-technical users: it accepts either a data frame or the list
#' produced by [flatten_submission_records()] and returns a simple summary. Numeric
#' fields get descriptive statistics; categorical fields get counts and percents.
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param field Column name or number to summarize.
#' @param top_n For categorical fields, the number of top values to return
#'   (default 10). Use `NULL` to return all values.
#' @param include_na Logical. If `TRUE`, include missing values as "(Missing)"
#'   in categorical summaries.
#' @param digits Number of decimal places for percentages/statistics.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{field}{Resolved column name}
#'   \item{type}{Detected type: "numeric", "categorical", or "date"}
#'   \item{total}{Total rows}
#'   \item{missing}{Missing value count}
#'   \item{distinct}{Distinct non-missing values}
#'   \item{summary}{Data frame of summary stats or counts}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc"))
#' summarize_field_distribution(flat, "age")
#' summarize_field_distribution(flat, "favorite_food", top_n = 5)
#' }
summarize_field_distribution <- function(
		x,
		field,
		top_n = 10,
		include_na = FALSE,
		digits = 2,
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	df <- extract_flat_df(x) # Normalize input to a flat data frame.
	field_name <- resolve_field_name(df, field) # Resolve column name from index/label.
	values <- df[[field_name]] # Extract the target column.
	total <- length(values) # Total number of rows.

	type <- detect_field_type(values) # Detect the field type for summary logic.

	if (type == "numeric") {
		values_num <- values # Start with raw values.
		if (is.factor(values_num)) values_num <- as.character(values_num) # Preserve numeric text in factors.
		num <- suppressWarnings(as.numeric(values_num)) # Coerce to numeric safely.
		missing <- sum(is.na(num)) # Count missing values.
		distinct <- length(unique(num[!is.na(num)])) # Count distinct non-missing values.

		stats <- c(
			n = sum(!is.na(num)),
			missing = missing,
			missing_pct = if (total == 0) NA_real_ else (missing / total) * 100,
			mean = if (all(is.na(num))) NA_real_ else mean(num, na.rm = TRUE),
			median = if (all(is.na(num))) NA_real_ else stats::median(num, na.rm = TRUE),
			sd = if (all(is.na(num))) NA_real_ else stats::sd(num, na.rm = TRUE),
			min = if (all(is.na(num))) NA_real_ else min(num, na.rm = TRUE),
			p25 = if (all(is.na(num))) NA_real_ else stats::quantile(num, 0.25, na.rm = TRUE, names = FALSE),
			p75 = if (all(is.na(num))) NA_real_ else stats::quantile(num, 0.75, na.rm = TRUE, names = FALSE),
			max = if (all(is.na(num))) NA_real_ else max(num, na.rm = TRUE)
		)

		summary <- data.frame(
			metric = names(stats),
			value = round(stats, digits),
			stringsAsFactors = FALSE
		)
	} else if (type == "date") {
		times <- coerce_time(values, tz = "UTC") # Normalize to POSIXct.
		missing <- sum(is.na(times)) # Count missing values.
		distinct <- length(unique(times[!is.na(times)])) # Count distinct non-missing timestamps.

		min_val <- if (all(is.na(times))) NA_character_ else format(min(times, na.rm = TRUE), "%Y-%m-%d %H:%M:%S") # Earliest timestamp.
		max_val <- if (all(is.na(times))) NA_character_ else format(max(times, na.rm = TRUE), "%Y-%m-%d %H:%M:%S") # Latest timestamp.

		stats <- c(
			n = sum(!is.na(times)),
			missing = missing,
			missing_pct = if (total == 0) NA_real_ else (missing / total) * 100,
			min = min_val,
			max = max_val
		)

		summary <- data.frame(
			metric = names(stats),
			value = unname(stats),
			stringsAsFactors = FALSE
		)
	} else {
		vals <- values # Start from raw values for categorical summary.
		if (include_na) {
			vals <- ifelse(is.na(vals), "(Missing)", as.character(vals)) # Keep missing values as explicit label.
		} else {
			vals <- as.character(vals[!is.na(vals)]) # Drop missing values.
		}

		if (length(vals) == 0) {
			counts <- integer(0) # No values to tabulate.
			missing <- sum(is.na(values)) # Count missing values.
			distinct <- 0 # No distinct values available.
		} else {
			counts <- sort(table(vals), decreasing = TRUE) # Frequency table of values.
			missing <- sum(is.na(values)) # Count missing values.
			distinct <- length(unique(values[!is.na(values)])) # Count distinct non-missing values.
		}

		if (!is.null(top_n) && top_n > 0 && length(counts) > top_n) {
			counts <- counts[seq_len(top_n)] # Keep only the top N values.
		}

		percent <- if (length(counts) == 0) numeric(0) else as.numeric(counts) / sum(counts) * 100 # Compute percents.

		summary <- data.frame(
			value = names(counts),
			count = as.integer(counts),
			percent = round(percent, digits),
			stringsAsFactors = FALSE
		)
	}

	out <- list( # Package summary outputs.
		field = field_name,
		type = type,
		total = total,
		missing = missing,
		distinct = distinct,
		summary = summary
	)

	if (!quiet) {
		message("Summarized ", field_name, " (", type, ").")
	}

	if (audit_depth == 1) {
		maybe_write_audit("summarize_field_distribution", details = paste0("field=", field_name), data = df) # Record the summary action.
	}

	out # Return the summary list.
}


#' Cross-tabulate two fields
#'
#' Builds a simple cross-tabulation between two columns, returning both a
#' wide table and a long table that includes counts and (optional) percents.
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param row Column name or number for the rows.
#' @param col Column name or number for the columns.
#' @param include_na Logical. If `TRUE`, treat missing values as "(Missing)".
#' @param percent How to calculate percentages. One of `"overall"` (default),
#'   `"row"`, `"col"`, or `"none"`.
#' @param digits Number of decimal places for percentages.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{row}{Resolved row field name}
#'   \item{col}{Resolved column field name}
#'   \item{percent}{Percent calculation mode}
#'   \item{table}{Wide counts table as a data frame}
#'   \item{long}{Long data frame with counts and percents}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabulate_field_by_group(flat, "region", "program", percent = "row")
#' }
tabulate_field_by_group <- function(
		x,
		row,
		col,
		include_na = FALSE,
		percent = c("overall", "row", "col", "none"),
		digits = 1,
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	percent <- match.arg(percent) # Validate percent option.

	df <- extract_flat_df(x) # Normalize input to a flat data frame.
	row_name <- resolve_field_name(df, row) # Resolve the row field name.
	col_name <- resolve_field_name(df, col) # Resolve the column field name.

	row_vals <- df[[row_name]] # Extract row values.
	col_vals <- df[[col_name]] # Extract column values.

	if (include_na) {
		row_vals <- ifelse(is.na(row_vals), "(Missing)", as.character(row_vals)) # Label missing values.
		col_vals <- ifelse(is.na(col_vals), "(Missing)", as.character(col_vals)) # Label missing values.
	} else {
		keep <- !is.na(row_vals) & !is.na(col_vals) # Drop rows with missing values.
		row_vals <- as.character(row_vals[keep])
		col_vals <- as.character(col_vals[keep])
	}

	xtab <- table(row_vals, col_vals) # Build contingency table.

	wide <- as.data.frame.matrix(xtab, stringsAsFactors = FALSE) # Convert to wide table.
	wide <- cbind(rownames(wide), wide, stringsAsFactors = FALSE) # Preserve row labels as first column.
	colnames(wide)[1] <- row_name # Label the row field column.
	rownames(wide) <- NULL # Drop row names for a clean data frame.

	long_counts <- as.data.frame(xtab, stringsAsFactors = FALSE) # Convert to long counts.
	colnames(long_counts) <- c(row_name, col_name, "count")

	if (percent != "none") {
		if (percent == "overall") norm <- prop.table(xtab) # Overall percent.
		if (percent == "row") norm <- prop.table(xtab, 1) # Row-wise percent.
		if (percent == "col") norm <- prop.table(xtab, 2) # Column-wise percent.

		long_percent <- as.data.frame(as.table(norm), stringsAsFactors = FALSE) # Convert percents to long table.
		colnames(long_percent) <- c(row_name, col_name, "percent")
		long_percent$percent <- round(long_percent$percent * 100, digits) # Convert to percent scale.

		long <- merge(long_counts, long_percent, by = c(row_name, col_name), all = TRUE) # Combine counts and percents.
	} else {
		long <- long_counts # Keep counts only.
	}

	out <- list( # Package output tables.
		row = row_name,
		col = col_name,
		percent = percent,
		table = wide,
		long = long
	)

	if (!quiet) {
		message("Cross-tabulated ", row_name, " by ", col_name, ".")
	}

	if (audit_depth == 1) {
		maybe_write_audit(
			"tabulate_field_by_group",
			details = paste0("row=", row_name, "; col=", col_name),
			data = df
		)
	}

	out # Return the cross-tab results.
}


#' Summarize responses over time
#'
#' Counts responses per day/week/month (or hour), using a timestamp column.
#' If `date_col` is `NULL`, the function tries common timestamp names
#' automatically (e.g., `created`, `modified`, `_createdAt`).
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param date_col Column name or number for the date/time field. If `NULL`,
#'   tries to guess a reasonable timestamp column.
#' @param interval One of `"day"` (default), `"week"`, `"month"`, or `"hour"`.
#' @param tz Time zone to use for parsing dates (default `"UTC"`).
#' @param start Optional start date/time to filter the range. Accepts a `Date`,
#'   `POSIXct`, or an ISO-8601 string (e.g., `"2024-03-01"` or
#'   `"2024-03-01 14:30:00"`).
#' @param end Optional end date/time to filter the range. Accepts a `Date`,
#'   `POSIXct`, or an ISO-8601 string (e.g., `"2024-03-31"` or
#'   `"2024-03-31 23:59:59"`).
#' @param start_date Alias for `start` (useful for readability). Same formats
#'   as `start`.
#' @param end_date Alias for `end` (useful for readability). Same formats
#'   as `end`.
#' @param include_empty Logical. If `TRUE`, fill missing periods with zeroes.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Data frame with `period` and `count` columns}
#'   \item{summary}{List with metadata about the calculation}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_response_timeline(flat, date_col = "created", interval = "week")
#' }
summarize_response_timeline <- function(
		x,
		date_col = NULL,
		interval = c("day", "week", "month", "hour"),
		tz = "UTC",
		start = NULL,
		end = NULL,
		start_date = NULL,
		end_date = NULL,
		include_empty = TRUE,
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	interval <- match.arg(interval) # Validate interval choice.

	df <- extract_flat_df(x) # Normalize input to a flat data frame.

	if (is.null(date_col)) {
		date_col <- guess_time_col(names(df)) # Try common timestamp columns.
	}
	if (is.null(date_col)) {
		stop("Could not find a date column. Please set date_col.")
	}
	date_name <- resolve_field_name(df, date_col) # Resolve the date column name.

	times <- coerce_time(df[[date_name]], tz = tz) # Parse times with the given time zone.
	if (all(is.na(times))) {
		stop("No valid dates found in column '", date_name, "'.")
	}

	start_was_null <- is.null(start) && is.null(start_date) # Track whether bounds were provided.
	end_was_null <- is.null(end) && is.null(end_date)
	if (is.null(start) && !is.null(start_date)) start <- start_date # Allow alias.
	if (is.null(end) && !is.null(end_date)) end <- end_date # Allow alias.
	if (!is.null(start)) start <- coerce_time(start, tz = tz) # Parse start time if provided.
	if (!is.null(end)) end <- coerce_time(end, tz = tz) # Parse end time if provided.

	keep <- !is.na(times) # Start from non-missing timestamps.
	if (!is.null(start)) keep <- keep & times >= start # Apply lower bound when provided.
	if (!is.null(end)) keep <- keep & times <= end # Apply upper bound when provided.
	times <- times[keep]

	if (length(times) == 0) {
		stop("No dates remain after applying start/end filters.")
	}

	if (is.null(start)) start <- min(times, na.rm = TRUE) # Default to earliest remaining time.
	if (is.null(end)) end <- max(times, na.rm = TRUE) # Default to latest remaining time.

	periods <- bucket_time(times, interval = interval, tz = tz) # Bucket times into periods.
	xtab <- table(periods) # Count submissions per period.

	period_values <- names(xtab) # Extract period labels.
	if (interval == "hour") {
		period_values <- as.POSIXct(period_values, tz = tz) # Convert hourly labels to POSIXct.
	} else {
		period_values <- as.Date(period_values) # Convert day/week/month labels to Date.
	}

	out_df <- data.frame( # Build the output table.
		period = period_values,
		count = as.integer(xtab),
		stringsAsFactors = FALSE
	)

	if (include_empty) {
		seq_start <- bucket_time(start, interval = interval, tz = tz) # Align start to bucket boundary.
		seq_end <- bucket_time(end, interval = interval, tz = tz) # Align end to bucket boundary.
		full_seq <- build_time_sequence(seq_start, seq_end, interval = interval, tz = tz) # Full sequence of periods.
		full_df <- data.frame(period = full_seq, stringsAsFactors = FALSE)

		key <- as.character(full_df$period) # Use string keys for matching counts.
		counts <- as.integer(xtab)
		names(counts) <- names(xtab)
		full_df$count <- counts[key] # Fill counts by matching period labels.
		full_df$count[is.na(full_df$count)] <- 0L # Replace missing counts with zero.

		out_df <- full_df # Use the full sequence with zero-filled gaps.

		# Trim leading/trailing empty periods when bounds were not explicitly set.
		if (start_was_null && end_was_null) {
			nonzero <- which(out_df$count > 0)
			if (length(nonzero) > 0) {
				out_df <- out_df[min(nonzero):max(nonzero), , drop = FALSE]
			}
		}
	}

	out_df <- out_df[order(out_df$period), , drop = FALSE] # Ensure chronological order.

	out <- list( # Package output and summary metadata.
		data = out_df,
		summary = list(
			date_col = date_name,
			interval = interval,
			start = start,
			end = end,
			total = length(df[[date_name]]),
			used = length(times)
		)
	)

	if (!quiet) {
		message("Built timeline for ", date_name, " (", interval, ").")
	}

	if (audit_depth == 1) {
		maybe_write_audit(
			"summarize_response_timeline",
			details = paste0("date_col=", date_name, "; interval=", interval),
			data = df
		)
	}

	out # Return the timeline list.
}


#' Plot a histogram for a numeric field
#'
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param field Column name(s) or number(s) to plot. When multiple columns are
#'   provided, their text is combined into one wordcloud.
#' @param bins Histogram breaks passed to [hist()]. Default "Sturges".
#' @param include_na Logical. If `TRUE`, keep `NA` values (ignored by `hist`).
#' @param main Optional plot title.
#' @param xlab Optional x-axis label.
#' @param col Bar fill color.
#' @param border Bar border color.
#' @param plot Logical. If `FALSE`, return histogram data without plotting.
#' @param ... Additional arguments passed to [hist()].
#'
#' @return Invisibly returns a list with the field name and histogram object.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_numeric_distribution(flat, "age")
#' }
plot_numeric_distribution <- function(
		x,
		field,
		bins = "Sturges",
		include_na = FALSE,
		main = NULL,
		xlab = NULL,
		col = "steelblue",
		border = "white",
		plot = TRUE,
		...
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	df <- extract_flat_df(x) # Normalize input to a flat data frame.
	field_name <- resolve_field_name(df, field) # Resolve the field name.
	values <- df[[field_name]] # Extract the target column.

	if (is.factor(values)) values <- as.character(values) # Preserve numeric text in factors.
	num <- suppressWarnings(as.numeric(values)) # Coerce to numeric safely.
	if (!include_na) num <- num[!is.na(num)] # Optionally drop missing values.
	if (all(is.na(num))) {
		stop("Field '", field_name, "' does not contain numeric values.")
	}

	if (is.null(main)) main <- paste("Histogram of", field_name) # Default title.
	if (is.null(xlab)) xlab <- field_name # Default x-axis label.

	if (isTRUE(plot)) {
		h <- graphics::hist( # Compute and draw the histogram.
			num,
			breaks = bins,
			main = main,
			xlab = xlab,
			col = col,
			border = border,
			plot = TRUE,
			...
		)
	} else {
		# hist() warns about main/xlab/col/border when plot = FALSE, so only pass
		# arguments that affect the returned binning/counts.
		h <- graphics::hist(num, breaks = bins, plot = FALSE, ...)
	}

	out <- list(field = field_name, hist = h) # Package output.

	if (audit_depth == 1) {
		maybe_write_audit("plot_numeric_distribution", details = paste0("field=", field_name), data = df) # Record the plot action.
	}

	invisible(out) # Return invisibly to avoid console spam.
}


#' Plot a bar chart for a categorical field
#'
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param field Column name or number to plot.
#' @param top_n Number of categories to show (default 10). Use `NULL` for all.
#' @param include_na Logical. If `TRUE`, include missing values as "(Missing)".
#' @param horiz Logical. If `TRUE`, draw horizontal bars.
#' @param main Optional plot title.
#' @param xlab Optional x-axis label.
#' @param col Bar fill color.
#' @param plot Logical. If `FALSE`, return bar data without plotting.
#' @param ... Additional arguments passed to [barplot()].
#'
#' @return Invisibly returns a list with the field name and bar data.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_categorical_summary(flat, "region")
#' }
plot_categorical_summary <- function(
		x,
		field,
		top_n = 10,
		include_na = FALSE,
		horiz = FALSE,
		main = NULL,
		xlab = NULL,
		col = "steelblue",
		plot = TRUE,
		...
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	summary <- summarize_field_distribution( # Reuse summary logic for counts and percents.
		x = x,
		field = field,
		top_n = top_n,
		include_na = include_na,
		quiet = TRUE
	)

	if (summary$type %in% c("numeric", "date")) {
		stop("Field '", summary$field, "' is not categorical. Use plot_numeric_distribution() or plot_response_timeline().")
	}

	data <- summary$summary # Extract the summary table.
	if (nrow(data) == 0) {
		stop("No values available to plot for field '", summary$field, "'.")
	}

	if (is.null(main)) main <- paste("Top", nrow(data), summary$field) # Default plot title.
	if (is.null(xlab)) xlab <- if (horiz) "Count" else "" # Default x-axis label.

	if (plot) {
		graphics::barplot( # Draw the bar chart.
			height = data$count,
			names.arg = data$value,
			horiz = horiz,
			las = if (horiz) 1 else 2,
			col = col,
			main = main,
			xlab = xlab,
			...
		)
	}

	out <- list(field = summary$field, data = data) # Package output.

	if (audit_depth == 1) {
		maybe_write_audit("plot_categorical_summary", details = paste0("field=", summary$field), data = x) # Record the plot action.
	}

	invisible(out) # Return invisibly to avoid console spam.
}


#' Plot a wordcloud for a text field
#'
#' Requires the optional `wordcloud` package. If it is not installed, the
#' function will stop with a helpful message.
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param field Column name or number to plot.
#' @param max_words Maximum number of words to display.
#' @param min_freq Minimum frequency to keep a word.
#' @param min_chars Minimum character length for a word to be kept.
#' @param remove_stopwords Logical. If `TRUE`, remove common stopwords.
#' @param stopwords Character vector of stopwords to remove.
#' @param seed Optional random seed for reproducible layout.
#' @param colors Vector of colors passed to [wordcloud::wordcloud()].
#' @param ... Additional arguments passed to [wordcloud::wordcloud()].
#'
#' @return Invisibly returns a list with the field name(s) and word frequencies.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_word_cloud(flat, "feedback")
#' }
plot_word_cloud <- function(
		x,
		field,
		max_words = 100,
		min_freq = 1,
		min_chars = 2,
		remove_stopwords = TRUE,
		stopwords = default_stopwords(),
		seed = NULL,
		colors = NULL,
		...
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	if (!requireNamespace("wordcloud", quietly = TRUE)) {
		stop("Package 'wordcloud' is required. Install it with install.packages('wordcloud').")
	}

	df <- extract_flat_df(x) # Normalize input to a flat data frame.
	field_names <- resolve_field_names(df, field) # Resolve one or more field names.
	values <- unlist(df[field_names], use.names = FALSE) # Combine values across fields.

	words <- tokenize_words(values, min_chars = min_chars) # Tokenize to individual words.
	if (remove_stopwords && length(words) > 0) {
		words <- words[!words %in% stopwords] # Remove stopwords if requested.
	}

	if (length(words) == 0) {
		stop("No words available to plot for the selected field(s).")
	}

	freq <- sort(table(words), decreasing = TRUE) # Count word frequencies.
	freq <- freq[freq >= min_freq] # Apply minimum frequency filter.
	if (!is.null(max_words) && length(freq) > max_words) {
		freq <- freq[seq_len(max_words)] # Keep only the top N words.
	}

	if (is.null(colors)) colors <- grDevices::rainbow(8) # Default color palette.
	if (!is.null(seed)) set.seed(seed) # Set seed for reproducible layout.

	wordcloud::wordcloud( # Draw the wordcloud.
		words = names(freq),
		freq = as.numeric(freq),
		colors = colors,
		...
	)

	out <- list(fields = field_names, freq = freq) # Package output.

	if (audit_depth == 1) {
		maybe_write_audit("plot_word_cloud", details = paste0("field=", paste(field_names, collapse = ", ")), data = df) # Record the plot action.
	}

	invisible(out) # Return invisibly to avoid console spam.
}


#' Plot a response timeline
#'
#' Convenience wrapper around [summarize_response_timeline()] that draws a simple line
#' chart using base graphics.
#' If audit logging is active (see [start_audit_log()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param date_col Column name or number for the date/time field. If `NULL`,
#'   tries to guess a reasonable timestamp column.
#' @param interval One of `"day"` (default), `"week"`, `"month"`, or `"hour"`.
#' @param tz Time zone to use for parsing dates (default `"UTC"`).
#' @param start Optional start date/time to filter the range. Accepts a `Date`,
#'   `POSIXct`, or an ISO-8601 string (e.g., `"2024-03-01"` or
#'   `"2024-03-01 14:30:00"`).
#' @param end Optional end date/time to filter the range. Accepts a `Date`,
#'   `POSIXct`, or an ISO-8601 string (e.g., `"2024-03-31"` or
#'   `"2024-03-31 23:59:59"`).
#' @param start_date Alias for `start` (useful for readability). Same formats
#'   as `start`.
#' @param end_date Alias for `end` (useful for readability). Same formats
#'   as `end`.
#' @param include_empty Logical. If `TRUE`, fill missing periods with zeroes.
#' @param main Optional plot title.
#' @param xlab Optional x-axis label.
#' @param ylab Optional y-axis label.
#' @param col Line color.
#' @param lwd Line width.
#' @param type Plot type passed to [plot()]. Default "l".
#' @param plot Logical. If `FALSE`, return timeline data without plotting.
#' @param ... Additional arguments passed to [plot()].
#'
#' @return Invisibly returns the output of [summarize_response_timeline()].
#' @export
#'
#' @examples
#' \dontrun{
#' plot_response_timeline(flat, date_col = "created", interval = "month")
#' }
plot_response_timeline <- function(
		x,
		date_col = NULL,
		interval = c("day", "week", "month", "hour"),
		tz = "UTC",
		start = NULL,
		end = NULL,
		start_date = NULL,
		end_date = NULL,
		include_empty = TRUE,
		main = NULL,
		xlab = NULL,
		ylab = "Responses",
		col = "steelblue",
		lwd = 2,
		type = "l",
		plot = TRUE,
		...
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	interval <- match.arg(interval) # Validate interval choice.

	out <- summarize_response_timeline( # Reuse timeline logic for data prep.
		x = x,
		date_col = date_col,
		interval = interval,
		tz = tz,
		start = start,
		end = end,
		start_date = start_date,
		end_date = end_date,
		include_empty = include_empty,
		quiet = TRUE
	)

	if (plot) {
		data <- out$data # Extract timeline counts.
		if (nrow(data) == 0) {
			stop("No timeline data available to plot.")
		}

		if (is.null(main)) main <- paste("Responses by", interval) # Default title.
		if (is.null(xlab)) xlab <- "" # Default x-axis label.

		x_vals <- data$period # X-axis values (periods).
		plot( # Draw the timeline chart.
			x = x_vals,
			y = data$count,
			type = type,
			col = col,
			lwd = lwd,
			main = main,
			xlab = xlab,
			ylab = ylab,
			...
		)
	}

	if (audit_depth == 1) {
		maybe_write_audit("plot_response_timeline", details = paste0("interval=", interval), data = x) # Record the plot action.
	}

	invisible(out) # Return invisibly to avoid console spam.
}


# ---- Internal helpers -----------------------------------------------------------

#' Resolve a single field name
#'
#' Converts a numeric index or character label into a validated column name.
#'
#' @param df Data frame of responses.
#' @param field Column name or index.
#' @return A valid column name from `df`.
#' @keywords internal
resolve_field_name <- function(df, field) {
	if (is.numeric(field)) {
		field_name <- names(df)[field] # Translate numeric index to column name.
	} else {
		field_name <- as.character(field) # Treat input as a column name.
	}
	if (is.na(field_name) || !field_name %in% names(df)) {
		stop("field '", field, "' not found in data")
	}
	field_name # Return the validated column name.
}

#' Resolve one or more field names
#'
#' Converts numeric indices or character labels into validated column names.
#'
#' @param df Data frame of responses.
#' @param field Column name(s) or index/indices.
#' @return A character vector of valid column names.
#' @keywords internal
resolve_field_names <- function(df, field) {
	if (length(field) == 0) stop("field must include at least one column.")
	if (is.numeric(field)) {
		field_names <- names(df)[field] # Translate numeric indices to names.
	} else {
		field_names <- as.character(field) # Treat input as column names.
	}
	if (any(is.na(field_names)) || any(!field_names %in% names(df))) {
		bad <- field_names[is.na(field_names) | !field_names %in% names(df)] # Identify missing columns.
		stop("field(s) not found in data: ", paste(bad, collapse = ", "))
	}
	field_names # Return validated column names.
}


#' Detect field type
#'
#' Classifies a vector as "numeric", "date", or "categorical".
#'
#' @param values A vector of field values.
#' @return A character string describing the type.
#' @keywords internal
detect_field_type <- function(values) {
	if (inherits(values, "Date") || inherits(values, "POSIXt")) {
		return("date")
	}
	if (is.numeric(values)) {
		return("numeric")
	}
	if (is.factor(values)) {
		if (is_numeric_like(as.character(values))) return("numeric")
		return("categorical")
	}
	if (!is.logical(values) && is_numeric_like(values)) {
		return("numeric")
	}
	"categorical"
}


#' Coerce values to POSIXct
#'
#' Tries to parse dates/times into POSIXct, falling back to Date parsing.
#'
#' @param values Vector of values to parse.
#' @param tz Time zone for parsing.
#' @return A POSIXct vector (possibly with `NA` values).
#' @keywords internal
coerce_time <- function(values, tz = "UTC") {
	if (is.factor(values)) values <- as.character(values) # Preserve numeric text in factors.
	if (inherits(values, "POSIXt")) {
		return(as.POSIXct(values, tz = tz)) # Normalize timezone on existing POSIXct.
	}
	if (inherits(values, "Date")) {
		return(as.POSIXct(values, tz = tz)) # Convert Date to POSIXct.
	}

	times <- suppressWarnings(as.POSIXct(values, tz = tz)) # Try to parse timestamps.
	if (all(is.na(times))) {
		dates <- suppressWarnings(as.Date(values)) # Fallback: parse as Date.
		times <- as.POSIXct(dates, tz = tz) # Convert Date to POSIXct.
	}
	times # Return parsed timestamps.
}


#' Bucket times into intervals
#'
#' Converts timestamps into day/week/month/hour buckets.
#'
#' @param times POSIXct vector of times.
#' @param interval One of "day", "week", "month", or "hour".
#' @param tz Time zone for bucketing.
#' @return A vector of bucketed times/dates.
#' @keywords internal
bucket_time <- function(times, interval, tz = "UTC") {
	if (interval == "day") {
		return(as.Date(times, tz = tz))
	}
	if (interval == "week") {
		return(as.Date(cut(times, "week")))
	}
	if (interval == "month") {
		return(as.Date(cut(times, "month")))
	}
	as.POSIXct(cut(times, "hour"), tz = tz)
}


#' Build a full time sequence
#'
#' Generates a sequence of evenly spaced periods from start to end.
#'
#' @param start Start date/time.
#' @param end End date/time.
#' @param interval One of "day", "week", "month", or "hour".
#' @param tz Time zone for POSIXct sequence.
#' @return A sequence of POSIXct or Date values.
#' @keywords internal
build_time_sequence <- function(start, end, interval, tz = "UTC") {
	if (interval == "hour") {
		return(seq.POSIXt(as.POSIXct(start, tz = tz), as.POSIXct(end, tz = tz), by = "hour"))
	}
	start_date <- as.Date(start) # Normalize to Date for day/week/month sequences.
	end_date <- as.Date(end)
	by <- if (interval == "day") "day" else if (interval == "week") "week" else "month" # Choose step size.
	seq.Date(start_date, end_date, by = by) # Build the date sequence.
}


#' Default stopword list
#'
#' Provides a small set of common English stopwords.
#'
#' @return A character vector of stopwords.
#' @keywords internal
default_stopwords <- function() {
	c(
		"a", "an", "and", "are", "as", "at", "be", "but", "by",
		"for", "from", "has", "have", "he", "her", "his", "i",
		"if", "in", "is", "it", "its", "me", "my", "not", "of",
		"on", "or", "our", "she", "so", "that", "the", "their",
		"them", "there", "they", "this", "to", "was", "we", "were",
		"will", "with", "you", "your"
	)
}


#' Tokenize text into words
#'
#' Splits text into lowercase word tokens and filters by minimum length.
#'
#' @param values Vector of text values.
#' @param min_chars Minimum number of characters per token.
#' @return A character vector of word tokens.
#' @keywords internal
tokenize_words <- function(values, min_chars = 2) {
	if (is.factor(values)) values <- as.character(values) # Preserve text in factors.
	text <- paste(values[!is.na(values)], collapse = " ") # Combine non-missing text.
	text <- tolower(text) # Normalize casing.
	text <- gsub("[^a-z0-9']+", " ", text) # Replace punctuation with spaces.
	words <- unlist(strsplit(text, "\\s+")) # Split on whitespace.
	words <- words[words != ""] # Drop empty tokens.
	if (min_chars > 1) {
		words <- words[nchar(words) >= min_chars] # Filter short tokens.
	}
	words # Return tokens.
}
