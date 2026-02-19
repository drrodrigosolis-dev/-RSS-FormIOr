#' Identify columns with multiple distinct values per submission
#'
#' Creates a diagnostic data frame with the same shape as the flattened responses,
#' where each original value (except in the ID column) is replaced by the number
#' of **distinct non-NA values** that appear in that column for the corresponding
#' submission. This makes it easy to detect which questions/fields caused row
#' duplication due to repeated sections, multi-select choices, or repeating groups.
#'
#' @param x A list returned by [flatten_submission_records()], containing at minimum
#'   `$FlatResponses` (the flattened data frame) and optionally `$ColumnNames`.
#' @param id_col Integer. The column number that contains the unique submission
#'   identifier (usually `submissionId`). Default = 1 (first column).
#'
#' @return A data frame with the **same dimensions and column names** as
#'   `x$FlatResponses`, but where (starting from the second column) every cell
#'   contains the count of distinct non-NA values in that column for the given
#'   submission. Counts = 1 indicate constant/single-value fields; counts > 1
#'   indicate multi-value fields that are likely causing duplicated rows.
#'
#' @details
#' This function is typically used before [resolve_duplicate_values()] to help non-technical users
#' identify which columns need special handling (e.g. concatenate, pick first,
#' sum, pivot wider, etc.).
#'
#' Columns that are constant within each submission will show `1` everywhere
#' (or `NA` if the column is entirely missing for that submission).
#'
#' @return A tibble (or data.frame) matching the structure of `x$FlatResponses`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc..."))
#'
#' # See which columns have multiple values per submission
#' multi_counts <- count_multivalue_fields(flat)
#'
#' # Quick check: which columns ever have more than one distinct value?
#' multi_counts |>
#'   summarise(across(-1, ~ max(.x, na.rm = TRUE))) |>
#'   pivot_longer(everything(), names_to = "column", values_to = "max_distinct") |>
#'   filter(max_distinct > 1)
#' }
#'
#' @seealso [flatten_submission_records()], [fetch_form_responses()], [resolve_duplicate_values()]


count_multivalue_fields <- function(x, id_col = 1) {
	xresp <- x$FlatResponses
	xDups <- xresp
	id_sym <- sym(names(xDups)[id_col])

	for (i in setdiff(seq_along(xDups), id_col)) {
		field_sym <- sym(names(xDups)[i])
		xDups <- xDups %>%
			group_by(!!id_sym) %>%
			mutate({{field_sym}} := n_distinct(!!field_sym, na.rm = TRUE)) %>%
			ungroup()
	}

	return(xDups)
}





