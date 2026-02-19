#' Rename columns from a flattened FormIO dataset
#'
#' Renames columns in a list returned by [flatten_submission_records()].
#' You can run it interactively (prompt for each column) or non-interactively
#' with `rename_map`.
#'
#' @param x List from [flatten_submission_records()] containing `FlatResponses` and
#'   `ColumnNames`.
#' @param NamesDF Logical. Include the rename table in the output. Default `TRUE`.
#' @param renameDF Logical. Include the renamed flat object in the output.
#'   Default `TRUE`.
#' @param rename_map Optional data frame with `OldNames` and `NewNames` columns
#'   for non-interactive renaming.
#' @param quiet Logical. If `TRUE`, suppresses preview output.
#'
#' @returns A named list with:
#' * `renamedDF`: rename mapping table (`Number`, `OldNames`, `NewNames`)
#' * `flat`: updated flattened object with renamed `FlatResponses`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Interactive
#' rename_columns_from_dictionary(flat)
#'
#' # Non-interactive map
#' map_df <- data.frame(
#'   OldNames = c("submissionId", "age"),
#'   NewNames = c("submission_id", "age_years"),
#'   stringsAsFactors = FALSE
#' )
#' rename_columns_from_dictionary(flat, rename_map = map_df, quiet = TRUE)
#' }
rename_columns_from_dictionary <- function(x, NamesDF = TRUE, renameDF = TRUE, rename_map = NULL, quiet = FALSE) {
  if (!is.list(x) || !"FlatResponses" %in% names(x) || !"ColumnNames" %in% names(x)) {
    stop("x must be output from flatten_submission_records()")
  }
  if (is.null(rename_map) && !interactive()) {
    stop("rename_columns_from_dictionary() is interactive when rename_map is NULL. Provide rename_map for non-interactive use.")
  }

  FlatX <- x
  renamedDF <- tibble::as_tibble(FlatX$ColumnNames)
  if (ncol(renamedDF) < 2) stop("x$ColumnNames must have at least two columns")
  colnames(renamedDF)[1:2] <- c("Number", "OldNames")
  renamedDF$OldNames <- as.character(renamedDF$OldNames)

  if (!is.null(rename_map)) {
    map <- as.data.frame(rename_map, stringsAsFactors = FALSE)
    if (!all(c("OldNames", "NewNames") %in% names(map))) {
      stop("rename_map must include columns 'OldNames' and 'NewNames'.")
    }
    renamedDF$NewNames <- renamedDF$OldNames
    idx <- match(renamedDF$OldNames, map$OldNames)
    has_match <- !is.na(idx)
    renamedDF$NewNames[has_match] <- as.character(map$NewNames[idx[has_match]])
    empty_new <- is.na(renamedDF$NewNames) | renamedDF$NewNames == ""
    renamedDF$NewNames[empty_new] <- renamedDF$OldNames[empty_new]
  } else {
    renamedDF$NewNames <- renamedDF$OldNames
    for (i in seq_len(nrow(renamedDF))) {
      currentcol <- renamedDF$OldNames[i]
      cat("\014")
      cat(green(paste0(i, "/", nrow(renamedDF))))
      cat("\n")
      cat("Provide a new name for:" %+% blurred(italic("(Leave blank to use same name)")))
      cat("\n")
      cat("-> " %+% bgWhite(black(currentcol)))
      ask <- readline()
      newname <- if (identical(ask, "")) currentcol else ask
      renamedDF$NewNames[i] <- newname
    }
  }

  colnames(FlatX$FlatResponses) <- renamedDF$NewNames

  if (!quiet) {
    print(renamedDF, n = min(100, nrow(renamedDF)))
  }

  out <- list()
  if (isTRUE(NamesDF)) out$renamedDF <- renamedDF
  if (isTRUE(renameDF)) out$flat <- FlatX
  out
}
