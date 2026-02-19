# Handbook doc checker: validates named arguments in code blocks
# Usage: source("inst/extdata/handbook/maintenance/check_docs.R")
# Then call: formior_check_doc_args()

formior_check_doc_args <- function(root = getwd(), stop_on_error = FALSE, quiet = FALSE) {
  if (!requireNamespace("pkgload", quietly = TRUE) && !requireNamespace("devtools", quietly = TRUE)) {
    stop("Please install 'pkgload' or 'devtools' to run the documentation checker.")
  }

  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(root, quiet = TRUE)
  } else {
    devtools::load_all(root, quiet = TRUE)
  }

  ns_lines <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  exports <- sub("^export\\(([^)]+)\\)$", "\\1", ns_lines[grepl("^export\\(", ns_lines)])
  exports <- unique(exports)

  formals_map <- list()
  for (fn in exports) {
    if (exists(fn, mode = "function")) {
      f <- get(fn, mode = "function")
      formals_map[[fn]] <- names(formals(f))
    }
  }

  extract_code_blocks <- function(lines) {
    blocks <- list()
    in_block <- FALSE
    buf <- character()
    for (line in lines) {
      if (!in_block && (grepl("^```\\{r", line) || grepl("^```r", line))) {
        in_block <- TRUE
        buf <- character()
        next
      }
      if (in_block && grepl("^```", line)) {
        in_block <- FALSE
        blocks <- c(blocks, list(paste(buf, collapse = "\n")))
        buf <- character()
        next
      }
      if (in_block) buf <- c(buf, line)
    }
    blocks
  }

  extract_calls <- function(code, fn) {
    calls <- list()
    pattern <- paste0("\\b", fn, "\\s*\\(")
    m <- gregexpr(pattern, code, perl = TRUE)
    if (m[[1]][1] == -1) return(calls)
    starts <- m[[1]]
    for (s in starts) {
      i <- s + attr(m[[1]], "match.length")[1] - 1
      if (substr(code, i, i) != "(") next
      depth <- 0
      in_str <- FALSE
      str_char <- ""
      j <- i
      while (j <= nchar(code)) {
        ch <- substr(code, j, j)
        if (in_str) {
          if (ch == str_char && substr(code, j - 1, j - 1) != "\\") in_str <- FALSE
        } else {
          if (ch == "'" || ch == '"') { in_str <- TRUE; str_char <- ch }
          if (ch == "(") depth <- depth + 1
          if (ch == ")") {
            depth <- depth - 1
            if (depth == 0) break
          }
        }
        j <- j + 1
      }
      if (depth == 0) calls <- c(calls, list(substr(code, s, j)))
    }
    calls
  }

  check_calls <- function(code, fn, valid_args) {
    calls <- extract_calls(code, fn)
    bad <- list()
    for (call in calls) {
      inner <- sub(paste0("^", fn, "\\s*\\("), "", call)
      inner <- sub("\\)$", "", inner)
      names_found <- regmatches(inner, gregexpr("[A-Za-z][A-Za-z0-9_.]*\\s*=", inner))[[1]]
      if (length(names_found) == 0) next
      names_found <- trimws(sub("=", "", names_found, fixed = TRUE))
      invalid <- setdiff(names_found, valid_args)
      if (length(invalid) > 0 && !("..." %in% valid_args)) {
        bad <- c(bad, list(list(call = call, invalid = invalid)))
      }
    }
    bad
  }

  files <- c(
    file.path(root, "README.md"),
    list.files(file.path(root, "inst", "extdata", "handbook"), pattern = "\\.(md|Rmd)$", recursive = TRUE, full.names = TRUE)
  )

  problems <- list()
  for (f in files) {
    if (!file.exists(f)) next
    lines <- readLines(f, warn = FALSE)
    blocks <- extract_code_blocks(lines)
    if (length(blocks) == 0) next
    for (block in blocks) {
      for (fn in names(formals_map)) {
        valid_args <- formals_map[[fn]]
        if (is.null(valid_args)) next
        bad <- check_calls(block, fn, valid_args)
        if (length(bad) > 0) {
          for (b in bad) {
            problems <- c(problems, list(list(file = f, fn = fn, invalid = b$invalid, call = b$call)))
          }
        }
      }
    }
  }

  if (!quiet) {
    if (length(problems) == 0) {
      message("No invalid named arguments found in code blocks.")
    } else {
      message("Invalid named arguments found:")
      for (p in problems) {
        message("- ", p$file, " :: ", p$fn, " invalid: ", paste(p$invalid, collapse = ", "))
      }
    }
  }

  if (length(problems) > 0 && isTRUE(stop_on_error)) {
    stop("Documentation argument check failed; see messages above.")
  }

  invisible(problems)
}

# If run directly via Rscript, execute check.
if (identical(environment(), globalenv()) && !interactive()) {
  formior_check_doc_args(stop_on_error = TRUE)
}
