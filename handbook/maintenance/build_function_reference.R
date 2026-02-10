# Build a quick reference page from Rd files
# Usage: source("inst/extdata/handbook/maintenance/build_function_reference.R")
# Then call: formior_build_function_reference()

formior_build_function_reference <- function(root = getwd()) {
  man_dir <- file.path(root, "man")
  out_dir <- file.path(root, "inst", "extdata", "handbook", "functions")
  out_path <- file.path(out_dir, "reference.md")

  ns_lines <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  exports <- sub("^export\\(([^)]+)\\)$", "\\1", ns_lines[grepl("^export\\(", ns_lines)])
  exports <- unique(exports)
  exports <- exports[order(tolower(exports))]

  read_braced <- function(text, start) {
    depth <- 1
    i <- start
    out <- character()
    while (i <= nchar(text)) {
      ch <- substr(text, i, i)
      if (ch == "{") depth <- depth + 1
      if (ch == "}") {
        depth <- depth - 1
        if (depth == 0) break
      }
      out <- c(out, ch)
      i <- i + 1
    }
    list(value = paste(out, collapse = ""), end = i + 1)
  }

  extract_section <- function(tag, text) {
    # Rd uses a single backslash (e.g., \title{...})
    pattern <- paste0("\\", tag, "{")
    idx <- regexpr(pattern, text, fixed = TRUE)[1]
    if (idx == -1) return("")
    start <- idx + nchar(pattern)
    res <- read_braced(text, start)
    res$value
  }

  clean_text <- function(x) {
    x <- gsub("\\\\code\\{([^}]+)\\}", "\\1", x)
    x <- gsub("\\\\verb\\{([^}]+)\\}", "\\1", x)
    x <- gsub("\\\\link\\[[^]]*\\]\\{([^}]*)\\}", "\\1", x)
    x <- gsub("\\\\link\\{([^}]+)\\}", "\\1", x)
    x <- gsub("\\\\item", "- ", x)
    # Fallback: strip any remaining Rd commands while keeping inner text
    x <- gsub("\\\\[A-Za-z]+\\{([^}]+)\\}", "\\1", x)
    x <- gsub("\\n", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }

  rows <- list()
  for (fn in exports) {
    rd <- file.path(man_dir, paste0(fn, ".Rd"))
    if (!file.exists(rd)) next
    txt <- paste(readLines(rd, warn = FALSE), collapse = "\n")
    title <- clean_text(extract_section("title", txt))
    desc <- clean_text(extract_section("description", txt))
    summary <- if (nzchar(desc)) desc else title
    # Keep it short
    if (nchar(summary) > 200) summary <- paste0(substr(summary, 1, 197), "...")
    rows[[length(rows) + 1]] <- c(fn, summary)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  lines <- c(
    "# Function Reference (Quick Lookup)",
    "",
    "This page lists every exported FormIOr function with a short description. ",
    "Click a function name to open its full handbook page.",
    "",
    "| Function | Summary |",
    "| --- | --- |"
  )

  for (row in rows) {
    fn <- row[1]
    summary <- row[2]
    lines <- c(lines, sprintf("| [%s](%s.md) | %s |", fn, fn, summary))
  }

  lines <- c(lines, "", "---", "", "Navigation", "- Home: [Handbook Index](../index.md)", "- Function Index: [All Functions](index.md)")

  writeLines(lines, out_path)
  out_path
}

# If run directly via Rscript, build the reference page.
if (identical(environment(), globalenv()) && !interactive()) {
  formior_build_function_reference()
}
