# Clean up function handbook pages to remove Rd-style markup artifacts.
# Converts itemize/enumerate blocks to Markdown lists.

func_dir <- file.path("inst", "extdata", "handbook", "functions")
files <- list.files(func_dir, pattern = "\\.md$", full.names = TRUE)

clean_lines <- function(lines) {
  # Convert inline Rd-style markup that may have leaked into Markdown
  lines <- gsub("emph\\{([^}]+)\\}", "*\\1*", lines)
  lines <- gsub("strong\\{([^}]+)\\}", "**\\1**", lines)
  lines <- gsub("url\\{([^}]+)\\}", "\\1", lines)

  out <- character()
  stack <- character()

  for (line in lines) {
    trimmed <- trimws(line)
    if (trimmed == "itemize{") {
      stack <- c(stack, "itemize")
      next
    }
    if (trimmed == "enumerate{") {
      stack <- c(stack, "enumerate")
      next
    }
    if (trimmed == "}") {
      if (length(stack) > 0) stack <- stack[-length(stack)]
      next
    }
    if (grepl("^item\\s+", trimmed) && length(stack) > 0) {
      item_text <- sub("^item\\s+", "", trimmed)
      prefix <- if (tail(stack, 1) == "enumerate") "1. " else "- "
      out <- c(out, paste0(prefix, item_text))
      next
    }
    out <- c(out, line)
  }

  out
}

for (f in files) {
  lines <- readLines(f, warn = FALSE)
  cleaned <- clean_lines(lines)
  if (!identical(lines, cleaned)) {
    writeLines(cleaned, f)
  }
}

message("Cleaned function pages: ", length(files))
