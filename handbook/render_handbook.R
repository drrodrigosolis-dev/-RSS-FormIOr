# Render the entire handbook to HTML without pandoc.
# Output is written to: handbook_site/

handbook_dir <- file.path("inst", "extdata", "handbook")
output_dir <- file.path("handbook_site")

if (!requireNamespace("markdown", quietly = TRUE)) {
  stop("Please install the 'markdown' package: install.packages('markdown')")
}

if (!dir.exists(handbook_dir)) {
  stop("Handbook folder not found: ", handbook_dir)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Maintenance helpers (run before rendering)
maintenance_dir <- file.path(handbook_dir, "maintenance")
check_script <- file.path(maintenance_dir, "check_docs.R")
ref_script <- file.path(maintenance_dir, "build_function_reference.R")

if (file.exists(ref_script)) {
  tryCatch({
    source(ref_script)
    if (exists("formior_build_function_reference")) {
      formior_build_function_reference(root = getwd())
    }
  }, error = function(e) message("Function reference build skipped: ", e$message))
}

if (file.exists(check_script)) {
  tryCatch({
    source(check_script)
    if (exists("formior_check_doc_args")) {
      formior_check_doc_args(root = getwd(), stop_on_error = FALSE, quiet = TRUE)
    }
  }, error = function(e) message("Doc checker skipped: ", e$message))
}

clean_function_pages <- function(func_dir) {
  if (!dir.exists(func_dir)) return(invisible(NULL))
  files <- list.files(func_dir, pattern = "\\\\.md$", full.names = TRUE)

  clean_lines <- function(lines) {
    lines <- gsub("emph\\\\{([^}]+)\\\\}", "*\\\\1*", lines)
    lines <- gsub("strong\\\\{([^}]+)\\\\}", "**\\\\1**", lines)
    lines <- gsub("url\\\\{([^}]+)\\\\}", "\\\\1", lines)

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
      if (grepl("^item\\\\s+", trimmed) && length(stack) > 0) {
        item_text <- sub("^item\\\\s+", "", trimmed)
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
}

# Clean any Rd-style artifacts from function pages before rendering
clean_function_pages(file.path(handbook_dir, "functions"))

md_files <- list.files(handbook_dir, pattern = "\\.md$", recursive = TRUE, full.names = TRUE)

for (md in md_files) {
  rel <- sub(paste0("^", handbook_dir, "/"), "", md)
  out <- file.path(output_dir, sub("\\.md$", ".html", rel))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

  # Write temp file alongside the source so relative image paths resolve
  lines <- readLines(md, warn = FALSE)
  tmp <- file.path(dirname(md), ".handbook_tmp_render.md")
  writeLines(lines, tmp)

  markdown::markdownToHTML(tmp, output = out, options = c("use_xhtml"))
  unlink(tmp)

  # Post-process HTML to rewrite .md hrefs -> .html (safe and reliable)
  html <- readLines(out, warn = FALSE)
  html <- gsub('href="([^"]+)\\.md(#[^"]*)?"', 'href="\\1.html\\2"', html)
  html <- gsub("href='([^']+)\\.md(#[^']*)?'", "href='\\1.html\\2'", html)
  writeLines(html, out)
}

# Copy assets so images are available
assets_src <- file.path(handbook_dir, "assets")
assets_dst <- file.path(output_dir, "assets")
if (dir.exists(assets_src)) {
  dir.create(assets_dst, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(assets_src, full.names = TRUE), assets_dst, overwrite = TRUE)
}

message("Handbook rendered to: ", normalizePath(output_dir))
message("Open: ", file.path(output_dir, "index.html"))
