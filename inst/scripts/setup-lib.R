script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) NA_character_
)

project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
if (!is.na(script_path) && nzchar(script_path)) {
  candidate <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(candidate, "DESCRIPTION"))) {
    project_root <- candidate
  }
}

lib_path <- file.path(project_root, ".Rlib")
if (!dir.exists(lib_path)) dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)

.libPaths(c(lib_path, .libPaths()))
message("Using project library: ", lib_path)
