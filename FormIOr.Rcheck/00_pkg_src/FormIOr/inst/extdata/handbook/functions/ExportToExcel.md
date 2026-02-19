
# ExportToExcel

**Title:** Export data to Excel (or CSV if needed)

## What this function is for

This function produces human-readable outputs for reporting.

## Overview

This is a simple, user-friendly export helper for FormIOr outputs.
It accepts a data.frame, a list of data.frames (multiple sheets),
or a list returned by `GetResponses()` / `FlattenSubmissions()`.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
ExportToExcel(
  data,
  path = export_path,
  sheet = "Data",
  overwrite = FALSE,
  include_row_names = FALSE,
  quiet = FALSE
)
```

## Key inputs

- `data`: A data.frame, list of data.frames, or list containing
`FlatResponses` or `submission_data`.
- `path`: File path for the Excel output (required; use `tempfile(fileext = ".xlsx")` in examples).
If you use a `.csv` or `.tsv` extension, delimited files will be written.
- `sheet`: Sheet name to use when exporting a single data.frame.
- `overwrite`: Logical. If `FALSE` (default), stop if the file exists.
- `include_row_names`: Logical. Include row names in the export.
- `quiet`: Logical. If `FALSE`, prints a short message.

## Outputs

- `path`: Output file path(s).
- `format`: `"xlsx"`, `"csv"`, or `"tsv"`.
- `sheets`: Sheet names used.
- `rows`: Row counts for each sheet.
- `cols`: Column counts for each sheet.

## Details and behavior

If an Excel writer is available (`writexl` or `openxlsx`), it writes `.xlsx`.
If not, it falls back to CSV files and prints a clear message.

## Examples

```r
df <- data.frame(name = c("A", "B"), score = c(1, 2))

export_path <- tempfile(fileext = ".xlsx")
ExportToExcel(df, export_path, overwrite = TRUE)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [DescribeForm](DescribeForm.md)
- Next: [Next Function](FieldDictionary.md)
