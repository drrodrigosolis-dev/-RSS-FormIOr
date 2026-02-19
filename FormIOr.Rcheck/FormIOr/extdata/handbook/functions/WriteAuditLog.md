
# WriteAuditLog

**Title:** Write a simple audit log entry

## What this function is for

This function manages audit logging to track dataset changes.

## Overview

This writes a small CSV/TSV log of key actions (e.g., downloads, cleaning,
exports). It is designed to be easy for non-technical users to open in Excel.
If you are using automatic logging, you generally do not need to call this
directly; it is used behind the scenes by other FormIOr functions.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
WriteAuditLog(
  action,
  details = NULL,
  file = "audit_log.csv",
  data = NULL,
  user = NULL,
  append = TRUE,
  quiet = FALSE
)
```

## Key inputs

- `action`: Short action name (e.g., `"export"`). Can be a vector.
- `details`: Optional details or notes. Can be a character vector or list.
- `file`: Path to the audit log file (default: `"audit_log.csv"`). If `NULL`,
uses the active audit log file when one is running.
- `data`: Optional data.frame (or list with `FlatResponses` / `submission_data`)
used to record row/column counts.
- `user`: Optional user name. Defaults to the system user if available.
- `append`: Logical. If `TRUE` (default), append to the existing file.
- `quiet`: Logical. If `FALSE`, prints a short message.

## Outputs

- (See `?WriteAuditLog` for return value details.)

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
df <- data.frame(a = 1:3)
log_path <- tempfile(fileext = ".csv")
WriteAuditLog(
"export",
details = "Exported survey data",
data = df,
file = log_path,
quiet = TRUE
)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [SummaryByField](SummaryByField.md)
- Next: [Data Shapes](../technical/data-shapes.md)
