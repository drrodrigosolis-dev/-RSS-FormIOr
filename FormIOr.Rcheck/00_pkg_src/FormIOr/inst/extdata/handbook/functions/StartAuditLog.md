
# StartAuditLog

**Title:** Start an audit log for FormIOr actions

## What this function is for

This function manages audit logging to track dataset changes.

## Overview

Creates a new audit log file and turns on automatic logging for
FormIOr functions. Each subsequent action (downloads, cleaning,
exports, etc.) will append a new row to the log.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
StartAuditLog(file = "audit_log.csv", overwrite = FALSE, quiet = FALSE)
```

## Key inputs

- `file`: Path to the audit log file (default: `"audit_log.csv"`).
- `overwrite`: Logical. If `FALSE` (default), stop if the file exists.
- `quiet`: Logical. If `FALSE`, prints a short message.

## Outputs

- (See `?StartAuditLog` for return value details.)

## Details and behavior

The audit log is designed to be "Excel friendly": it is a simple CSV/TSV
with one row per action, including a timestamp, an action name, optional
details, and (when available) row/column counts for the dataset being
processed.

If you do *not* start a log, many FormIOr functions will (once per R session)
ask whether you want to begin logging. Starting a log here avoids that prompt
and gives you control over the file location.

## Examples

```r
log_file <- tempfile(fileext = ".csv")
StartAuditLog(log_file, overwrite = TRUE)

# Run some FormIOr steps (each will append a row when possible)
flat <- FlattenSubmissions(FoodTypes)
norm <- NormalizeColumnNames(flat, quiet = TRUE)

StopAuditLog()
read.csv(log_file, stringsAsFactors = FALSE)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [ResponseTimeline](ResponseTimeline.md)
- Next: [Next Function](StopAuditLog.md)
