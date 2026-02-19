
# ResponseTimeline

**Title:** Summarize responses over time

## What this function is for

This function focuses on diagnostics and reporting.

## Overview

Counts responses per day/week/month (or hour), using a timestamp column.
If `date_col` is `NULL`, the function tries common timestamp names
automatically (e.g., `created`, `modified`, verb{_createdAt}).
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
ResponseTimeline(
  x,
  date_col = NULL,
  interval = c("day", "week", "month", "hour"),
  tz = "UTC",
  start = NULL,
  end = NULL,
  include_empty = TRUE,
  quiet = FALSE
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `FlattenSubmissions()`.
- `date_col`: Column name or number for the date/time field. If `NULL`,
tries to guess a reasonable timestamp column.
- `interval`: One of `"day"` (default), `"week"`, `"month"`, or `"hour"`.
- `tz`: Time zone to use for parsing dates (default `"UTC"`).
- `start`: Optional start date/time to filter the range.
- `end`: Optional end date/time to filter the range.
- `include_empty`: Logical. If `TRUE`, fill missing periods with zeroes.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- `data`: Data frame with `period` and `count` columns
- `summary`: List with metadata about the calculation

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
ResponseTimeline(flat, date_col = "created", interval = "week")
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [ResolveRepeats](ResolveRepeats.md)
- Next: [Next Function](StartAuditLog.md)
