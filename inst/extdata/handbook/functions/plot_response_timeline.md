
# plot_response_timeline

**Title:** Plot a response timeline

## What this function is for

This function focuses on diagnostics and reporting.

## Overview

Convenience wrapper around `summarize_response_timeline()` that draws a simple line
chart using base graphics.
If audit logging is active (see `start_audit_log()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
plot_response_timeline(
  x,
  date_col = NULL,
  interval = c("day", "week", "month", "hour"),
  tz = "UTC",
  start = NULL,
  end = NULL,
  include_empty = TRUE,
  main = NULL,
  xlab = NULL,
  ylab = "Responses",
  col = "steelblue",
  lwd = 2,
  type = "l",
  plot = TRUE,
  ...
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `flatten_submission_records()`.
- `date_col`: Column name or number for the date/time field. If `NULL`,
tries to guess a reasonable timestamp column.
- `interval`: One of `"day"` (default), `"week"`, `"month"`, or `"hour"`.
- `tz`: Time zone to use for parsing dates (default `"UTC"`).
- `start`: Optional start date/time to filter the range.
- `end`: Optional end date/time to filter the range.
- `include_empty`: Logical. If `TRUE`, fill missing periods with zeroes.
- `main`: Optional plot title.
- `xlab`: Optional x-axis label.
- `ylab`: Optional y-axis label.
- `col`: Line color.
- `lwd`: Line width.
- `type`: Plot type passed to `plot()`. Default "l".
- `plot`: Logical. If `FALSE`, return timeline data without plotting.
- `...`: Additional arguments passed to `plot()`.

## Outputs

- (See `?plot_response_timeline` for return value details.)

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
plot_response_timeline(flat, date_col = "created", interval = "month")
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [plot_numeric_distribution](plot_numeric_distribution.md)
- Next: [Next Function](plot_word_cloud.md)
