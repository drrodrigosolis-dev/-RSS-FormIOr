
# PlotBarSummary

**Title:** Plot a bar chart for a categorical field

## What this function is for

This function focuses on diagnostics and reporting.

## Overview

If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
PlotBarSummary(
  x,
  field,
  top_n = 10,
  include_na = FALSE,
  horiz = FALSE,
  main = NULL,
  xlab = NULL,
  col = "steelblue",
  plot = TRUE,
  ...
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `FlattenSubmissions()`.
- `field`: Column name or number to plot.
- `top_n`: Number of categories to show (default 10). Use `NULL` for all.
- `include_na`: Logical. If `TRUE`, include missing values as "(Missing)".
- `horiz`: Logical. If `TRUE`, draw horizontal bars.
- `main`: Optional plot title.
- `xlab`: Optional x-axis label.
- `col`: Bar fill color.
- `plot`: Logical. If `FALSE`, return bar data without plotting.
- `...`: Additional arguments passed to `barplot()`.

## Outputs

- (See `?PlotBarSummary` for return value details.)

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
PlotBarSummary(flat, "region")
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [NormalizeColumnNames](NormalizeColumnNames.md)
- Next: [Next Function](PlotHistogram.md)
