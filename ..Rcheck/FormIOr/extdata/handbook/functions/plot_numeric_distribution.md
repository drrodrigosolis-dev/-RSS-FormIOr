
# plot_numeric_distribution

**Title:** Plot a histogram for a numeric field

## What this function is for

This function focuses on diagnostics and reporting.

## Overview

If audit logging is active (see `start_audit_log()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
plot_numeric_distribution(
  x,
  field,
  bins = "Sturges",
  include_na = FALSE,
  main = NULL,
  xlab = NULL,
  col = "steelblue",
  border = "white",
  plot = TRUE,
  ...
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `flatten_submission_records()`.
- `field`: Column name(s) or number(s) to plot. When multiple columns are
provided, their text is combined into one wordcloud.
- `bins`: Histogram breaks passed to `hist()`. Default "Sturges".
- `include_na`: Logical. If `TRUE`, keep `NA` values (ignored by `hist`).
- `main`: Optional plot title.
- `xlab`: Optional x-axis label.
- `col`: Bar fill color.
- `border`: Bar border color.
- `plot`: Logical. If `FALSE`, return histogram data without plotting.
- `...`: Additional arguments passed to `hist()`.

## Outputs

- (See `?plot_numeric_distribution` for return value details.)

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
plot_numeric_distribution(flat, "age")
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [plot_categorical_summary](plot_categorical_summary.md)
- Next: [Next Function](plot_response_timeline.md)
