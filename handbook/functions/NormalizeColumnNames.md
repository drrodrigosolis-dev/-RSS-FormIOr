
# NormalizeColumnNames

**Title:** Normalize column names into a clean, readable format

## What this function is for

This function focuses on cleaning or standardizing the flattened dataset.

## Overview

Designed for non-technical users who want consistent column names after
downloading FormIO submissions. Works with either a raw data frame or the
list returned by `FlattenSubmissions()`.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
NormalizeColumnNames(
  x,
  style = c("snake", "lower", "upper", "title"),
  make_unique = TRUE,
  transliterate = TRUE,
  return_flat = FALSE,
  quiet = FALSE
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `FlattenSubmissions()`.
- `style`: One of `"snake"` (default), `"lower"`, `"upper"`, or `"title"`.
Controls casing and separator behavior.
- `make_unique`: Logical. If `TRUE`, make duplicated names unique.
- `transliterate`: Logical. If `TRUE`, convert accents/special characters
to ASCII when possible.
- `return_flat`: Logical. If `TRUE` and `x` came from `FlattenSubmissions()`,
include the updated list as `flat` in the output.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- `data`: Data frame with normalized column names
- `name_map`: Data frame with old and new column names
- `flat`: If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
responses <- GetResponses(form_id = "123", api_key = "abc")
flat <- FlattenSubmissions(responses)
norm <- NormalizeColumnNames(flat)
names(norm$data)
norm$name_map
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [MakeCodebook](MakeCodebook.md)
- Next: [Next Function](PlotBarSummary.md)
