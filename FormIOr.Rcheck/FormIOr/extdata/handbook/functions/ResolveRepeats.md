
# ResolveRepeats

**Title:** Resolve repeated answers into one row per submission

## What this function is for

This function focuses on cleaning or standardizing the flattened dataset.

## Overview

Automatically collapses repeated values within each submission ID using a
consistent strategy (or simple heuristics when `strategy = "auto"`).
This is a non-interactive alternative to `FixDups()`.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
ResolveRepeats(
  x,
  id_col = 1,
  strategy = c("auto", "concat", "first", "last", "sum", "mean", "count", "count_yes"),
  sep = ", ",
  unique = TRUE,
  return_flat = FALSE,
  quiet = FALSE
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `FlattenSubmissions()`.
- `id_col`: Integer or character. Submission ID column (default 1).
- `strategy`: One of `"auto"`, `"concat"`, `"first"`, `"last"`, `"sum"`,
`"mean"`, `"count"`, or `"count_yes"`.
- `sep`: Separator used for concatenation (default `", "`).
- `unique`: Logical. If `TRUE`, remove duplicate values before concatenating.
- `return_flat`: Logical. If `TRUE` and `x` came from `FlattenSubmissions()`,
include the updated list as `flat` in the output.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- `data`: Collapsed data frame with one row per submission
- `summary`: Data frame describing how each column was handled
- `flat`: If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list

## Details and behavior

Note: some FormIO components (for example uploads or address blocks) can
produce list-columns or nested data-frame columns even after flattening.
These values are converted to readable JSON/text before collapsing so the
output remains stable and export-friendly.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## Examples

```r
responses <- GetResponses(form_id = "123", api_key = "abc")
flat <- FlattenSubmissions(responses)
resolved <- ResolveRepeats(flat, id_col = "submissionId")
head(resolved$data)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [ReviewDuplicateSubmissions](ReviewDuplicateSubmissions.md)
- Next: [Next Function](ResponseTimeline.md)
