
# DeduplicateSubmissions

**Title:** Deduplicate submissions by submission ID

## What this function is for

This function focuses on cleaning or standardizing the flattened dataset.

## Overview

Keeps one row per submission ID, using a timestamp column when available
(for example `created` or `modified`), otherwise keeps first/last row.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
DeduplicateSubmissions(
  x,
  id_col = 1,
  time_col = NULL,
  keep = c("last", "first"),
  return_flat = FALSE,
  quiet = FALSE
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `FlattenSubmissions()`.
- `id_col`: Integer or character. Submission ID column (default 1).
- `time_col`: Optional column name to use for ordering. If `NULL`, the
function tries common timestamp names automatically.
- `keep`: One of `"last"` (default) or `"first"`.
- `return_flat`: Logical. If `TRUE` and `x` came from `FlattenSubmissions()`,
include the updated list as `flat` in the output.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- `data`: Deduplicated data frame
- `summary`: List with counts and the time column used
- `flat`: If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
responses <- GetResponses(form_id = "123", api_key = "abc")
flat <- FlattenSubmissions(responses)
dedup <- DeduplicateSubmissions(flat, id_col = "submissionId")
nrow(dedup$data)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [CrossTab](CrossTab.md)
- Next: [Next Function](DescribeForm.md)
