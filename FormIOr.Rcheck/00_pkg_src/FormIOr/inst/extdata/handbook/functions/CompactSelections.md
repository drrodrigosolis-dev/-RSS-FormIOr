
# CompactSelections

**Title:** Compact checkbox/multi-select columns into a single readable column

## What this function is for

This function focuses on cleaning or standardizing the flattened dataset.

## Overview

When a question generates multiple TRUE/FALSE columns (e.g., select boxes),
this function combines them into a single comma-separated column.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
CompactSelections(
  x,
  sep = "-",
  combine_sep = ", ",
  drop = TRUE,
  keep_empty = FALSE,
  yes_values = c(TRUE, "TRUE", "True", "true", "Yes", "YES", "yes", "Y", "y", 1, "1"),
  no_values = c(FALSE, "FALSE", "False", "false", "No", "NO", "no", "N", "n", 0, "0", "",
    " ", "NA"),
  return_flat = FALSE,
  quiet = FALSE
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `FlattenSubmissions()`.
- `sep`: Separator used in column names to split prefix and option.
Default `"-"` (matches `FlattenSubmissions()` naming).
- `combine_sep`: Separator used between selected options.
- `drop`: Logical. If `TRUE`, drop the original checkbox columns.
- `keep_empty`: Logical. If `TRUE`, keep empty strings instead of `NA`
when no options are selected.
- `yes_values`: Values that should count as "selected".
- `no_values`: Values that should count as "not selected" (defaults include
`FALSE`, `"No"`, `0`, and blank strings).
- `return_flat`: Logical. If `TRUE` and `x` came from `FlattenSubmissions()`,
include the updated list as `flat` in the output.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- `data`: Data frame with compacted selection columns
- `summary`: Data frame describing which columns were compacted
- `flat`: If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
compacted <- CompactSelections(flat)
head(compacted$data)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [AskCredentials](AskCredentials.md)
- Next: [Next Function](CompareFormVersions.md)
